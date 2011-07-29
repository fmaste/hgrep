{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances  #-}
module Main (
	main,
	GrepM,
	Position,
	getPosition,
	setPosition,
	modifyPosition,
	modifyPositionM
) where

-------------------------------------------------------------------------------

import Data.Maybe
import Data.Either
import Data.List (foldr, foldl')
import Data.Monoid
import Control.Monad
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Error (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.OldException
import Control.Parallel
import Control.Concurrent
-- "sudo ghc-pkg expose transformers" was needed.
-- See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=626985
-- And: http://stackoverflow.com/questions/5252066/why-is-package-hidden-by-default-and-how-can-i-unhide-it
-- Also:
-- sudo cabal upgrade transformers
-- sudo cabal upgrade mtl
-- For profiling: cabal install transformers mtl parallel --enable-library-profiling --reinstall
import System (getArgs)
import System.IO (Handle, stdin, stdout, stderr, hPutStrLn, hFlush)
import System.Directory (doesDirectoryExist, doesFileExist, getPermissions, Permissions(..), getDirectoryContents)
import qualified Data.ByteString.Lazy.Char8 as BS

-------------------------------------------------------------------------------

type GrepError = String

-- The Grep Monad has:
-- A Reader that allows to have as environment the actual position in a file.
-- A Writer to log messages.
-- A state with the parsing state machine.
-- And finally, allows to handle errors.
-- All this inside the list monad to allow to generate multiple states from a parsing.
newtype GrepM m a = GrepM {runGrepM :: Position -> GrepState -> m (Either GrepError a, Position, GrepState)}

instance Monad m => Monad (GrepM m) where
	return a = GrepM $ \p s -> return (Right a, p, s)
	m >>= f = bind m f
	fail str = GrepM $ \p s -> return (Left str, p, s)

bind :: Monad m => GrepM m a -> (a -> GrepM m b) -> GrepM m b
bind m f = GrepM $ \p s -> do
	(err, p', s') <- runGrepM m p s
	case err of
		Right a -> runGrepM (f a) p' s'
		Left e' -> return (Left e', p', s')

-- Position as a state monad

getPosition :: Monad m => GrepM m Position
getPosition = GrepM $ \p s -> return (Right p, p, s)

setPosition :: Monad m => Position -> GrepM m ()
-- Strictness needed otherwise it is never evaluated until a match is found!
setPosition p = GrepM $ \_ s -> seq p $ return (Right (), p, s)

modifyPosition :: MonadIO m => (Position -> Position) -> GrepM m ()
modifyPosition f = do
	p <- getPosition
	setPosition (f p)

modifyPositionM :: Monad m => (Position -> GrepM m Position) -> GrepM m ()
modifyPositionM f = do
	p <- getPosition
	p' <- f p
	setPosition p'

-- State monad with GrepState

getState :: Monad m => GrepM m GrepState
getState = GrepM $ \p s -> return (Right s, p, s)

setState :: Monad m => GrepState -> GrepM m ()
setState s = GrepM $ \p _ -> return (Right (), p, s)

modifyState :: MonadIO m => (GrepState -> GrepState) -> GrepM m ()
modifyState f = do
	s <- getState
	setState (f s)

modifyStateM :: Monad m => (GrepState -> GrepM m GrepState) -> GrepM m ()
modifyStateM f = do
	s <- getState
	s' <- f s
	setState s'

instance MonadTrans GrepM where
	lift m = GrepM $ \p s -> do
		a <- m
		return (Right a, p, s)

instance Monad m => MonadError GrepError (GrepM m) where
	throwError e = GrepM $ \p s -> return (Left e, p, s)
	catchError m h = GrepM $ \p s -> do
		(err, p', s') <- runGrepM m p s
		case err of
			Right a -> return (Right a, p', s')
			Left e -> runGrepM (h e) p' s'

instance MonadIO m => MonadIO (GrepM m) where
	liftIO = lift . liftIO

-------------------------------------------------------------------------------

type LineNumber = Int

type ColumnNumber = Int

data Position = Position {name :: String, lineNumber :: !LineNumber, columnNumber :: !ColumnNumber}
	deriving Show

initialPosition name = Position name 1 1

getName (Position n _ _) = n

getLineNumber (Position _ ln _) = ln

incrementLine (Position n ln _) = Position n (ln + 1) 1

getColumnNumber (Position _ _ cl) = cl

incrementColumn (Position n ln cl) = Position n ln (cl + 1)

-------------------------------------------------------------------------------

data Action = 
	Start
	| NewLine 
	| AddChar Char
	| End
	deriving Show

-- The pattern, the pattern length and the array of (Position, Eq counts)
data GrepState = GrepState String Int [(Position, Int)]
	deriving Show

stateStep :: MonadIO m => Action -> GrepState -> GrepM m GrepState
stateStep Start state = return state
stateStep NewLine state = do
	{--
		position <- getPosition
		return $ resetState position state
	--}
	return state
stateStep (AddChar char) state = do
	position <- getPosition
	let (newState, maybePos) = addChar position char state
	case maybePos of
		Just pos -> liftIO $ putStrLn ("Found in: " ++ (show pos))
		Nothing -> return ()
	return newState
stateStep End state = return state

-- Create an initial array with (File "", 0)
initialState :: String -> GrepState
initialState pattern = let
	len = length pattern
	counts = replicate len (Position "" 0 0, 0)
	in GrepState pattern len counts

resetState :: Position -> GrepState -> GrepState
resetState pos (GrepState pattern len _) = GrepState pattern len (replicate len (pos, 0))

addChar :: Position -> Char -> GrepState -> (GrepState, Maybe Position)
addChar addedPos addedChar (GrepState pattern len counts) = let 
	outCounts = map f $ zip pattern ((addedPos, 0):counts) where
		f (actualChar, (actualPos, actualEqs)) = (actualPos, if actualChar == addedChar then (actualEqs + 1) else actualEqs)
	(lastPos, lastEqs) = last outCounts
	maybePos = if lastEqs == len then (Just lastPos) else Nothing
	in (GrepState pattern len outCounts, maybePos)

-------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	-- TODO: when (length args < 1) $ throw "No string search pattern"
	let state = initialState $ head args
	if length args >= 2 
		-- Take the first argument as the path if there is one.
		then processPaths (tail args) state
		-- If no argument process stdin.
		else processHandle stdin (initialPosition "stdin") state
	hFlush stderr
	hFlush stdout

processPaths :: [FilePath] -> GrepState -> IO ()
processPaths filePaths state = do
	mVars <- forM filePaths (\filePath -> do
		mVar <- newEmptyMVar
		forkIO $ processPath filePath state mVar
		return mVar)
	mapM_ takeMVar mVars

processPath :: FilePath -> GrepState -> MVar () -> IO ()
processPath path state mVar = do
	isDir <- doesDirectoryExist path
	isFile <- doesFileExist path
	if not $ isDir || isFile
		then hPutStrLn stderr $ "Path \"" ++ path ++ "\" does not exists"
		else do
			perms <- getPermissions path
			if not $ readable perms
				then hPutStrLn stderr $ "Path \"" ++ path ++ "\" has no read permission"
				else if isDir 
					then processDirPath path state
					else processFilePath path state
	putMVar mVar ()

processDirPath :: FilePath -> GrepState -> IO ()
processDirPath dirPath state = do
	eitherPaths <- try $ getDirectoryContents dirPath
	case eitherPaths of
		Left e -> do
			hPutStrLn stderr $ "Skipping directory \"" ++ dirPath ++ "\": " ++ (show e)
		Right paths -> do
			let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
			processPaths filteredPaths state

processFilePath :: FilePath -> GrepState -> IO ()
processFilePath filePath state = do
	eitherContent <- try $ BS.readFile filePath
	case eitherContent of
		Left e -> do
			 hPutStrLn stderr $ "Skipping file \"" ++ filePath ++ "\": " ++ (show e)
		Right content -> do
			processContent content (initialPosition filePath) state

processHandle :: Handle -> Position -> GrepState -> IO ()
processHandle handle position state = do
	eitherContent <- try $ BS.hGetContents handle
	case eitherContent of
		Left e -> do
			hPutStrLn stderr $ "Error reading from handle:" ++ (show e)
		Right content -> do
			processContent content position state

-------------------------------------------------------------------------------

processContent :: BS.ByteString -> Position -> GrepState -> IO ()
processContent content position state = do 
	(eitherAns, p, state) <- runGrepM (readLines content) position state
	case eitherAns of
		Left e -> hPutStrLn stderr e
		Right a -> return ()
	-- TODO: Create a log a Writer monad
	-- mapM_ putStrLn $ log

readLines :: MonadIO m => BS.ByteString -> GrepM m ()
readLines content = mapM_ readLine $ BS.lines content

readLine :: MonadIO m => BS.ByteString -> GrepM m ()
readLine line = do
	modifyStateM (stateStep NewLine)
	readColumns line
	modifyPosition incrementLine

readColumns :: MonadIO m => BS.ByteString -> GrepM m ()
readColumns columns
	| BS.null columns = return ()
	| otherwise = do
		readColumn $ BS.head columns
		modifyPosition incrementColumn
		readColumns $ BS.tail columns

readColumn :: MonadIO m => Char -> GrepM m ()
readColumn column = modifyStateM (stateStep (AddChar column))

{-- 
la
lala
lalala
lalalala
--}

