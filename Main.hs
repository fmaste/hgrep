module Main (
	main
) where

import Control.Monad
import Control.Monad.List
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.OldException
-- "sudo ghc-pkg expose transformers" was needed.
-- See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=626985
-- And: http://stackoverflow.com/questions/5252066/why-is-package-hidden-by-default-and-how-can-i-unhide-it
-- Also:
-- sudo cabal upgrade transformers
-- sudo cabal upgrade mtl
-- For profiling: cabal install transformers mtl --enable-library-profiling --reinstall
import Data.List
import Data.Maybe
import Data.Either
import System
import System.IO
import System.Directory

-------------------------------------------------------------------------------

type Log = [String]

type GrepError = String

-- The Grep Monad has:
-- A Reader that allows to have as environment the actual position in a file.
-- A Writer to log messages.
-- A state with the parsing state machine.
-- And finally, allows to handle errors.
-- All this inside the list monad to allow to generate multiple states from a parsing.
type GrepM m a = ReaderT Position (ErrorT GrepError (WriterT Log (StateT GrepState (ListT m)))) a

runGrepM :: Monad m => GrepM m a -> Position -> GrepState -> m [(Either GrepError a, Log, GrepState)]
runGrepM gm pos state = do
	ansList <- runListT (runStateT (runWriterT (runErrorT (runReaderT gm pos))) state)
	return $ map (\((eitherAns, log), state) -> (eitherAns, log, state)) ansList

-------------------------------------------------------------------------------

type LineNumber = Integer

type ColumnNumber = Integer

data Position = 
	-- A file to parse.
	File {fileName :: FilePath, lineNumber :: LineNumber, columnNumber :: ColumnNumber}
	-- Parsing stdin.
	| Stdin {lineNumber :: LineNumber, columnNumber :: ColumnNumber}
	deriving Show

initialStdinPosition = Stdin 1 1

initialFilePosition path = File path 1 1

getFileName (Stdin _ _) = "Standard input"
getFileName (File fp _ _) = fp

getLineNumber (Stdin ln _) = ln
getLineNumber (File _ ln _) = ln

incrementLine (Stdin ln cl) = Stdin (ln + 1) cl
incrementLine (File fp ln cl) = File fp (ln + 1) cl

getColumnNumber (Stdin _ cl) = cl
getColumnNumber (File _ _ cl) = cl

incrementColumn (Stdin ln cl) = Stdin ln (cl + 1)
incrementColumn (File fp ln cl) = File fp ln (cl + 1)

-------------------------------------------------------------------------------

data Action = 
	Start
	| NewLine 
	| AddChar Char
	| End
	deriving Show

-- The pattern, the pattern length and the array of (Position, Eq counts)
data GrepState = GrepState String Integer [(Position, Integer)]
	deriving Show

stateStep :: MonadIO m => Action -> GrepState -> GrepM m GrepState
stateStep Start state = return state
stateStep NewLine state = do
	position <- ask
	return $ resetState position state
	-- return $ lift $ ListT [resetState position state, state]
stateStep (AddChar char) state = do
	position <- ask
	let newState = addChar position char state
	case (getLastMatch newState) of
		Just pos -> do
			tell ["Found in: " ++ (show pos)]
			return newState
		Nothing -> return newState
stateStep End state = return state

-- Create an initial array with (File "", 0)
initialState :: String -> GrepState
initialState pattern = let
	lenInt = length pattern
	counts = replicate lenInt (File "" 0 0, 0)
	in GrepState pattern (toInteger lenInt) counts

resetState :: Position -> GrepState -> GrepState
resetState pos (GrepState pattern len _) = GrepState pattern len (replicate (fromInteger len) (pos, 0))

addChar :: Position -> Char -> GrepState -> GrepState
addChar addedPos addedChar (GrepState pattern len counts) = let 
	outCounts = map f $ zip pattern ((addedPos, 0):counts) where
		f (actualChar, (actualPos, actualEqs)) = 
			let actualEqs' = if actualChar == addedChar then (actualEqs + 1) else actualEqs
			in (actualPos, actualEqs')
	in (GrepState pattern len outCounts)

getLastMatch :: GrepState -> Maybe Position
getLastMatch (GrepState _ len counts) = let
	(lastPos, lastEqs) = last counts
	in if lastEqs == len then (Just lastPos) else Nothing

-------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	-- TODO: when (length args < 1) $ throw "No string search pattern"
	let state = initialState $ head args
	if length args >= 2 
		-- Take the first argument as the path if there is one.
		then processPath (args !! 1) state
		-- If no argument process stdin.
		else processHandle stdin initialStdinPosition state

processPath :: FilePath -> GrepState -> IO ()
processPath path state = do
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

processDirPath :: FilePath -> GrepState -> IO ()
processDirPath dirPath state = do
	-- TODO: Check errors!
	paths <- getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> processPath p state) filteredPaths
	return ()

processFilePath :: FilePath -> GrepState -> IO ()
processFilePath filePath state = do
	eitherHandle <- try $ openFile filePath ReadMode
	either whenLeft whenRight eitherHandle where
		whenLeft e = do
			hPutStrLn stderr $ "Unable to open file \"" ++ (show filePath) ++ "\": " ++ (show e)
		whenRight handle = do
			-- It may only throw an error if handle was already used.
			hSetBuffering handle $ BlockBuffering (Just 2048)
			-- May need to flush the handle, we are not checking for errors here.
			hSetEncoding handle utf8
			processHandle handle (initialFilePosition filePath) state
			-- TODO: At least log the closing error!
			try $ hClose handle
			return ()

processHandle :: Handle -> Position -> GrepState -> IO ()
processHandle handle position state = do
	((eitherAns, log, state):xs) <- runGrepM (readLines handle) position state
	case eitherAns of
		Left e -> hPutStrLn stderr e
		Right a -> return ()
	mapM_ putStrLn log

readLines :: MonadIO m => Handle -> GrepM m ()
readLines handle = do 
	-- Not checking errors here, if hIsEOF fails readLine should have failed before.
	isEOF <- liftIO $ hIsEOF handle
	unless isEOF $ readLine handle >> local incrementLine (readLines handle)

readLine :: MonadIO m => Handle -> GrepM m ()
readLine handle = do
	eitherLineStr <- liftIO $ try (hGetLine handle)
	case eitherLineStr of
		Left e -> do
			position <- ask
			let fileName = getFileName position
			let lineNumber = getLineNumber position
			throwError $ 
				"Skipping file \"" ++ fileName ++ 
				"\", error reading line number " ++ (show lineNumber) ++ ": " ++ (show e)
		Right lineStr -> do
			modifyState NewLine
			readColumns lineStr

readColumns :: MonadIO m => String -> GrepM m ()
readColumns [] = return ()
readColumns (x:xs) = readColumn x >> local incrementColumn (readColumns xs)

readColumn :: MonadIO m => Char -> GrepM m ()
readColumn columnChar = do
	modifyState (AddChar columnChar)

modifyState :: MonadIO m => Action -> GrepM m ()
modifyState action = do
	state <- get
	newState <- stateStep action state
	put newState

{-- 
la
lala
lalala
lalalala
--}

