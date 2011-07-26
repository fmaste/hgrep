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
import Data.List
import Data.Maybe
import Data.Either
import System
import System.IO
import System.Directory

-------------------------------------------------------------------------------

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

incrementLineNumber (Stdin ln cl) = Stdin (ln + 1) cl
incrementLineNumber (File fp ln cl) = File fp (ln + 1) cl

getColumnNumber (Stdin _ cl) = cl
getColumnNumber (File _ _ cl) = cl

incrementColumnNumber (Stdin ln cl) = Stdin ln (cl + 1)
incrementColumnNumber (File fp ln cl) = File fp ln (cl + 1)

-------------------------------------------------------------------------------

type Log = [String]

-------------------------------------------------------------------------------

data Action = 
	Start
	| NewLine 
	| AddChar Char
	| End
	deriving Show

-- The pattern, the pattern length and the array of (Position, Eq counts)
data GrepState = GrepState String Integer [(Position, Integer)] (Maybe Position)
	deriving Show

stateStep :: MonadIO m => Action -> GrepState -> GrepM m GrepState
stateStep Start state = return state
stateStep NewLine state = do
	position <- ask
	return $ resetState position state
stateStep (AddChar char) state = do
	position <- ask
	let newState = addChar position char state
	case (getLastMatchedPosition newState) of
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
	in GrepState pattern (toInteger lenInt) counts Nothing

resetState :: Position -> GrepState -> GrepState
resetState pos (GrepState pattern len _ _) = GrepState pattern len (replicate (fromInteger len) (pos, 0)) Nothing

addChar :: Position -> Char -> GrepState -> GrepState
addChar addedPos addedChar (GrepState pattern len counts _) = let 
	outCounts = map f $ zip pattern ((addedPos, 0):counts) where
		f (actualChar, (actualPos, actualEqs)) = 
			let actualEqs' = if actualChar == addedChar then (actualEqs + 1) else actualEqs
			in (actualPos, actualEqs')
	(lastPos, lastEqs) = last outCounts
	maybePos = if lastEqs == len then (Just lastPos) else Nothing
	in (GrepState pattern len (init outCounts) maybePos)

getLastMatchedPosition :: GrepState -> Maybe Position
getLastMatchedPosition (GrepState _ _ _ maybePos) = maybePos

-------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		-- Take the first argument as the path if there is one.
		then processPath (head args)
		-- If no argument process stdin.
		else processHandle stdin initialStdinPosition (initialState "lala")

processPath :: FilePath -> IO ()
processPath path = do
	putStrLn $ "Processing path: " ++ path
	isDir <- doesDirectoryExist path
	isFile <- doesFileExist path
	if not $ isDir || isFile
		then do putStrLn $ path ++ " does not exists"
		else do
			perms <- getPermissions path
			if not $ readable perms
				then putStrLn $ path ++ " has no read permission"
				else if isDir 
					then processDirPath path
					else processFilePath path

processDirPath :: FilePath -> IO ()
processDirPath dirPath = do
	putStrLn $ "Processing dir path: " ++ dirPath
	-- TODO: Check errors!
	paths <- getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> processPath p) filteredPaths
	return ()

processFilePath :: FilePath -> IO ()
processFilePath filePath = do
	eitherHandle <- try $ openFile filePath ReadMode
	either whenLeft whenRight eitherHandle where
		whenLeft e = do
			putStrLn $ "Unable to open file " ++ (show filePath) ++ ": " ++ (show e)
		whenRight handle = do
			-- It may only throw an error if handle was already used.
			hSetBuffering handle $ BlockBuffering (Just 2048)
			-- May need to flush the handle, we are not checking for errors here.
			hSetEncoding handle utf8
			processHandle handle (initialFilePosition filePath) (initialState "lala")
			-- TODO: At least log the closing error!
			try $ hClose handle
			return ()

processHandle :: Handle -> Position -> GrepState -> IO ()
processHandle handle position state = do
	((eitherAns, log, state):xs) <- runGrepM (readLines handle) position state
	case eitherAns of
		Left e -> putStrLn e
		Right a -> return ()
	mapM_ putStrLn log

readLines :: MonadIO m => Handle -> GrepM m ()
readLines handle = do 
	-- Not checking errors here, if hIsEOF fails readLine should have failed before.
	isEOF <- liftIO $ hIsEOF handle
	unless isEOF $ readLine handle >> local incrementLineNumber (readLines handle)

readLine :: MonadIO m => Handle -> GrepM m ()
readLine handle = do
	eitherLineStr <- liftIO $ try (hGetLine handle)
	case eitherLineStr of
		Left e -> do
			position <- ask
			let fileName = getFileName position
			let lineNumber = getLineNumber position
			throwError $ "Skipping file \"" ++ fileName ++ "\", error reading line number " ++ (show lineNumber) ++ ": " ++ (show e)
		Right lineStr -> do
			modifyState NewLine
			readColumns lineStr

readColumns :: MonadIO m => String -> GrepM m ()
readColumns [] = return ()
readColumns (x:xs) = readColumn x >> local incrementColumnNumber (readColumns xs)

readColumn :: MonadIO m => Char -> GrepM m ()
readColumn columnChar = do
	modifyState (AddChar columnChar)

modifyState :: MonadIO m => Action -> GrepM m ()
modifyState action = do
	state <- get
	newState <- stateStep action state
	put newState

{-- la
lalala
--}

