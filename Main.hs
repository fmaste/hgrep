module Main (
	main
) where

import Control.OldException
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
-- "sudo ghc-pkg expose transformers" was needed.
-- See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=626985
-- And: http://stackoverflow.com/questions/5252066/why-is-package-hidden-by-default-and-how-can-i-unhide-it
-- Also:
-- sudo cabal upgrade transformers
-- sudo cabal upgrade mtl
import Data.List
import Data.Maybe (
	isJust,
	isNothing,
	fromJust)
import Data.Either
import System (
	getArgs)
import System.IO (
	Handle,
	stdin, 
	putStrLn, 
	openFile, 
	hSetBuffering, 
	hGetEncoding,
	hSetEncoding,
	utf8,
	BufferMode(BlockBuffering), 
	hClose, 
	hIsEOF, 
	hGetLine, 
	IOMode(ReadMode))
import System.Directory (
	getPermissions, 
	Permissions(readable), 
	doesFileExist, 
	doesDirectoryExist, 
	getDirectoryContents)

-------------------------------------------------------------------------------

-- The Grep Monad has:
-- A Reader that allows to have as environment the actual position in a file.
-- A Writer to log messages.
-- A state with the parsing state machine.
-- And finally, allows to handle errors.
type GrepM a = ReaderT Position (ErrorT IOError (WriterT Log (StateT GrepState IO))) a

runGrepM :: GrepM a -> Position -> GrepState -> IO ((Either IOError a, Log), GrepState)
runGrepM gm pos state = runStateT (runWriterT (runErrorT (runReaderT gm pos))) state

-------------------------------------------------------------------------------

type LineNumber = Integer

type ColumnNumber = Integer

data Position = 
	-- A path to start processing.
	Path {fileName :: FilePath}
	-- A directory to traverse.
	| Directory {fileName :: FilePath}
	-- A file to parse.
	| File {fileName :: FilePath, lineNumber :: LineNumber, columnNumber :: ColumnNumber}
	-- Parsing stdin.
	| Stdin {lineNumber :: LineNumber, columnNumber :: ColumnNumber}
	deriving Show

initialStdinPosition = Stdin 1 1

initialFilePosition path = File path 1 1

getFileName (Stdin _ _) = "Standard input"
getFileName (Path fp) = fp
getFileName (Directory fp) = fp
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

data Action = NewFile Position
	| NewLine Position
	| NewChar Position Char
	deriving Show

-- The pattern, the pattern length and the array of (Position, Eq counts)
data GrepState = GrepState String Integer [(Position, Integer)] (Maybe Position)
	deriving Show

-- Create an initial array with (Path "", 0)
initialState :: String -> GrepState
initialState pattern = let
	lenInt = length pattern
	counts = replicate lenInt (Path "", 0)
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

-- scanl (\state char -> addChar (Path "") char state) (initialState "lala") "lalala"

-------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	log <- if length args >= 1 
		then do
			-- Take the first argument as the path if there is one.
			let path = head args
			((_, log), _) <- runGrepM (processPath path) (Path path) (initialState "lala")
			return log
		else do
			-- If no argument process stdin.
			((_, log), _) <- runGrepM (processHandle stdin) initialStdinPosition (initialState "lala")
			return log
	putStrLn "------ LOG ------"
	mapM_ putStrLn log
	return ()

processPath :: FilePath -> GrepM ()
processPath path = do
	tell ["Processing path: " ++ path]
	isDir <- liftIO $ doesDirectoryExist path
	isFile <- liftIO $ doesFileExist path
	if not $ isDir || isFile
		then do
			liftIO $ putStrLn $ path ++ " does not exists"
			return ()
		else do
			perms <- liftIO $ getPermissions path
			if not $ readable perms
				then liftIO $ putStrLn $ path ++ " has no read permission"
				else if isDir 
					then local (\r -> Directory path) $ processDirPath path
					else processFilePath path

processDirPath :: FilePath -> GrepM ()
processDirPath dirPath = do
	liftIO $ putStrLn $ "Processing dir path: " ++ dirPath
	-- TODO: Check errors!
	paths <- liftIO $ getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> local (\r -> Path p) (processPath p)) filteredPaths
	return ()

processFilePath :: FilePath -> GrepM ()
processFilePath filePath = do
	-- TODO: Check error when opening.
	eitherHandle <- liftIO $ try (openFile filePath ReadMode)
	either (whenLeft filePath) (whenRight filePath) eitherHandle where
		whenLeft filePath e = do
			liftIO $ putStrLn $ "Unable to open file " ++ (show filePath) ++ ": " ++ (show e)
		whenRight filePath handle = do
			-- It may only throw an error if handle was already used.
			liftIO $ hSetBuffering handle $ BlockBuffering (Just 2048)
			-- May need to flush the handle, we are not checking for errors here.
			liftIO $ hSetEncoding handle utf8
			local (\r -> initialFilePosition filePath) (processHandle handle)
			-- TODO: At least log the closing error!
			liftIO $ try (hClose handle)

			return ()

processHandle :: Handle -> GrepM ()
processHandle handle = readLines handle

readLines :: Handle -> GrepM ()
readLines handle = do
	-- Not checking errors here, if hIsEOF fails readLine should have failed before.
	isEOF <- liftIO $ hIsEOF handle
	unless isEOF $ do
		maybeHead <- readLine handle
		if isNothing maybeHead
			then do
				position <- ask
				let fileName = getFileName position
				liftIO $ putStrLn $ "Skipping file: " ++ (show fileName)
			else local incrementLineNumber (readLines handle)
	return ()

readLine :: Handle -> GrepM (Maybe ())
readLine handle = do
	position <- ask
	let lineNumber = getLineNumber position
	--modify $ resetState position
	eitherLineStr <- liftIO $ try (hGetLine handle)
	case eitherLineStr of
		Left e -> do
			liftIO $ putStrLn $ "Error reading line number " ++ (show lineNumber) ++ ": " ++ (show e)
			return Nothing
		Right lineStr -> do
			readColumns lineStr
			return $ Just ()

readColumns :: String -> GrepM ()
readColumns [] = return ()
readColumns (x:xs) = readColumn x >> local incrementColumnNumber (readColumns xs)

readColumn :: Char -> GrepM ()
readColumn columnChar = do
	position <- ask
	modify (addChar position columnChar)
	-- TODO: Leave the output on the state or use the writer!
	maybePos <- gets getLastMatchedPosition
	when (isJust maybePos) (tell ["Found in: " ++ (show $ fromJust maybePos)])

{-- la
lalala
--}

