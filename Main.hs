module Main (
	main
) where

import Control.OldException
import Control.Monad
-- "sudo ghc-pkg expose transformers" was needed.
-- See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=626985
-- And: http://stackoverflow.com/questions/5252066/why-is-package-hidden-by-default-and-how-can-i-unhide-it
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Error
import Data.List
import Data.Maybe (
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
type GrepMonad a = RWST Position Log State IO a

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
	-- From stdin.
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
data State = State String Integer [(Position, Integer)] (Maybe Position)
	deriving Show

-- Create an initial array with (Path "", 0)
initialState :: String -> State
initialState pattern = let
	lenInt = length pattern
	counts = replicate lenInt (Path "", 0)
	in State pattern (toInteger lenInt) counts Nothing

resetState :: Position -> State -> State
resetState pos (State pattern len _ _) = State pattern len (replicate (fromInteger len) (pos, 0)) Nothing

addChar :: Position -> Char -> State -> State
addChar addedPos addedChar (State pattern len counts _) = let 
	outCounts = map f $ zip pattern ((addedPos, 0):counts) where
		f (actualChar, (actualPos, actualEqs)) = 
			let actualEqs' = if actualChar == addedChar then (actualEqs + 1) else actualEqs
			in (actualPos, actualEqs')
	(lastPos, lastEqs) = last outCounts
	maybePos = if lastEqs == len then (Just lastPos) else Nothing
	in (State pattern len (init outCounts) maybePos)

getLastMatchedPosition :: State -> Maybe Position
getLastMatchedPosition (State _ _ _ maybePos) = maybePos

-- scanl (\state char -> addChar (Path "") char state) (initialState "lala") "lalala"

-------------------------------------------------------------------------------

type FileColumn = Char
type FileLine = String
type FileContent = [FileLine]
type DirectoryContent = [FileContent]

main :: IO ()
main = do
	args <- getArgs
	(ans, log) <- if length args >= 1 
		then do
			-- Take the first argument as the path if there is one.
			let path = head args
			(ans, _, log) <- runRWST processPath (Path path) (initialState "lala")
			return (ans, log)
		else do
			-- If no argument process stdin.
			(ans, _, log) <- runRWST (processHandle stdin) initialStdinPosition (initialState "lala")
			return ([ans] ,log)
	putStrLn "------ LOG ------"
	mapM_ putStrLn log
	--putStrLn "------ ANS ------"
	--putStrLn (show ans)
	return ()

processPath :: GrepMonad DirectoryContent
processPath = do
	position <- ask
	let path = getFileName position
	tell ["Processing path: " ++ path]
	isDir <- liftIO $ doesDirectoryExist path
	isFile <- liftIO $ doesFileExist path
	if not $ isDir || isFile
		then do
			tell [path ++ " does not exists"]
			return []
		else do
			perms <- liftIO $ getPermissions path
			if not $ readable perms
				then do
					tell [path ++ " has no read permission"]
					return []
				else if isDir 
					then local (\r -> Directory path) processDirPath
					else do
						lines <- processFilePath
						--liftIO $ mapM_ (\(num,text) -> putStrLn text) lines
						return [lines]

processDirPath :: GrepMonad DirectoryContent
processDirPath = do
	position <- ask
	let dirPath = getFileName position
	tell ["Processing dir path: " ++ dirPath]
	-- TODO: Check errors!
	paths <- liftIO $ getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> local (\r -> Path p) processPath) filteredPaths
	return $ concat dirContentsList

processFilePath :: GrepMonad FileContent
processFilePath = do
	position <- ask
	let filePath = getFileName position
	-- TODO: Check error when opening.
	eitherHandle <- liftIO $ try (openFile filePath ReadMode)
	either (whenLeft filePath) (whenRight filePath) eitherHandle where
		whenLeft filePath e = do
			tell ["Unable to open file " ++ (show filePath) ++ ": " ++ (show e)]
			return []
		whenRight filePath handle = do
			lines <- local (\r -> initialFilePosition filePath) (processHandle handle)
			-- TODO: At least log the closing error!
			liftIO $ try (hClose handle)
			return lines

processHandle :: Handle -> GrepMonad FileContent
processHandle handle = do
	-- It may only throw an error if handle was already used.
	liftIO $ hSetBuffering handle $ BlockBuffering (Just 2048)
	-- May need to flush the handle, we are not checking for errors here.
	liftIO $ hSetEncoding handle utf8
	lines <- readLines handle
	return lines

readLines :: Handle -> GrepMonad FileContent
readLines handle = do
	-- Not checking errors here, if hIsEOF fails readLine should have failed before.
	isEOF <- liftIO $ hIsEOF handle
	if isEOF 
		then return [] 
		else do
			maybeHead <- readLine handle
			if isNothing maybeHead
				then do
					position <- ask
					let fileName = getFileName position
					tell ["Skipping file: " ++ (show fileName)]
					return []
				else do
					tail' <- local incrementLineNumber (readLines handle)
					return $ (fromJust maybeHead) : tail'

readLine :: Handle -> GrepMonad (Maybe FileLine)
readLine handle = do
	position <- ask
	let lineNumber = getLineNumber position
	modify $ resetState position
	eitherLineStr <- liftIO $ try (hGetLine handle)
	either (whenLeft lineNumber) (whenRight lineNumber) eitherLineStr where
		whenLeft lineNumber e = do
			tell ["Error reading line number " ++ (show lineNumber) ++ ": " ++ (show e)]
			return Nothing
		whenRight lineNumber lineStr = do
			fileColumns <- readColumns lineStr
			return $ Just fileColumns

readColumns :: String -> GrepMonad String
readColumns lineStr = do
	position <- ask
	let lineNumber = getLineNumber position
	case lineStr of
		[] -> return []
		(x:xs) -> do
			head' <- readColumn x
			tail' <- local incrementColumnNumber (readColumns xs)
			return $ head' : tail'

readColumn :: Char -> GrepMonad Char
readColumn columnChar = do
	position <- ask
	modify (addChar position columnChar)
	checkMatch
	return columnChar

checkMatch :: GrepMonad ()
checkMatch = do
	maybePos <- gets getLastMatchedPosition
	case maybePos of
		Just pos -> tell ["Found in: " ++ (show pos)]
		Nothing -> return ()

-- lalalala

