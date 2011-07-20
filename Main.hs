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

type GrepMonad a = RWST Position Log () IO a

-------------------------------------------------------------------------------

type LineNumber = Integer

data Position = Stdin LineNumber | Path FilePath | Directory FilePath | File FilePath LineNumber

incrementLineNumber (File fp ln) = File fp (ln + 1)
incrementLineNumber (Stdin ln) = Stdin (ln + 1)

getFileName (Stdin _) = "Standard input"
getFileName (Path fp) = fp
getFileName (Directory fp) = fp
getFileName (File fp _) = fp

getLineNumber (File _ ln) = ln
getLineNumber (Stdin ln) = ln

-------------------------------------------------------------------------------

type Log = [String]

-------------------------------------------------------------------------------

type FileLine = (Integer, String)
type FileContent = [FileLine]
type DirectoryContent = [FileContent]

main :: IO ()
main = do
	args <- getArgs
	(ans, log) <- if length args >= 1 
		then do
			-- Take the first argument as the path if there is one.
			let path = head args
			(ans, _, log) <- runRWST (processPath $ path) (Path path) ()
			return (ans, log)
		else do
			-- If no argument process stdin.
			(ans, _, log) <- runRWST (processHandle stdin) (Stdin 1) ()
			return ([ans] ,log)
	putStrLn "------ LOG ------"
	mapM_ putStrLn log
	putStrLn "------ ANS ------"
	putStrLn (show ans)
	return ()

processPath :: FilePath -> GrepMonad DirectoryContent
processPath path = do
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
					then local (\r -> Directory path) (processDirPath path)
					else do
						lines <- processFilePath path 
						--liftIO $ mapM_ (\(num,text) -> putStrLn text) lines
						return [lines]

processDirPath :: FilePath -> GrepMonad DirectoryContent
processDirPath dirPath = do
	tell ["Processing dir path: " ++ dirPath]
	-- TODO: Check errors!
	paths <- liftIO $ getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> local (\r -> Directory dirPath) (processPath p)) filteredPaths
	return $ concat dirContentsList

processFilePath :: FilePath -> GrepMonad FileContent
processFilePath filePath = do
	tell ["Processing file path: " ++ filePath]
	-- TODO: Check error when opening.
	handle <- liftIO $ openFile filePath ReadMode
	lines <- local (\r -> File filePath 1) (processHandle handle)
	-- TODO: At least log the closing error!
	liftIO $ hClose handle
	return lines

processHandle :: Handle -> GrepMonad FileContent
processHandle handle = do
	tell ["Processing handle: " ++ (show handle)]
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
	eitherLineStr <- liftIO $ try (hGetLine handle)
	either (whenLeft lineNumber) (whenRight lineNumber) eitherLineStr where
		whenLeft lineNumber e = do
			tell ["Error reading line number " ++ (show lineNumber) ++ ": " ++ (show e)]
			return Nothing
		whenRight lineNumber lineStr = do
			return $ Just (lineNumber, lineStr)

