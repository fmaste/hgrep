module Main (
	main
) where

import Control.Monad
-- "sudo ghc-pkg expose transformers" was needed.
-- See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=626985
-- And: http://stackoverflow.com/questions/5252066/why-is-package-hidden-by-default-and-how-can-i-unhide-it
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.Maybe (
	isNothing)
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

type Log = [String]

-------------------------------------------------------------------------------

type LineNumber = Integer

data Position = Stdin LineNumber | Path FilePath | Directory FilePath | File FilePath LineNumber

incrementLineNumber (File fp ln) = File fp (ln + 1)
incrementLineNumber (Stdin ln) = Stdin (ln + 1)

getLineNumber (File _ ln) = ln
getLineNumber (Stdin ln) = ln

-------------------------------------------------------------------------------

type FileLine = (Integer, String)
type FileContent = [FileLine]
type DirectoryContent = [FileContent]

main :: IO ()
main = do
	args <- getArgs
	log <- if length args >= 1 
		then do
			-- Take the first argument as the path if there is one.
			let path = head args
			(_, _, log) <- runRWST (processPath $ path) (Path path) ()
			return log
		else do
			-- If no argument process stdin.
			(_, _, log) <- runRWST (processHandle stdin) (Stdin 1) ()
			return log
	putStrLn "------ LOG ------"
	mapM_ putStrLn log
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
						liftIO $ mapM_ (\(num,text) -> putStrLn text) lines
						return [lines]

processDirPath :: FilePath -> GrepMonad DirectoryContent
processDirPath dirPath = do
	tell ["Processing dir path: " ++ dirPath]
	paths <- liftIO $ getDirectoryContents dirPath
	let filteredPaths =  map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths
	dirContentsList <- sequence $ map (\p -> local (\r -> Directory dirPath) (processPath p)) filteredPaths
	return $ concat dirContentsList

processFilePath :: FilePath -> GrepMonad FileContent
processFilePath filePath = do
	tell ["Processing file path: " ++ filePath]
	handle <- liftIO $ openFile filePath ReadMode
	lines <- local (\r -> File filePath 1) (processHandle handle)
	liftIO $ hClose handle
	return lines

processHandle :: Handle -> GrepMonad FileContent
processHandle handle = do
	tell ["Processing handle: " ++ (show handle)]
	liftIO $ hSetBuffering handle $ BlockBuffering (Just 2048)
	liftIO $ hSetEncoding handle utf8
	lines <- readLines handle
	return lines

readLines :: Handle -> GrepMonad FileContent
readLines handle = do
	isEOF <- liftIO $ hIsEOF handle
	if isEOF 
		then return [] 
		else do
			head' <- readLine handle
			tail' <- local incrementLineNumber (readLines handle)
			return $ head' : tail'

readLine :: Handle -> GrepMonad FileLine
readLine handle = do
	lineStr <- liftIO $ hGetLine handle
	position <- ask
	let lineNumber = getLineNumber position
	return (lineNumber, lineStr)

