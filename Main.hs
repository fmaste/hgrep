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

type FileLine = (Int, String)
type FileContent = [FileLine]
type DirectoryContent = [FileContent]

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		then do
			(_, _, log) <- runRWST (processPath $ head args) 0 () -- Take the first argument as the path if there is one.
			putStrLn "------ LOG ------"
			mapM_ putStrLn log
			return ()
		else do
			(_, _, log) <- runRWST (processHandle stdin) 0 () -- If no argument process stdin.
			putStrLn "------ LOG ------"
			mapM_ putStrLn log
			return ()

processPath :: FilePath -> RWST Int [String] () IO DirectoryContent
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
					then processDirPath path 
					else do
						lines <- processFilePath path 
						liftIO $ mapM_ (\(num,text) -> putStrLn text) lines
						return [lines]

processDirPath :: FilePath -> RWST Int [String] () IO DirectoryContent
processDirPath dirPath = do
	tell ["Processing dir path: " ++ dirPath]
	paths <- liftIO $ getDirectoryContents dirPath
	dirContentsList <- sequence $ map processPath $ map ((dirPath ++ "/") ++) $ excludeDots paths
	return $ concat dirContentsList

excludeDots = filter (flip notElem [".", ".."])

processFilePath :: FilePath -> RWST Int [String] () IO FileContent
processFilePath filePath = do
	tell ["Processing file path: " ++ filePath]
	handle <- liftIO $ openFile filePath ReadMode
	encoding <- liftIO $ hGetEncoding handle
	lines <- if isNothing encoding
		then do
			tell ["Skipping binary file: " ++ filePath]
			return []
		else processHandle handle
	liftIO $ hClose handle
	return lines

processHandle :: Handle -> RWST Int [String] () IO FileContent
processHandle handle = do
	tell ["Processing handle: " ++ (show handle)]
	liftIO $ hSetBuffering handle $ BlockBuffering (Just 2048)
	liftIO $ hSetEncoding handle utf8
	lines <- readLines handle
	return lines

readLines :: Handle -> RWST Int [String] () IO FileContent
readLines handle = do
	isEOF <- liftIO $ hIsEOF handle
	if isEOF 
		then return [] 
		else do
			head' <- readLine handle
			tail' <- readLines handle
			return $ head' : tail'

readLine :: Handle -> RWST Int [String] () IO FileLine
readLine handle = do
	line <- liftIO $ hGetLine handle
	return (0, line)

