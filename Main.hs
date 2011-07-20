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

type GrepMonad a = RWST Int [String] () IO a
data Position = Position Int
type FileLine = (Int, String)
type FileContent = [FileLine]
type DirectoryContent = [FileContent]

main :: IO ()
main = do
	args <- getArgs
	log <- if length args >= 1 
		then do
			-- Take the first argument as the path if there is one.
			(_, _, log) <- runRWST (processPath $ head args) 0 ()
			return log
		else do
			-- If no argument process stdin.
			(_, _, log) <- runRWST (processHandle stdin) 0 ()
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
					then processDirPath path 
					else do
						lines <- processFilePath path 
						liftIO $ mapM_ (\(num,text) -> putStrLn text) lines
						return [lines]

processDirPath :: FilePath -> GrepMonad DirectoryContent
processDirPath dirPath = do
	tell ["Processing dir path: " ++ dirPath]
	paths <- liftIO $ getDirectoryContents dirPath
	dirContentsList <- sequence $ map processPath $ map ((dirPath ++ "/") ++) $ excludeDots paths
	return $ concat dirContentsList

excludeDots = filter (flip notElem [".", ".."])

processFilePath :: FilePath -> GrepMonad FileContent
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
			tail' <- readLines handle
			return $ head' : tail'

readLine :: Handle -> GrepMonad FileLine
readLine handle = do
	line <- liftIO $ hGetLine handle
	return (0, line)

