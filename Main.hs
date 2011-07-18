module Main (
	main
) where

import Control.Monad
import Control.Monad.Writer
import System (
	getArgs)
import System.IO (
	Handle,
	stdin, 
	putStrLn, 
	openFile, 
	hSetBuffering, 
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

-- Writer monad

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		then processPath $ head args -- Take the first argument as the path if there is one.
		else do
			runWriterT $ processHandle stdin -- If no argument process stdin.
			return ()

processPath :: FilePath -> IO ()
processPath path = do
	putStrLn $ "//**-- Processing path: " ++ path
	isDir <- doesDirectoryExist path
	isFile <- doesFileExist path
	if not $ isDir || isFile
		then putStrLn  $ "//**-- " ++ path ++ " does not exists"
		else do
			perms <- getPermissions path
			if not $ readable perms
				then putStrLn  $ "//**-- " ++ path ++ " has no read permission"
				else if isDir 
					then processDirPath path 
					else processFilePath path 

processDirPath :: FilePath -> IO ()
processDirPath dirPath = do
	putStrLn $ "//**-- Processing dir path: " ++ dirPath
	paths <- getDirectoryContents dirPath
	sequence_  $ map processPath $ map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths

processFilePath :: FilePath -> IO ()
processFilePath filePath = do
	putStrLn $ "//**-- Processing file path: " ++ filePath
	handle <- openFile filePath ReadMode
	hSetBuffering handle $ BlockBuffering (Just 2048)
	execWriterT $ processHandle handle
	hClose handle

processHandle :: Handle -> WriterT [String] IO ()
processHandle handle = do
	lines <- readLines handle
	return ()

readLines :: Handle -> WriterT [String] IO [String]
readLines handle = do
	isEOF <- lift $ hIsEOF handle
	if isEOF then return [] else do
		head' <- readLine handle
		lift $ putStrLn head' 
		tail' <- readLines handle
		return $ head' : tail'

readLine :: Handle -> WriterT [String] IO String
readLine handle = do
	line <- lift $ hGetLine handle
	return line

