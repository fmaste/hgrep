module Main (
	main
) where

import Control.Monad
import System (
	getArgs)
import System.IO (
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

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		then processPath $ head args -- Take the first argument as the path if there is one.
		else processHandle stdin -- If no argument process stdin.

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

processDirPath dirPath = do
	putStrLn $ "//**-- Processing dir path: " ++ dirPath
	paths <- getDirectoryContents dirPath
	sequence_  $ map processPath $ map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths

processFilePath filePath = do
	putStrLn $ "//**-- Processing file path: " ++ filePath
	handle <- openFile filePath ReadMode
	hSetBuffering handle $ BlockBuffering (Just 2048)
	processHandle handle
	hClose handle

processHandle handle = do
	lines <- readLines handle
	return ()

readLines handle = do
	isEOF <- hIsEOF handle
	if isEOF then return [] else do
		head' <- hGetLine handle 
		putStrLn head'
		tail' <- readLines handle
		return $ head' : tail'

