module Main (
	main
) where

import Control.Monad
import Control.Monad.Writer
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

-- Writer monad

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		then do
			log <- execWriterT $ processPath $ head args -- Take the first argument as the path if there is one.
			putStrLn "------ LOG ------"
			mapM_ putStrLn log
			return ()
		else do
			log <- execWriterT $ processHandle stdin -- If no argument process stdin.
			putStrLn "------ LOG ------"
			mapM_ putStrLn log
			return ()

processPath :: FilePath -> WriterT [String] IO ()
processPath path = do
	tell ["Processing path: " ++ path]
	isDir <- lift $ doesDirectoryExist path
	isFile <- lift $ doesFileExist path
	if not $ isDir || isFile
		then tell [path ++ " does not exists"]
		else do
			perms <- lift $ getPermissions path
			if not $ readable perms
				then tell [path ++ " has no read permission"]
				else if isDir 
					then processDirPath path 
					else do
						lines <- processFilePath path 
						return ()

processDirPath :: FilePath -> WriterT [String] IO ()
processDirPath dirPath = do
	tell ["Processing dir path: " ++ dirPath]
	paths <- lift $ getDirectoryContents dirPath
	sequence_  $ map processPath $ map ((dirPath ++ "/") ++) $ filter (flip notElem [".", ".."]) paths

processFilePath :: FilePath -> WriterT [String] IO [String]
processFilePath filePath = do
	tell ["Processing file path: " ++ filePath]
	handle <- lift $ openFile filePath ReadMode
	encoding <- lift $ hGetEncoding handle
	lines <- if isNothing encoding
		then do
			tell ["Skipping binary file: " ++ filePath]
			return []
		else processHandle handle
	lift $ hClose handle
	return lines

processHandle :: Handle -> WriterT [String] IO [String]
processHandle handle = do
	tell ["Processing handle: " ++ (show handle)]
	lift $ hSetBuffering handle $ BlockBuffering (Just 2048)
	lift $ hSetEncoding handle utf8
	lines <- readLines handle
	lift $ mapM_ putStrLn lines
	return lines

readLines :: Handle -> WriterT [String] IO [String]
readLines handle = do
	isEOF <- lift $ hIsEOF handle
	if isEOF 
		then return [] 
		else do
			head' <- readLine handle
			tail' <- readLines handle
			return $ head' : tail'

readLine :: Handle -> WriterT [String] IO String
readLine handle = do
	line <- lift $ hGetLine handle
	return line

