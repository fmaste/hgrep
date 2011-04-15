module Main (
	main
) where

import System (getArgs)
import System.IO (stdin, putStrLn, openFile, hClose, hIsEOF, hGetLine, IOMode(ReadMode))
import System.Directory (doesFileExist, doesDirectoryExist)

main :: IO ()
main = do
	args <- getArgs
	path <- return $ if length args >= 1 then head args else error "No path input" -- TODO: else stdin
	putStrLn path
	return ()

processPath path = do
	isFile <- doesFileExist path
	isDir <- doesDirectoryExist path
	if isDir 
		then processDirectory path 
		else processFile path

processDirectory fileName = do
	handle <- openFile fileName ReadMode
	hClose handle
	return fileName

processFile fileName = do
	handle <- openFile fileName ReadMode
	-- TODO: Set buffer mode
	lines <- readLines handle
	hClose handle
	return fileName

-- processFileHandle handle = do

readLines handle = do
	isEOF <- hIsEOF handle
	if isEOF then return [] else do
		head' <- hGetLine handle 
		tail' <- readLines handle
		return $ head' : tail'
