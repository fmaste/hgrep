module Main (
	main
) where

import Control.Monad
import Data.Monoid
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

newtype Writer w a = Writer {runWriter :: (a, w)}

instance (Monoid w) => Monad (Writer w) where
	return x = Writer (x, mempty)
	(Writer (x, w)) >>= f = let (Writer (x', w')) = f x in Writer (x', mappend w w')

tell :: (Monoid w) => w -> Writer w ()
tell w = Writer ((), w)

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1 
		then processPath $ head args -- Take the first argument as the path if there is one.
		else processHandle stdin -- If no argument process stdin.

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
	processHandle handle
	hClose handle

processHandle :: Handle -> IO ()
processHandle handle = do
	lines <- readLines handle
	return ()

readLines :: Handle -> IO [String]
readLines handle = do
	isEOF <- hIsEOF handle
	if isEOF then return [] else do
		head' <- readLine handle 
		tail' <- readLines handle
		return $ head' : tail'

readLine :: Handle -> IO String
readLine handle = do
	line <- hGetLine handle
	putStrLn line
	return line

