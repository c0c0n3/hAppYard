module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.Environment
import System.IO


main = withSocketsDo $ do
	[responsePath] <- getArgs
	sock <- listenOn $ PortNumber 1234
	forever $ do
		(handle, hostname, port) <- accept sock
		serve handle responsePath
		hFlush handle
		hClose handle

serve h responsePath = printRequest h >> outputResponse h responsePath
	
printRequest h = do
	req <- hGetContents h
	putStrLn "---------------------- REQUEST ------------------------------"
	putStr req
	putStrLn "-------------------------------------------------------------"

outputResponse h responsePath = do
	msg <- readFile responsePath
	hPutStr h msg

--
-- COMPILE:
--     ghc -odir bin -hidir bin -o bin\pong --make Main.hs
--
-- USE:
--     cd bin
--     pong response-filepath
--