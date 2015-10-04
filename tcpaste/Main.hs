module Main where

import System.Environment

import Paste

main = do
     [host, port] <- getArgs
     process host port
     return ()

--
-- COMPILE:
--      mkdir -p bin
--      ghc -odir bin -hidir bin -o bin/tcpaste --make Main.hs
--
-- RUN:
--      tcpaste <host or ip> <port number>
--
-- e.g. paste the content of my-file to a server and output response to stdout
--      
--      cat my-file | tcpaste sum-host 1234
--
-- e.g. send (ill-formed) request to Google and output response to google.res
--
--      echo 'GET /' | tcpaste www.google.com 80 > google.res
--