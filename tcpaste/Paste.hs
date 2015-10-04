module Paste (process) where

import qualified Data.ByteString.Lazy as BL
import Network
import System.IO


process :: String -> String -> IO ()
process host port = withSocketsDo . runPaste host . toPortId $ port

toPortId :: String -> PortID
toPortId = PortNumber . toEnum . read

runPaste :: HostName -> PortID -> IO ()
runPaste host port = do
    server <- connectTo host port
    copy stdin server
    copy server stdout

copy :: Handle -> Handle -> IO ()
copy hin hout = do
    hSetBinaryMode hin True
    hSetBinaryMode hout True

    din <- BL.hGetContents hin
    BL.hPutStr hout din

    hFlush hout

