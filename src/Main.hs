module Main where

import Network.Socket
import Network.Socket.ByteString as NSB
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text as DT
import qualified Data.ByteArray as DBA

main :: IO ()
main = do
  Prelude.putStrLn "server starting up"
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8080 iNADDR_ANY)
  listen sock 2
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    sendBytes sock helloResponse
    close sock

sendBytes:: Socket -> String -> IO Int
sendBytes sock content = let
  bs = encodeUtf8 (DT.pack content)
  in (NSB.send sock bs)

helloResponse :: String
helloResponse = let
  c = "☃\n"
  l = contentLength c
  in ((header l) ++ c)

header :: String -> String
header contentLength = "HTTP/1.1 200 OK!\n" ++ contentLength ++ "\nConnection: close\nContent-Type: text/plain; charset=utf-8\n\n"

contentLength :: String -> String
contentLength content = let
  bs = encodeUtf8 (DT.pack content)
  l = DBA.length bs
  lengthStr = show l
  in ("Content-Length: " ++ lengthStr)
