module Server.Connection where

import Network.Socket
import Network.Socket.ByteString as NSB
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import qualified Data.Text as DT
import qualified Data.List as DL
import qualified Data.ByteArray as DBA
import qualified Control.Exception as CE

run :: String -> IO ()
run responseHandler = do
    Prelude.putStrLn "server connection starting up"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock RecvTimeOut 1000
    bind sock (SockAddrInet 8080 iNADDR_ANY)
    listen sock 2
    mainLoop sock responseHandler

mainLoop :: Socket -> String -> IO ()
mainLoop sock responseHandler = do
    conn <- accept sock
    readable <- isReadable sock
    Prelude.putStrLn ("Is readable? " ++ (show readable))
    runConn conn responseHandler
    mainLoop sock responseHandler

catchAny :: IO a -> (CE.IOException -> IO a) -> IO a
catchAny = CE.catch

runConn :: (Socket, SockAddr) -> String -> IO ()
runConn (sock, address) responseHandler = do
    Prelude.putStrLn ("runConn " ++ (show address))
    received <- catchAny (receiveBytes sock 1) $ \e -> do
      putStrLn ("Receive exception: " ++ (show e))
      return ""
    Prelude.putStrLn ("\n\nReceived: " ++ received)
    sendBytes sock (helloResponse responseHandler)
    close sock

receiveBytes :: Socket -> Int -> IO String
receiveBytes socket n = do
  Prelude.putStrLn ("Going in " ++ (show n))
  bytes <- NSB.recv socket 1024
  Prelude.putStrLn ("done with " ++ (show n))
  let l = DBA.length bytes
  let lengthStr = show l
  let txt = decodeUtf8 bytes
  let str = DT.unpack txt
  Prelude.putStrLn ("\n\nReceived: " ++ str)
  if (DL.isSuffixOf "\r\n" str) 
    then return str
    else do
      substr <- receiveBytes socket (n + 1)
      let result = str ++ substr
      return result 

sendBytes:: Socket -> String -> IO Int
sendBytes sock content = let
  bs = encodeUtf8 (DT.pack content)
  in (NSB.send sock bs)

helloResponse :: String -> String
helloResponse responseHandler = let
  c = responseHandler
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
