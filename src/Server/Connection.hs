module Server.Connection where

import Network.Socket
import Network.Socket.ByteString as NSB
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import qualified Data.Text as DT
import qualified Data.List as DL
import qualified Data.ByteArray as DBA
import qualified Control.Exception as CE
import qualified Server.Headers as SH
import qualified Server.Handler as SHA

run :: SHA.RequestHandler -> IO ()
run requestHandler = do
    Prelude.putStrLn "server connection starting up"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock RecvTimeOut 1000
    bind sock (SockAddrInet 8080 iNADDR_ANY)
    listen sock 2
    mainLoop sock requestHandler

mainLoop :: Socket -> SHA.RequestHandler -> IO ()
mainLoop sock requestHandler = do
    conn <- accept sock
    readable <- isReadable sock
    runConn conn requestHandler
    mainLoop sock requestHandler

catchAny :: IO a -> (CE.IOException -> IO a) -> IO a
catchAny = CE.catch

runConn :: (Socket, SockAddr) -> SHA.RequestHandler -> IO ()
runConn (sock, address) requestHandler = do
    Prelude.putStrLn ("runConn " ++ (show address))
    received <- catchAny (receiveBytes sock 1) $ \e -> do
      putStrLn ("Receive network exception: " ++ (show e))
      return ""
    Prelude.putStrLn ("\n" ++ received)
    sendBytes sock (response received requestHandler)
    close sock

receiveBytes :: Socket -> Int -> IO String
receiveBytes socket n = do
  bytes <- NSB.recv socket 1024
  let l = DBA.length bytes
  let lengthStr = show l
  let txt = decodeUtf8 bytes
  let str = DT.unpack txt
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

response :: String -> SHA.RequestHandler -> String
response rawRequest requestHandler = let
    requestHeaders = SH.requestHeaders rawRequest
    c = requestHandler requestHeaders
    l = SH.contentLength c
    in ((SH.responseHeaders l) ++ c)