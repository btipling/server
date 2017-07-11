module Server.Connection where

import Network.Socket
import Network.Socket.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.ByteArray as ByteArray
import qualified Server.Handler as Handler
import qualified Server.Http as Http
import qualified Utils
import Data.Text.Encoding(decodeUtf8, encodeUtf8)

run :: Handler.RequestHandler -> IO ()
run requestHandler = do
    Prelude.putStrLn "Server connection starting up."
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock RecvTimeOut 1000
    bind sock (SockAddrInet 8080 iNADDR_ANY)
    listen sock 2
    mainLoop sock requestHandler

mainLoop :: Socket -> Handler.RequestHandler -> IO ()
mainLoop sock requestHandler = do
    conn     <- accept sock
    readable <- isReadable sock
    runConn conn requestHandler
    mainLoop sock requestHandler

runConn :: (Socket, SockAddr) -> Handler.RequestHandler -> IO ()
runConn (sock, address) requestHandler = do
    Prelude.putStrLn ("runConn " ++ (show address))
    received <- Utils.catchAny (receiveBytes sock 1) $ \e -> do
      putStrLn ("Received network exception: " ++ (show e))
      return ""
    Prelude.putStrLn ("\n" ++ received)
    response <- Http.response received requestHandler
    sendBytes sock response
    close sock

receiveBytes :: Socket -> Int -> IO String
receiveBytes socket n = do
  bytes <- ByteString.recv socket 1024
  let l         = ByteArray.length bytes
  let lengthStr = show l
  let txt       = decodeUtf8 bytes
  let str       = Text.unpack txt
  if (List.isSuffixOf "\r\n" str)
    then return str
    else do
      substr <- receiveBytes socket (n + 1)
      let result = str ++ substr
      return result

sendBytes:: Socket -> String -> IO Int
sendBytes sock content = let
  bs = encodeUtf8 (Text.pack content)
  in (ByteString.send sock bs)
