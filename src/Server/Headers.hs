module Server.Headers where

import qualified Data.ByteArray as DBA
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text as DT

responseHeaders :: String -> String
responseHeaders contentLength = "HTTP/1.1 200 OK!\n" ++ contentLength ++ "\nConnection: close\nContent-Type: text/plain; charset=utf-8\n\n"

requestHeaders :: String -> [String]
requestHeaders s = []

contentLength :: String -> String
contentLength content = let
  bs = encodeUtf8 (DT.pack content)
  l = DBA.length bs
  lengthStr = show l
  in ("Content-Length: " ++ lengthStr)
