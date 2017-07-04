module Server.Headers where

import qualified Data.ByteArray as DBA
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text as DT

header :: String -> String
header contentLength = "HTTP/1.1 200 OK!\n" ++ contentLength ++ "\nConnection: close\nContent-Type: text/plain; charset=utf-8\n\n"

contentLength :: String -> String
contentLength content = let
  bs = encodeUtf8 (DT.pack content)
  l = DBA.length bs
  lengthStr = show l
  in ("Content-Length: " ++ lengthStr)
