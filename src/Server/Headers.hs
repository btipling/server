module Server.Headers where

import qualified Data.ByteArray as DBA
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text as DT
import qualified Data.List.Split as Split
import qualified Data.List as DL

responseHeaders :: String -> String
responseHeaders contentLength = "HTTP/1.1 200 OK!\n" ++ contentLength ++ "\nConnection: close\nContent-Type: text/plain; charset=utf-8\n\n"

requestHeaders :: String -> [(String, String)]
requestHeaders s = let
        lines = tail (Split.splitOn "\r\n" s)
        filteredLines = filter isHeader lines
    in(fmap parseHeader filteredLines)

isHeader :: String -> Bool
isHeader line = case (DL.elemIndex ':' line) of
    Nothing -> False
    Just _ -> True

parseHeader :: String -> (String, String)
parseHeader line = let
    index = case (DL.elemIndex ':' line) of
        Nothing -> -1
        Just x -> x
    in (validateHeader index line)

validateHeader :: Int -> String -> (String, String)
validateHeader index line | index == -1 = ("", "")
                          | (length line) < (index + 2) = ("", "")
                          | otherwise = parseHeaderAt index line

parseHeaderAt :: Int -> String -> (String, String)
parseHeaderAt i line = let
    header = take i line
    value = drop (i + 2) line
    in (header, value)

contentLength :: String -> String
contentLength content = let
  bs = encodeUtf8 (DT.pack content)
  l = DBA.length bs
  lengthStr = show l
  in ("Content-Length: " ++ lengthStr)
