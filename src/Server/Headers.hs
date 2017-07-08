module Server.Headers where

import qualified Data.ByteArray as ByteArray
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text as Text
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map

responseHeaders :: String -> Map.Map String String
responseHeaders content = Map.fromList [
    (contentLength content),
    ("Connection",   "close"),
    ("Content-Type", "text/plain; charset=utf-8")]

requestHeaders :: String -> Map.Map String String
requestHeaders s = let
        lines           = tail (Split.splitOn "\r\n" s)
        filtereListines = filter isHeader lines
        headerPairs     = fmap parseHeader filtereListines
    in (mapHeaders headerPairs)

mapHeaders :: [(String, String)] -> Map.Map String String
mapHeaders headerPairs = Map.fromList headerPairs

isHeader :: String -> Bool
isHeader line = case (List.elemIndex ':' line) of
    Nothing -> False
    Just _  -> True

parseHeader :: String -> (String, String)
parseHeader line = let
    index = case (List.elemIndex ':' line) of
        Nothing -> -1
        Just x  -> x
    in (validateHeader index line)

validateHeader :: Int -> String -> (String, String)
validateHeader index line | index == -1                 = ("", "")
                          | (length line) < (index + 2) = ("", "")
                          | otherwise                   = parseHeaderAt index line

parseHeaderAt :: Int -> String -> (String, String)
parseHeaderAt i line = let
    header = take i line
    value  = drop (i + 2) line
    in (header, value)

contentLength :: String -> (String, String)
contentLength content = let
    bs        = encodeUtf8 (Text.pack content)
    l         = ByteArray.length bs
    lengthStr = show l
    in (("Content-Length", lengthStr))
