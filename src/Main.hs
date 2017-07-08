module Main where

import qualified Server.Connection as Connection
import qualified Server.Handler as Handler
import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))

main :: IO ()
main = do
  Prelude.putStrLn "Application is starting."
  Connection.run getResponse

getResponse :: Handler.RequestHandler
getResponse requestData = let
  userAgent  = getUserAgent $ Handler.httpRequestHeaders requestData
  content    = "𝒜 ☃ was visited by " ++ (show requestData) ++ "!\n"
  httpStatus = 200
  in (Handler.Response {
    Handler.content = content,
    Handler.status  = httpStatus
  })

getUserAgent :: Map.Map String String -> String
getUserAgent headers = case (Map.member "User-Agent" headers) of
  False -> ""
  True  -> headers ! "User-Agent"
