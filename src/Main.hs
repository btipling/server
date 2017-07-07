module Main where

import qualified Server.Connection as SC
import qualified Server.Handler as Handler
import qualified Data.Map.Strict as DM
import Data.Map.Strict((!))

main :: IO ()
main = do
  Prelude.putStrLn "Application is starting."
  SC.run getResponse
  
getResponse :: Handler.RequestHandler
getResponse headers = let 
  userAgent = getUserAgent headers
  content = "𝒜 ☃ was visited by " ++ userAgent ++ "!\n"
  httpStatus = 200
  in (Handler.Response {
    Handler.content = content,
    Handler.status = httpStatus
  })

getUserAgent :: DM.Map String String -> String
getUserAgent headers = case (DM.member "User-Agent" headers) of 
  False -> ""
  True -> headers ! "User-Agent"