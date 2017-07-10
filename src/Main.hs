module Main where

import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Server.Connection as Connection
import qualified Server.Handler as Handler
import qualified Data.Map.Strict as Map
import qualified FileSystem.Directory as Directory
import Data.Map.Strict((!))

main :: IO ()
main = do
  args <- Environment.getArgs
  let path = if length args > 0 then head args else ""
  success <- Directory.validate path
  if success
    then do
      Prelude.putStrLn "Application is starting."
      Connection.run $ getResponse path
    else do
      Prelude.putStrLn $ "'" ++ path ++ "' is either not a valid path or it doesn't exist."
      Exit.exitWith $ Exit.ExitFailure 1

getResponse :: String -> Handler.HttpRequest -> Handler.HandlerResponse
getResponse path requestData = let
  userAgent  = getUserAgent $ Handler.httpRequestHeaders requestData
  content    = "𝒜 ☃ was visited by " ++ (show requestData) ++ "!\n" ++ path
  httpStatus = 200
  in (Handler.Response {
    Handler.content = content,
    Handler.status  = httpStatus
  })

getUserAgent :: Map.Map String String -> String
getUserAgent headers = case (Map.member "User-Agent" headers) of
  False -> ""
  True  -> headers ! "User-Agent"
