module Main where

import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Server.Connection as Connection
import qualified Server.Handler as Handler
import qualified Data.Map.Strict as Map
import qualified FileSystem
import Data.Map.Strict((!))

main :: IO ()
main = do
  args <- Environment.getArgs
  let path = if length args > 0 then head args else ""
  success <- FileSystem.validate path
  if success
    then do
      Prelude.putStrLn "Application is starting."
      Connection.run $ getResponse path
    else do
      Prelude.putStrLn $ "'" ++ path ++ "' is either not a valid path or it doesn't exist."
      Exit.exitWith $ Exit.ExitFailure 1

getResponse :: String -> Handler.HttpRequest -> IO Handler.HandlerResponse
getResponse path requestData = do
  let userAgent  = getUserAgent $ Handler.httpRequestHeaders requestData
  result <- FileSystem.getPathContents path $ Handler.httpRequestPathList requestData
  case result of
    Nothing -> do
      let httpStatus = 404
      let content = "𝒜 ☃ says: \"Not found\""
      return Handler.Response {
        Handler.content = content,
        Handler.status  = httpStatus
      }
    Just pathData -> do
      let content    = "𝒜 ☃ was visited by " ++ (show requestData) ++ "!\n" ++ path ++ "\n" ++ pathData
      let httpStatus = 200
      return Handler.Response {
        Handler.content = content,
        Handler.status  = httpStatus
      }

getUserAgent :: Map.Map String String -> String
getUserAgent headers = case (Map.member "User-Agent" headers) of
  False -> ""
  True  -> headers ! "User-Agent"
