module Main where

import qualified Server.Connection as SC
import qualified Server.Handler as SH
import qualified Data.Map.Strict as DM
import Data.Map.Strict((!))

main :: IO ()
main = do
  Prelude.putStrLn "Application is starting."
  SC.run getResponse
  
getResponse :: SH.RequestHandler
getResponse headers = let 
  userAgent = getUserAgent headers
  in ("𝒜 ☃ was visited by " ++ userAgent ++ "\n")

getUserAgent :: DM.Map String String -> String
getUserAgent headers = case (DM.member "User-Agent" headers) of 
  False -> ""
  True -> headers ! "User-Agent"