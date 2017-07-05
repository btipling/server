module Main where

import qualified Server.Connection as SC
import qualified Server.Handler as SH

main :: IO ()
main = do
  Prelude.putStrLn "Application is starting."
  SC.run getResponse
  
getResponse :: SH.RequestHandler
getResponse headers = let 
  headersDescription = foldl concatonateHeaders "" headers
  in ("𝒜 ☃\n" ++ headersDescription ++ "\n")

concatonateHeaders :: String -> (String, String) -> String
concatonateHeaders acc (headerName, headerValue) = acc ++ "\n" ++ headerName ++ ": " ++ headerValue