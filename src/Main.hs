module Main where

import qualified Server.Connection as SC
import qualified Server.Handler as SH

main :: IO ()
main = do
  Prelude.putStrLn "Application is starting."
  SC.run getResponse
  
getResponse :: SH.RequestHandler
getResponse headers = "𝒜 ☃\n"