module Main where

import qualified Server.Connection as SC

main :: IO ()
main = do
  Prelude.putStrLn "server starting up"
  SC.run getResponse
  
getResponse :: String
getResponse = "𝒜 ☃\n"