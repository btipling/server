module Server.Handler (HandlerResponse(Response), RequestHandler, content, status) where

import qualified Data.Map.Strict as DMS

data HandlerResponse = Response { 
    content :: String, 
    status :: Int
} deriving (Show)

type RequestHandler = DMS.Map String String -> HandlerResponse
