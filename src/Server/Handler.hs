module Server.Handler (
    HandlerResponse(Response),
    RequestHandler,
    content,
    status,
    HttpRequest(Request),
    httpRequestRaw,
    httpRequestContent,
    httpRequestMethod,
    httpRequestHeaders,
    httpRequestQuery,
    httpRequestPathString,
    httpRequestPathList) where

import qualified Data.Map.Strict as Map

data HandlerResponse = Response {
    content :: String,
    status  :: Int
} deriving (Show)

data HttpRequest = Request {
    httpRequestRaw        :: String,
    httpRequestContent    :: String,
    httpRequestMethod     :: Int,
    httpRequestHeaders    :: Map.Map String String,
    httpRequestQuery      :: Map.Map String String,
    httpRequestPathString :: String,
    httpRequestPathList   :: [String]
} deriving (Show)

type RequestHandler = HttpRequest -> HandlerResponse
