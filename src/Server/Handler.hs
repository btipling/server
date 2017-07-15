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
    httpRequestPathList,
    HttpHeadersMap) where

import qualified Data.Map.Strict as Map

data HandlerResponse = Response {
    content :: String,
    status  :: Int
} deriving (Show)

type HttpHeadersMap = Map.Map String String

data HttpRequest = Request {
    httpRequestRaw        :: String,
    httpRequestContent    :: String,
    httpRequestMethod     :: Int,
    httpRequestHeaders    :: HttpHeadersMap,
    httpRequestQuery      :: HttpHeadersMap,
    httpRequestPathString :: String,
    httpRequestPathList   :: [String]
} deriving (Show)

type RequestHandler = HttpRequest -> IO HandlerResponse
