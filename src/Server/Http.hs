module Server.Http where

import qualified Server.Headers as Headers
import qualified Server.Handler as Handler
import qualified Data.Map as Map
import Debug.Trace

data HttpMethod = GET | POST | HEADB deriving (Enum)

data HttpRequest = Request {
    httpRequestRaw        :: String,
    httpRequestContent    :: String,
    httpRequestMethod     :: Int,
    httpRequestHeaders    :: Map.Map String String,
    httpRequestQuery      :: Map.Map String String,
    httpRequestPathString :: String,
    httpRequestPathList   :: [String]
} deriving (Show)

status :: Int -> String
status statusCode = "HTTP/1.1 " ++ (show statusCode) ++ "\r\n"

response :: String -> Handler.RequestHandler -> String
response rawRequest requestHandler = let
    httpRequestData          = parseRequestData rawRequest
    handlerResponse          = requestHandler $ httpRequestHeaders httpRequestData
    content                  = Handler.content handlerResponse
    httpStatus               = status $ Handler.status handlerResponse
    responseHeaders          = Headers.responseHeaders content
    formattedResponseHeaders = formatResponseHeaders responseHeaders
    finalResponse            = httpStatus ++ formattedResponseHeaders ++ "\r\n" ++ content
    in (finalResponse)

parseRequestData :: String -> HttpRequest
parseRequestData rawRequest = let
        path = requestPath rawRequest
    in (Request {
    httpRequestRaw        = rawRequest,
    httpRequestContent    = "",
    httpRequestMethod     = requestMethod rawRequest,
    httpRequestHeaders    = Headers.requestHeaders rawRequest,
    httpRequestQuery      = queryParams rawRequest,
    httpRequestPathString = path,
    httpRequestPathList   = requestPathList path})

formatResponseHeaders :: Map.Map String String -> String
formatResponseHeaders responseHeaders = let
    headerPairs           = Map.assocs responseHeaders
    headerStrings         = fmap concatHeaderNameValue headerPairs
    responseHeadersString = foldl (++) "" headerStrings
    in (responseHeadersString)

concatHeaderNameValue :: (String, String) -> String
concatHeaderNameValue (name, value) = name ++ ": " ++ value ++ "\r\n"

queryParams :: String -> Map.Map String String
queryParams line = Map.fromList []

requestPath :: String -> String
requestPath line = ""

requestPathList :: String -> [String]
requestPathList path = [path]

requestMethod :: String -> Int
requestMethod line = fromEnum GET
