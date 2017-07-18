module Server.Http (response) where

import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Server.Handler  as Handler
import qualified Server.Headers  as Headers

data HttpMethod = UNSUPPORTED | GET | POST | HEAD deriving (Enum)

status :: Int -> String
status statusCode = "HTTP/1.1 " ++ (show statusCode) ++ "\r\n"

response :: String -> Handler.RequestHandler -> IO String
response rawRequest requestHandler = if length rawRequest > 0
    then do
        let httpRequestData          = parseRequestData rawRequest
        handlerResponse              <- requestHandler httpRequestData
        let content                  = Handler.content handlerResponse
        let httpStatus               = status $ Handler.status handlerResponse
        let responseHeaders          = Headers.responseHeaders content
        let formattedResponseHeaders = formatResponseHeaders responseHeaders
        return (httpStatus ++ formattedResponseHeaders ++ "\r\n" ++ content)
    else return ""

parseRequestData :: String -> Handler.HttpRequest
parseRequestData rawRequest = let
        requestLine      = head $ Split.splitOn "\r\n" rawRequest
        requestLineParts = Split.splitOn " " requestLine
        pathPart         = requestLineParts !! 1
        path             = requestPath pathPart
    in (Handler.Request {
        Handler.httpRequestRaw        = rawRequest,
        Handler.httpRequestContent    = "",
        Handler.httpRequestMethod     = requestMethod $ head requestLineParts,
        Handler.httpRequestHeaders    = Headers.requestHeaders rawRequest,
        Handler.httpRequestQuery      = queryParams pathPart,
        Handler.httpRequestPathString = path,
        Handler.httpRequestPathList   = requestPathList path})

formatResponseHeaders :: Map.Map String String -> String
formatResponseHeaders responseHeaders = let
        headerPairs           = Map.assocs responseHeaders
        headerStrings         = fmap concatHeaderNameValue headerPairs
        responseHeadersString = foldl (++) "" headerStrings
    in (responseHeadersString)

concatHeaderNameValue :: (String, String) -> String
concatHeaderNameValue (name, value) = name ++ ": " ++ value ++ "\r\n"

queryParams :: String -> Map.Map String String
queryParams line = if elem '?' line
        then let
                query       = (Split.splitOn "?" line) !! 1
                queryParts  = Split.splitOn "&" query
                queryTuples = fmap splitQueryParts queryParts
            in (Map.fromList queryTuples)
        else Map.fromList []

splitQueryParts :: String -> (String, String)
splitQueryParts queryPart = let
        subParts = Split.splitOn "=" queryPart
        name     = subParts !! 0
        value    = getQueryValue subParts
    in ((name, value))

getQueryValue :: [String] -> String
getQueryValue subParts = if (length subParts) == 1
    then ""
    else subParts !! 1

requestPath :: String -> String
requestPath line = head $ Split.splitOn "?" line

requestPathList :: String -> [String]
requestPathList path = Split.splitOn "/" path

requestMethod :: String -> Int
requestMethod method | method == "GET"  = fromEnum GET
                     | method == "POST" = fromEnum POST
                     | method == "HEAD" = fromEnum HEAD
                     | otherwise        = fromEnum UNSUPPORTED
