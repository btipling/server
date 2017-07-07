module Server.Http where

import qualified Server.Headers as Headers
import qualified Server.Handler as Handler
import qualified Data.Map as Map
import Debug.Trace

status :: Int -> String
status statusCode = "HTTP/1.1 " ++ (show statusCode) ++ "\r\n"

response :: String -> Handler.RequestHandler -> String
response rawRequest requestHandler = let
    requestHeaders           = Headers.requestHeaders rawRequest
    handlerResponse          = requestHandler requestHeaders
    content                  = Handler.content handlerResponse
    httpStatus               = status $ Handler.status handlerResponse
    responseHeaders          = Headers.responseHeaders content
    formattedResponseHeaders = formatResponseHeaders responseHeaders
    finalResponse            = httpStatus ++ formattedResponseHeaders ++ "\r\n" ++ content
    in (finalResponse)

formatResponseHeaders :: Map.Map String String -> String
formatResponseHeaders responseHeaders = let
    headerPairs           = Map.assocs responseHeaders
    headerStrings         = fmap concatHeaderNameValue headerPairs
    responseHeadersString = foldl (++) "" headerStrings
    in (responseHeadersString)

concatHeaderNameValue :: (String, String) -> String
concatHeaderNameValue (name, value) = name ++ ": " ++ value ++ "\r\n"
