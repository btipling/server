module Server.Http where

import qualified Server.Headers as Headers
import qualified Server.Handler as Handler

status :: Int -> String
status statusCode = "HTTP/1.1 " ++ (show statusCode) ++ "\n"

response :: String -> Handler.RequestHandler -> String
response rawRequest requestHandler = let
    requestHeaders = Headers.requestHeaders rawRequest
    handlerResponse = requestHandler requestHeaders
    c = Handler.content handlerResponse
    httpStatus = status $ Handler.status handlerResponse
    in (httpStatus ++ (Headers.responseHeaders c) ++ c)