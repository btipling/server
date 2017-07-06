module Server.Handler where

import qualified Data.Map.Strict as DMS

type RequestHandler = DMS.Map String String -> String