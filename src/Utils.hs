module Utils (catchAny) where

import qualified Control.Exception as Exception

catchAny :: IO a -> (Exception.IOException -> IO a) -> IO a
catchAny = Exception.catch
