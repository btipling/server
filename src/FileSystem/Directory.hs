module FileSystem.Directory (validate) where

import System.Directory as Directory

validate :: String -> IO Bool
validate path = Directory.doesDirectoryExist path

