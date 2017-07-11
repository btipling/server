module FileSystem.Directory (listContents, validate) where

import qualified System.Directory as Directory
import qualified System.IO.Error as Error
import qualified Control.Exception as Exception

validate :: String -> IO Bool
validate path = Directory.doesDirectoryExist path

listContents :: String -> IO (Maybe String)
listContents path = do
    c <- Exception.try $ Directory.listDirectory path  :: IO (Either Exception.IOException [String])
    case c of
        Left _ -> return Nothing
        Right entries -> return (Just (foldl (\acc entry -> acc ++ " \n -> " ++ entry) "" entries))
