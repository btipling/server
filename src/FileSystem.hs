module FileSystem (getPathContents, listContents, fileContents, processPath, validate) where

import qualified Control.Exception as Exception
import qualified Control.Monad     as Monad
import qualified Debug.Trace       as Trace
import qualified System.Directory  as Directory
import qualified System.FilePath   as FilePath
import qualified System.IO         as IO
import qualified System.IO.Error   as Error

validate :: String -> IO Bool
validate path = Directory.doesDirectoryExist path

listContents :: String -> IO (Maybe (Either [String] String))
listContents path = do
    c <- Exception.try $ Directory.listDirectory path :: IO (Either Exception.IOException [String])
    case c of
        Left _        -> return Nothing
        Right entries -> return $ Just $ Left entries

fileContents :: String -> IO (Maybe (Either [String] String))
fileContents path = do
    c <- Exception.try $ IO.readFile path :: IO (Either Exception.IOException String)
    case c of
        Left _         -> return Nothing
        Right contents -> return $ Just (Right contents)

getPathContents :: String -> [String] -> IO (Maybe (Either [String] String))
getPathContents rootPath pathList = let
    validList = filter notEmpty pathList
    in (case validList of
         []        -> listContents rootPath
         validList -> getPathContents' rootPath validList)

getPathContents' :: String -> [String] -> IO (Maybe (Either [String] String))
getPathContents' currentPath pathList = do
    if null pathList then listContents currentPath
    else do
        let nextPath     = currentPath ++ [FilePath.pathSeparator] ++ (head pathList)
        isValidDirectory <- Directory.doesDirectoryExist nextPath
        if isValidDirectory then getPathContents' nextPath (tail pathList)
        else do
            isValidFile <- Directory.doesFileExist nextPath
            if isValidFile then fileContents nextPath
            else return Nothing

notEmpty :: String -> Bool
notEmpty s = not $ null s

processPath :: String -> IO String
processPath relativePath = do
    currentDirectory <- Directory.getCurrentDirectory
    return $ currentDirectory ++ [FilePath.pathSeparator] ++ relativePath
