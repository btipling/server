module FileSystem (getPathContents, listContents, fileContents, processPath, validate) where

import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Error as Error
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Debug.Trace as Trace
import qualified System.FilePath as FilePath

validate :: String -> IO Bool
validate path = Directory.doesDirectoryExist path

listContents :: String -> IO (Maybe String)
listContents path = do
    c <- Exception.try $ Directory.listDirectory path :: IO (Either Exception.IOException [String])
    case c of
        Left _        -> return Nothing
        Right entries -> return (Just (foldl (\acc entry -> acc ++ " \n -> " ++ entry) "" entries))

fileContents :: String -> IO (Maybe String)
fileContents path = do
    c <- Exception.try $ IO.readFile path :: IO (Either Exception.IOException String)
    case c of
        Left _         -> return Nothing
        Right contents -> return $ Just contents

getPathContents :: String -> [String] -> IO (Maybe String)
getPathContents rootPath pathList = let
    validList = filter notEmpty pathList
    in (case validList of
         []        -> listContents rootPath
         validList -> getPathContents' rootPath validList)

getPathContents' :: String -> [String] -> IO (Maybe String)
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
