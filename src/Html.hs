module Html (fillTemplate, loadTemplates, ServerTemplates) where

import qualified Data.List.Split as Split
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.FilePath as FilePath
import qualified FileSystem
import qualified Server.Handler as Handler

type Template = String
type Content = String
type HtmlOutput = String
type FileName = String
type Path = String
type IsDirectory = Bool
type ServerTemplates = Map.Map String String

replaceTemplateVar :: Template -> (String, Content) -> HtmlOutput
replaceTemplateVar template (variableName, content) = let
    variable = "{{" ++ variableName ++ "}}"
    parts = Split.splitOn variable template
    in ((head parts) ++ content ++ (last parts))

fillTemplate :: Template -> [(String, Content)] ->HtmlOutput
fillTemplate t keyValues = foldl (\acc kv -> replaceTemplateVar acc kv) t keyValues

directoryEntries :: Template -> Template -> Template -> Path -> [(FileName, IsDirectory)] -> HtmlOutput
directoryEntries t dirTemplate fileTemplate path entries = let
    dt           = fillTemplate dirTemplate
    ft           = fillTemplate fileTemplate
    p            = ("path", path)
    html         = fmap (\(name, isDir) -> if isDir then (dt [p, ("name", name)]) else (ft [("name", name)])) entries
    listContents = foldl (++) "" html
    in (fillTemplate t [("path", path), ("listContents", listContents)])

headers :: Template -> Template -> Handler.HttpHeadersMap -> HtmlOutput
headers t entryTemplate headers = let
    headerKeyValues = Map.toList headers :: [(String, String)]
    entryTemp       = fillTemplate entryTemplate
    tableData       = foldl (\acc kv -> acc ++ (entryTemp [kv])) "" headerKeyValues
    in (fillTemplate t [("tableData", tableData)])

loadTemplate :: String -> IO (Maybe String)
loadTemplate templateName = do
    let seperator    = [FilePath.pathSeparator]
    let templatePath = "templates" ++ seperator ++ templateName ++ ".html"
    filePath         <- FileSystem.processPath templatePath
    FileSystem.fileContents filePath

loadTemplates :: IO (Maybe ServerTemplates)
loadTemplates = do
    loadedMaybeTemplates <- sequence $ fmap loadTemplate templates :: IO [Maybe String]
    loadedTemplates      <- return $ sequence loadedMaybeTemplates :: IO (Maybe [String])
    case loadedTemplates of
        Nothing -> return Nothing
        Just t  -> return $ Just $ Map.fromList $ zip templates t

loadedTemplate :: Bool -> Maybe String -> Bool
loadedTemplate prev t = case prev of
        False  -> False
        True -> Maybe.isNothing t

templates :: [String]
templates = [
    "base",
    "directoryEntries",
    "directoryEntry",
    "fileContent",
    "fileEntry",
    "headerTable",
    "headerTableRow"]
