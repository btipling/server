module Html (directoryEntries, fillTemplate, headers, loadTemplates, ServerTemplates) where

import qualified Control.Monad   as Monad
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe
import qualified FileSystem
import qualified Server.Handler  as Handler
import qualified System.FilePath as FilePath

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
    parts    = Split.splitOn variable template
    in ((head parts) ++ content ++ (last parts))

fillTemplate :: Template -> [(String, Content)] ->HtmlOutput
fillTemplate t keyValues = foldl (\acc kv -> replaceTemplateVar acc kv) t keyValues

directoryEntries :: Template -> Template -> Path -> [FileName] -> HtmlOutput
directoryEntries t dirTemplate path entries = let
    dt           = fillTemplate dirTemplate
    p            = if (last path) == '/' then path else path ++ "/"
    entryToHTML  = \name -> dt [("path", p ++ name), ("directoryName", name)]
    html         = fmap entryToHTML entries
    listContents = foldl (++) "" html
    in (fillTemplate t [("path", path), ("listContents", listContents)])

headers :: Template -> Template -> Handler.HttpHeadersMap -> HtmlOutput
headers t entryTemplate headers = let
    entryValues = fmap (\(k, v) -> [("name", k), ("value", v)]) (Map.toList headers) :: [[(String, String)]]
    entryTemp       = fillTemplate entryTemplate
    tableData       = foldl (\acc kv -> acc ++ (entryTemp kv)) "" entryValues
    in (fillTemplate t [("tableData", tableData)])

loadTemplate :: String -> IO (Maybe String)
loadTemplate templateName = do
    let seperator    = [FilePath.pathSeparator]
    let templatePath = "templates" ++ seperator ++ templateName ++ ".html"
    filePath         <- FileSystem.processPath templatePath
    c                <- FileSystem.fileContents filePath
    case c of
        Nothing               -> return Nothing
        Just (Left _)         -> return Nothing
        Just (Right contents) -> return $ Just contents

loadTemplates :: IO (Maybe ServerTemplates)
loadTemplates = do
    loadedMaybeTemplates <- sequence $ fmap loadTemplate templates :: IO [Maybe String]
    loadedTemplates      <- return $ sequence loadedMaybeTemplates :: IO (Maybe [String])
    case loadedTemplates of
        Nothing -> return Nothing
        Just t  -> return $ Just $ Map.fromList $ zip templates t

loadedTemplate :: Bool -> Maybe String -> Bool
loadedTemplate prev t = case prev of
        False -> False
        True  -> Maybe.isNothing t

templates :: [String]
templates = [
    "base",
    "directoryEntries",
    "directoryEntry",
    "fileContent",
    "headerTable",
    "headerTableRow"]
