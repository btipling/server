module Html (base, loadTemplate) where

import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified System.FilePath as FilePath
import qualified FileSystem
import qualified Server.Handler as Handler

type Title = String
type Template = String
type Content = String
type HtmlOutput = String
type FileName = String
type Path = String
type IsDirectory = Bool

data ServerTemplates = ServerTemplates {
    baseTemplate              :: Title -> Content -> HtmlOutput,
    dirEntriesTemlate         :: Path -> [(FileName, IsDirectory)] -> HtmlOutput,
    dirEntryTemplate          :: Path -> FileName -> HtmlOutput,
    fileEntryTemplate         :: FileName -> HtmlOutput,
    fileContentTemplate       :: Path -> Content -> HtmlOutput,
    requestTableTemplate      :: Handler.HttpHeadersMap -> HtmlOutput,
    requestTableEntryTemplate :: String -> String -> HtmlOutput
}

replaceTemplateVar :: Template -> (String, Content) -> HtmlOutput
replaceTemplateVar template (variableName, content) = let
    variable = "{{" ++ variableName ++ "}}"
    parts = Split.splitOn variable template
    in ((head parts) ++ content ++ (last parts))

fillTemplate :: Template -> [(String, Content)] ->HtmlOutput
fillTemplate t keyValues = foldl (\acc kv -> replaceTemplateVar acc kv) t keyValues

base :: Template -> Title -> Content -> HtmlOutput
base t title content = fillTemplate t [("title", title), ("content", content)]

directoryEntries :: Template -> Template -> Template -> Path -> [(FileName, IsDirectory)] -> HtmlOutput
directoryEntries t dirTemplate fileTemplate path entries = let
    dt           = directoryEntry dirTemplate path
    ft           = fileEntry fileTemplate
    html         = fmap (\(name, isDir) -> if isDir then (dt name) else (ft name)) entries
    listContents = foldl (++) "" html
    in (fillTemplate t [("path", path), ("listContents", listContents)])

directoryEntry :: Template -> Path -> FileName -> HtmlOutput
directoryEntry t path dirName = fillTemplate t [("path", path), ("directoryName", dirName)]

fileEntry :: Template -> FileName -> HtmlOutput
fileEntry t fileName = fillTemplate t [("fileName", fileName)]

fileContent :: Template -> Path -> Content -> HtmlOutput
fileContent t path content = fillTemplate t [("path", path), ("content", content)]

headers :: Template -> Template -> Handler.HttpHeadersMap -> HtmlOutput
headers t entryTemplate headers = let
    headerKeyValues = Map.toList headers
    tableData       = foldl (\acc kv -> acc ++ (headerEntry entryTemplate kv)) "" headerKeyValues
    in (fillTemplate t [("tableData", tableData)])

headerEntry :: Template -> (String, String) -> HtmlOutput
headerEntry t (name, value) = fillTemplate t [("name", name), ("value", value)]

loadTemplate :: String -> IO (Maybe String)
loadTemplate templateName = do
    let seperator    = [FilePath.pathSeparator]
    let templatePath = "templates" ++ seperator ++ templateName ++ ".html"
    filePath         <- FileSystem.processPath templatePath
    FileSystem.fileContents filePath

loadTemplates :: IO (Maybe ServerTemplates)
loadTemplates = return Nothing
