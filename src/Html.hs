module Html (base, loadTemplate) where

import qualified Data.List.Split as Split
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

replaceTemplateVar :: Template -> String -> Content -> HtmlOutput
replaceTemplateVar template variableName content = let
    variable = "{{" ++ variableName ++ "}}"
    parts = Split.splitOn variable template
    in ((head parts) ++ content ++ (last parts))

base :: Template -> Title -> Content -> HtmlOutput
base template title content = let
    templateWitTitleFilled = replaceTemplateVar template "title" title
    in (replaceTemplateVar templateWitTitleFilled "content" content)

directoryEntries :: Template -> Path -> [(FileName, IsDirectory)] -> HtmlOutput
directoryEntries template path entries = ""

directoryEntry :: Template -> Path -> FileName -> HtmlOutput
directoryEntry template path filename = ""

fileEntry :: Template -> FileName -> HtmlOutput
fileEntry template filename = ""

fileContent :: Template -> Path -> Content -> HtmlOutput
fileContent template path content = ""

requestTable :: Template -> Handler.HttpHeadersMap -> HtmlOutput
requestTable template headers = ""

requestTableEntry :: Template -> String -> String -> HtmlOutput
requestTableEntry template key value = ""

loadTemplate :: String -> IO (Maybe String)
loadTemplate templateName = do
    let seperator = [FilePath.pathSeparator]
    let templatePath = "templates" ++ seperator ++ templateName ++ ".html"
    filePath <- FileSystem.processPath templatePath
    FileSystem.fileContents filePath

loadTemplates :: IO (Maybe ServerTemplates)
loadTemplates = return Nothing
