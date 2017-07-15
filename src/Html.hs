module Html (base, loadTemplate) where

import qualified Data.List.Split as Split
import qualified System.FilePath as FilePath
import qualified FileSystem

replaceTemplateVar :: String -> String -> String -> String
replaceTemplateVar template variableName content = let
    variable = "{{" ++ variableName ++ "}}"
    parts = Split.splitOn variable template
    in ((head parts) ++ content ++ (last parts))

base :: String -> String -> String -> String
base template title content = let
    templateWitTitleFilled = replaceTemplateVar template "title" title
    in (replaceTemplateVar templateWitTitleFilled "content" content)

loadTemplate :: String -> IO (Maybe String)
loadTemplate templateName = do
    let seperator = [FilePath.pathSeparator]
    let templatePath = "templates" ++ seperator ++ templateName ++ ".html"
    filePath <- FileSystem.processPath templatePath
    FileSystem.fileContents filePath
