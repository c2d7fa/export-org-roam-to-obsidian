module Main where

import qualified System.Directory as Dir
import qualified Data.Org as O
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as L
import Data.List (stripPrefix)
import Data.Maybe (fromJust, maybe)

type Titles = M.Map String String

withDefault :: a -> Maybe a -> a
withDefault d = maybe d id

orgTitle :: O.OrgFile -> Maybe String
orgTitle org = T.unpack <$> M.lookup (T.pack "TITLE") (O.orgMeta org)

fileTitle :: String -> IO String
fileTitle path = do
  source <- readFile path
  let title = O.org (T.pack source) >>= orgTitle
  return $ withDefault path title

loadTitles :: String -> IO Titles
loadTitles dir = Dir.withCurrentDirectory dir $ do
  files <- Dir.listDirectory "."
  titles <- mapM (\file -> fileTitle file >>= (\title -> return (file, title))) files
  return $ M.fromList titles

fixLinks :: Titles -> O.OrgFile -> O.OrgFile
fixLinks titles org = O.OrgFile (O.orgMeta org) (fixInDoc $ O.orgDoc org)
  where 
    fixInDoc :: O.OrgDoc -> O.OrgDoc
    fixInDoc doc = O.OrgDoc (map fixInBlock $ O.docBlocks doc) (map fixInSection $ O.docSections doc)
    
    fixInBlock :: O.Block -> O.Block
    fixInBlock (O.Paragraph words) = O.Paragraph (fixInWords words)
    fixInBlock (O.List items) = O.List (fixInItems items)
    fixInBlock other = other

    fixInItems :: O.ListItems -> O.ListItems
    fixInItems (O.ListItems items) = O.ListItems $ L.map fixInItem items
    
    fixInItem :: O.Item -> O.Item
    fixInItem (O.Item words subitems) = O.Item (fixInWords words) (fixInItems <$> subitems)
    
    fixInSection :: O.Section -> O.Section
    fixInSection section = O.Section (fixInWords $ O.sectionHeading section) (O.sectionTags section) (fixInDoc $ O.sectionDoc section)
    
    fixInWords :: L.NonEmpty O.Words -> L.NonEmpty O.Words
    fixInWords = L.map fixInWord

    fixInWord :: O.Words -> O.Words
    fixInWord (O.Link (O.URL url) title) = O.Link (O.URL $ T.pack $ rewriteUrl $ T.unpack url) Nothing
    fixInWord other = other

    rewriteUrl :: String -> String
    rewriteUrl url = case stripPrefix "file:" url of
      Just file -> (withDefault (file ++ ".md") $ M.lookup file titles)
      Nothing -> url

inputDirectory = "input"
testFile = "20200520225959-test.org"

main :: IO ()
main = do
  titles <- loadTitles inputDirectory
  source <- readFile (inputDirectory ++ "/" ++ testFile)
  putStrLn $ T.unpack $ O.prettyOrgFile $ fixLinks titles $ fromJust $ O.org (T.pack source)
