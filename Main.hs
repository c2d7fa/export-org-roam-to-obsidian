module Main where

import qualified System.Directory as Dir
import qualified Data.Org as O
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as L
import Data.List (stripPrefix, intercalate)
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
    fixInWord (O.Link (O.URL url) title) = O.Link (O.URL $ T.pack $ rewriteUrl $ T.unpack url) title
    fixInWord other = other

    rewriteUrl :: String -> String
    rewriteUrl url = case stripPrefix "file:" url of
      Just file -> (withDefault (file ++ ".md") $ M.lookup file titles)
      Nothing -> url

markdown :: O.OrgFile -> String
markdown org = mdDoc $ O.orgDoc org
  where
    mdDoc :: O.OrgDoc -> String
    mdDoc doc = ps [mdBlocks (O.docBlocks doc), mdSections (O.docSections doc)]

    mdBlocks :: [O.Block] -> String
    mdBlocks = ps . map mdBlock

    mdBlock :: O.Block -> String
    mdBlock (O.Quote text) = "> " ++ T.unpack text
    mdBlock (O.Example text) = "```\n" ++ T.unpack text ++ "\n```"
    mdBlock (O.Code lang text) = "```" ++ (withDefault "" ((\(O.Language lang) -> T.unpack lang) <$> lang)) ++ "\n" ++ T.unpack text ++ "\n"
    mdBlock (O.List items) = mdListItems items
    mdBlock (O.Table rows) = "(org-roam-to-obsidian: Error: Unsupported element: Table)"
    mdBlock (O.Paragraph words) = mdWords words

    mdListItems :: O.ListItems -> String
    mdListItems = mdListItems' 0

    mdListItems' :: Int -> O.ListItems -> String
    mdListItems' level (O.ListItems items) = intercalate "\n" . L.toList $ L.map (mdListItem level) items

    mdListItem :: Int -> O.Item -> String
    mdListItem level (O.Item words subitems) = replicate (level * 2) ' ' ++ "- " ++ mdWords words ++ mdSubitems subitems
      where mdSubitems Nothing = ""
            mdSubitems (Just items) = "\n" ++ mdListItems' (level + 1) items

    mdSections :: [O.Section] -> String
    mdSections = concat . map mdSection

    mdSection :: O.Section -> String
    mdSection section = ps ["## " ++ mdWords (O.sectionHeading section), mdDoc (O.sectionDoc section)]

    mdWords :: L.NonEmpty O.Words -> String
    mdWords = joinWords . L.toList . L.map mdWord

    joinWords :: [String] -> String
    joinWords [] = ""
    joinWords [w] = w
    joinWords ("(" : ws) = "(" ++ joinWords ws
    joinWords (w : ")" : ws) = w ++ joinWords (")" : ws)
    joinWords (w : "," : ws) = w ++ joinWords ("," : ws)
    joinWords (w : "." : ws) = w ++ joinWords ("." : ws)
    joinWords (w : ";" : ws) = w ++ joinWords (";" : ws)
    joinWords (w : ":" : ws) = w ++ joinWords (":" : ws)
    joinWords (w : "!" : ws) = w ++ joinWords ("!" : ws)
    joinWords (w : "?" : ws) = w ++ joinWords ("?" : ws)
    joinWords (w : "'" : w' : ws) = w ++ "'" ++ joinWords (w' : ws)
    joinWords (w : ws) = w ++ " " ++ joinWords ws

    mdWord :: O.Words -> String
    mdWord (O.Bold text) = "**" ++ T.unpack text ++ "**"
    mdWord (O.Italic text) = "*" ++ T.unpack text ++ "*"
    mdWord (O.Highlight text) = "==" ++ T.unpack text ++ "=="
    mdWord (O.Underline text) = "__" ++ T.unpack text ++ "__"
    mdWord (O.Verbatim text) = "`" ++ T.unpack text ++ "`"
    mdWord (O.Strike text) = "~~" ++ T.unpack text ++ "~~"
    mdWord (O.Link (O.URL url) title) =
      case title of
        Nothing -> T.unpack url
        Just title ->
          if T.unpack url == T.unpack title then
            "[[" ++ T.unpack url ++ "]]"
          else
            "[" ++ T.unpack title ++ "](" ++ T.unpack url ++ ")"
            -- TODO: Handle local links with titles
    mdWord (O.Image (O.URL url)) = "![" ++ T.unpack url ++ "]"
    mdWord (O.Tags _) = ""
    mdWord (O.Punct ch) = [ch]
    mdWord (O.Plain text) = T.unpack text

    ps :: [String] -> String
    ps paragraphs = intercalate "\n\n" paragraphs

inputDirectory = "input"
testFile = "20200520225959-test.org"

main :: IO ()
main = do
  titles <- loadTitles inputDirectory
  source <- readFile (inputDirectory ++ "/" ++ testFile)
  putStrLn $ markdown $ fixLinks titles $ fromJust $ O.org (T.pack source)
