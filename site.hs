--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import            Author
import            Control.Applicative ((<$>), (<|>), empty)
import            Control.Monad       (forM_, liftM, liftM3)
import			  Data.Char
import            Data.List           (foldl', intersperse, sortBy, elemIndex)
import            Data.List.HT        (chop, segmentBefore)
import            Data.Map            (keys, (!))
import            Data.Maybe
import            Data.Monoid         (mappend, (<>), mconcat)
import            Data.Ord            (comparing)
import            Data.String         (fromString)
import            Hakyll
import            Paper
import qualified  Text.BibTeX.Entry   as BibTex
import            Text.Pandoc
import            Text.Regex
import            System.FilePath

--------------------------------------------------------------------------------
-- TODO: Clean up and reorganise!

main :: IO ()
main = hakyllWith config $ do

  -- Load in the conference details for reference from paper entries
  match "db/*.bib" $ version "fields" $ do
    compile $ do
      entryCompiler 

  -- Compile conference details in, e.g., @db/v31.bib@ to HTML
  match "db/*.bib" $ version "html" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      gsubRoute ".bib" (const "/index.html")

    compile $ do 
      entryCompiler
        >>= (\conf -> do 
          let confID = itemIdentifier conf
          let pattern = fromGlob $ (dropExtension . toFilePath $ confID) ++ "/*.bib"
          papers <- pageSort <$> loadAllSnapshots pattern "test"

          let sectionOrd = comparing $ (flip elemIndex $ (sectionOrder conf)) . fromMaybe "none" .fst
          let sections = fmap (Item "") . sortBy sectionOrd $ makeSections [] papers

          let sectionCtx = sectionContext conf
          let sectionsCtx = listField "sections" sectionCtx (return sections) <> entryContext'
          
          loadAndApplyTemplate "templates/papers.html" sectionsCtx conf
        ) 
        >>= relativizeUrls

  -- Papers are in, e.g., @db/v26/reid12b.bib
  match "db/*/*.bib" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      setExtension "html"

    compile $ do
      entryCompiler
        >>= saveSnapshot "test"
        >>= loadAndApplyTemplate "templates/paper.html" entryContext
        >>= relativizeUrls

  -- All files (PDFs, Zip, BibTeX)
  match "db/*/*.*" $ do
	route $ gsubRoute "db/" (const "")
	compile copyFileCompiler

  -- Templates
  match "templates/**" $ 
    compile templateCompiler

  -- Javascript
  match "static/js/*.js" $ do
    route (gsubRoute "static/" (const ""))
    compile copyFileCompiler

  -- CSS
  match "static/css/*.css" $ do
    route (gsubRoute "static/" (const ""))
    compile copyFileCompiler

  -- Images
  match "static/img/*" $ do
    route (gsubRoute "static/" (const ""))
    compile copyFileCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "/tmp/jmlr/",
    storeDirectory       = "/tmp/hakyll_cache/jmlr/",
	deployCommand = 
      "rsync --checksum -avz --no-group /tmp/jmlr/* " 
        ++ "mreid@login.csail.mit.edu:"
        ++ "/afs/csail.mit.edu/group/jmlr/docroot/proceedings/papers/"
  }

-- FIXME: Make sure this can handle roman (e.g., "xvi") page numbers.
pageSort :: [Item Entry] -> [Item Entry]
pageSort = sortBy (\i1 i2 -> compare (page $ i1) (page $ i2))
  where
	page = (read :: String -> Int) . fromJust . (getField "firstpage")

-- Define a context for template fields using a given entry.
-- This context will look for a matching field in the Entry's BibTeX fields
-- if it is one of "title", "author", "abstract", or "pages".
-- If it is not one of these it will look up the associated conference entry
-- and look for the BibTeX field there.
entryContext :: Context Entry
entryContext = 
  constField "baseURI" "http://jmlr.org/proceedings/papers"
  <> listField' "authors" authorContext (return . entryAuthors)
  <> entryContext'
  -- <> suppContext

suppContext :: Context Entry
suppContext =
  field "supp-url"    (suppLookup filename)   <>
  field "supp-kind"   (suppLookup kind)       <>
  field "supp-name"   (suppLookup suppID)

suppLookup :: (Supplementary -> String) -> Item Entry -> Compiler String
suppLookup f = maybe empty (return . f) . entrySupplementary

entryAuthors :: Item Entry -> [Item Author]
entryAuthors = map (Item "") . maybe [] toAuthors . getField "author" 

-- FIXME: Parse supplementary string into list of (name,file)
toSupps :: String -> [(String,FilePath)]
toSupps str = undefined

entryContext' :: Context Entry
entryContext' = Context $ 
  \key item -> maybeGetField key item >>= return . StringField

-- Return a Compiler String for a looked-up value or the empty Compiler
maybeGetField :: String -> (Item Entry) -> Compiler String
maybeGetField key = maybe empty return . getField key

-- Compile an entry by parsing its associated BibTeX file
entryCompiler :: Compiler (Item Entry)
entryCompiler = getResourceString >>= makeItem . parseEntry . itemBody

-- Save a snapshot of the paper with snapshot name equal to its conference ID
saveEntryCompiler :: Item Entry -> Compiler (Item Entry)
saveEntryCompiler entry = saveSnapshot (entryConfPath entry) entry

-- Compute the path for the conference BibTeX file given the path for a paper
-- e.g., this takes "db/ICML/2012/reid12a.bib" to "db/ICML/2012.bib"
confPath :: FilePath -> FilePath
confPath path = (takeDirectory path) ++ ".bib"

entryConfPath :: Item Entry -> FilePath
entryConfPath = confPath . toFilePath . itemIdentifier 

-- Load the conference entry associated with the paper with the given ID.
conferenceEntry :: (Item Entry) -> Compiler (Item Entry)
conferenceEntry = load . setVersion (Just "fields") . fromFilePath . entryConfPath

--------------------------------------------------------------------------------
-- Sections in the proceedings
type Section = (Maybe String, [Item Entry])

sectionEntries :: Section -> [Item Entry]
sectionEntries = snd

sectionID :: Section -> String
sectionID = fromMaybe "none" . fst

sectionEntriesCompiler :: Compiler [Item Entry]
sectionEntriesCompiler = fmap sectionEntries (getUnderlying >>= loadBody)

-- Build an association list of sections
makeSections :: [Section] -> [Item Entry] -> [Section] 
makeSections = foldl' addToSection

-- Adds an entry to the association list according to its section.
-- The entry is added to the section "none" if it has no section field.
addToSection :: [Section] -> (Item Entry) -> [Section]
addToSection [] entry = [(getField "section" entry, [entry])]
addToSection ((sec, es):rest) entry
  | sec == getField "section" entry   = (sec, es ++ [entry]):rest
  | True                              = (sec, es):(addToSection rest entry)

-- Build a context for a section using the given titling function.
--    section = title of section
--    papers  = rendered list of papers
sectionContext :: Item Entry -> Context Section
sectionContext conf =
  field     "sectionid"   (return . secID)              <>
  field     "section"     (return . (sectionTitles conf) . secID)   <>
  listField' "papers"     entryContext (return . sectionEntries . itemBody)
  where
    secID = sectionID . itemBody

-- Build a function that mas sections keywords to their corresponding titles
-- by parsing a "sections" field of a conference that has the form
--	  key1=Title Number 1|key2=Title Number Two|none=Default Title
sectionTitles :: Item Entry -> String -> String
sectionTitles entry = case getField "sections" entry of
    Nothing     -> \_ -> ""
    Just val    -> \key -> fromMaybe "" . lookup key . convert $ val
    where 
	  tuplify [x,y] = (x,y)
	  convert = map tuplify . parseSections

-- Get the section IDs from the sections field in the order they appear
sectionOrder :: Item Entry -> [String]
sectionOrder = maybe [] (map head . parseSections) . (getField "sections")

parseSections :: String -> [[String]]
parseSections = map (chop (=='=')) . chop (=='|')

--------------------------------------------------------------------------------
-- Data type for the supplementary contents for a paper
data Supplementary = Supplementary {
  suppID :: String,
  filename :: FilePath
}
kind supp = map toUpper (tail . takeExtension . filename $ supp)

suppDefRegex = mkRegex "^[ ]*([a-z|A-Z|0-9]+):(.*)[ ]*$"

parseSupplementary :: String -> Maybe Supplementary
parseSupplementary str = case matchRegex suppDefRegex str of
  Just [name, filename]	  -> Just (Supplementary name filename)
  Just _				  -> Nothing
  Nothing				  -> Nothing

entrySupplementary :: Item Entry -> Maybe Supplementary
entrySupplementary item = 
  getField "supplementary" item >>= parseSupplementary

--------------------------------------------------------------------------------
-- Miscellaneous functions
-- listField' should really be part of the Hakyll library.
listField' :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
listField' key c value = field' key $ fmap (ListField c) . value

field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k i -> if k == key then value i else empty
