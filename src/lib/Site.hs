--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Site where

--------------------------------------------------------------------------------
import            Author
import            Page

import            Control.Applicative ((<$>), empty)
import            Control.Monad       (liftM)
import			  Data.Char
import            Data.Function       (on)
import            Data.List           (foldl', sortBy, elemIndex)
import            Data.List.HT        (chop)
import            Data.Maybe
import            Data.Monoid         ((<>))
import            Data.Ord            (comparing)
import            Data.Time.Format    (formatTime, parseTime)
import            Data.Time.Calendar  (Day)
import            Hakyll
import            Paper
import            Text.Regex
import            System.FilePath
import            System.Locale       (defaultTimeLocale)

-- Base URI for the W&CP site
baseURI :: String
baseURI = "http://jmlr.org/proceedings/papers/"

createCompiler :: Compiler (Item String)
createCompiler = makeItem ""

--------------------------------------------------------------------------------
realMain :: String -> IO ()
realMain regex = hakyllWith config $ do
  let onlyVols = fromRegex regex

  -- Load in the conference details for reference from paper entries
  match ("db/*.bib" .&&. onlyVols) $ version "volume" $ 
    compile  
      entryCompiler 

  -- Compile conference details in, e.g., @db/v31.bib@ to HTML
  match ("db/*.bib" .&&. onlyVols) $ version "html" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      gsubRoute ".bib" (const "/index.html")

    compile $
      entryCompiler
        >>= saveSnapshot "volumes"
        >>= (\conf -> do 
          let confID = itemIdentifier conf
          let pattern = fromGlob $ (dropExtension . toFilePath $ confID) ++ "/*.bib"
          papers <- pageSort <$> loadAllSnapshots pattern "test"

          let sectionOrd = comparing $ flip elemIndex (sectionOrder conf) . fromMaybe "default" .fst
          let sections = fmap (Item "") . sortBy sectionOrd $ makeSections [] papers

          let sectionCtx = sectionContext conf
          let editorNames = entryNames "editor" conf
          let sectionsCtx = listField "sections" sectionCtx (return sections) 
                            <> listField "editors" authorContext (return editorNames)
                            <> entryContext'
          
          loadAndApplyTemplate "templates/papers.html" sectionsCtx conf
        ) 
        >>= relativizeUrls

  -- Papers are in, e.g., @db/v26/reid12b.bib
  match ("db/*/*.bib" .&&. onlyVols) $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      setExtension "html"

    compile $ 
      entryCompiler
        >>= saveSnapshot "test"
        >>= loadAndApplyTemplate "templates/paper.html" entryContext
        >>= relativizeUrls

  -- -- match "db/*/*.bib" $ version "bib" $ do
  -- --   route $ gsubRoute "db/" (const "")
  -- --   compile copyFileCompiler

  -- -- All files (PDFs, Zip, BibTeX)
  -- -- match ("db/*/*.*" .&&. onlyVols) $ do
	-- -- route $ gsubRoute "db/" (const "")
	-- -- compile copyFileCompiler

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
   
  -- Top level index of all volumes
  create ["index.html"] $ do
    route idRoute
    compile $ do
      volumes <- sortBy (compare `on` getField "published") 
                 <$> loadAll ("db/*.bib" .&&. hasVersion "volume")

      let volumeCtx = listField "volumes" volumeContext (return volumes)
      createCompiler
        >>= loadAndApplyTemplate "templates/index.html" volumeCtx
        >>= relativizeUrls
  
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "/tmp/jmlr/",
    storeDirectory       = "/tmp/hakyll_cache/jmlr/",
	deployCommand = 
      "rsync --checksum -avz --exclude '*.pdf' --exclude '*.zip' --exclude '*.gz' --rsh='ssh -p1022 -i /Users/mreid/.ssh/id_rsa ' --no-group /tmp/jmlr/* " 
        ++ "confla@mark.reid.name:"
        ++ "/home/confla/www/pmlr.cc/"
  }

-- Sorts the entries by first page, taking into account roman numerals
-- indicating prefaces.
pageSort :: [Item Entry] -> [Item Entry]
pageSort = sortBy (compare `on` page) -- (\i1 i2 -> compare (page i1) (page i2))
  where
	page = orderValue . fromJust . firstPage . getT . itemBody

-- Define a context for template fields using a given entry.
-- This context will look for a matching field in the Entry's BibTeX fields
-- if it is one of "title", "author", "abstract", or "pages".
-- If it is not one of these it will look up the associated conference entry
-- and look for the BibTeX field there.
entryContext :: Context Entry
entryContext = 
  constField "baseURI" baseURI     -- Base URI
  <> listFieldWith "authors" authorContext (return . entryNames "author")
  <> entryContext'      -- Fields from BibTeX entry
  <> confContext        -- Fields from parent conference's BibTeX entry
  <> suppContext        -- Fields for entry's supplementary items, if found

suppContext :: Context Entry
suppContext =
  field "supp-url"    (suppLookup filename)   <>
  field "supp-kind"   (suppLookup kind)       <>
  field "supp-name"   (suppLookup suppID)

-- Get entry fields for a Volume plus a human readable published date.
-- TODO: Fix hacky code for parsing and formatting date
volumeContext :: Context Entry
volumeContext =
  entryContext'
  <> field "vol-published"   (return . formatter . fromJust . getField "published")
  where
    formatter = (formatTime defaultTimeLocale "%e %B %Y") . fromJust .  
                (parseTime defaultTimeLocale "%F" :: String -> Maybe Day)

suppLookup :: (Supplementary -> String) -> Item Entry -> Compiler String
suppLookup f = maybe empty (return . f) . entrySupplementary

entryNames :: String -> Item Entry -> [Item Author]
entryNames key = map (Item "") . maybe [] toAuthors . getField key

-- FIXME: Parse supplementary string into list of (name,file)
toSupps :: String -> [(String,FilePath)]
toSupps str = undefined

entryContext' :: Context Entry
entryContext' = Context $ 
  \key _ item -> liftM StringField (maybeGetField key item)

confContext :: Context Entry
confContext = Context $
  \key _ item -> liftM StringField (conferenceEntry item >>= maybeGetField key)

-- Return a Compiler String for a looked-up value or the empty Compiler
-- If the key search for is a default key then log a warning and return a
-- placeholder value if it is missing
maybeGetField :: String -> Item Entry -> Compiler String
maybeGetField key  
 | key `elem` defaultKeys = maybe (defaultKeyWarning key) return . getField key
 | otherwise              = maybe empty return . getField key
  where
    defaultKeys = ["abstract", "title", "author", "pages"]
    defaultKeyWarning key' = 
      debugCompiler ("WARNING: Could not find " ++ key')
      >> return ("[Not found: " ++ key' ++ "]")

maybeFieldWith :: String -> 

-- Compile an entry by parsing its associated BibTeX file
entryCompiler :: Compiler (Item Entry)
entryCompiler = getResourceString >>= makeItem . parseEntry . itemBody

-- Save a snapshot of the paper with snapshot name equal to its conference ID
saveEntryCompiler :: Item Entry -> Compiler (Item Entry)
saveEntryCompiler entry = saveSnapshot (entryConfPath entry) entry

-- Compute the path for the conference BibTeX file given the path for a paper
-- e.g., this takes "db/ICML/2012/reid12a.bib" to "db/ICML/2012.bib"
confPath :: FilePath -> FilePath
confPath path = takeDirectory path ++ ".bib"

entryConfPath :: Item Entry -> FilePath
entryConfPath = confPath . toFilePath . itemIdentifier 

-- Load the conference entry associated with the paper with the given ID.
conferenceEntry :: Item Entry -> Compiler (Item Entry)
conferenceEntry = load . setVersion (Just "volume") . fromFilePath . entryConfPath

--------------------------------------------------------------------------------
-- Sections in the proceedings
type Section = (Maybe String, [Item Entry])

sectionEntries :: Section -> [Item Entry]
sectionEntries = snd

sectionID :: Section -> String
sectionID = fromMaybe "default" . fst

sectionEntriesCompiler :: Compiler [Item Entry]
sectionEntriesCompiler = fmap sectionEntries (getUnderlying >>= loadBody)

-- Build an association list of sections
makeSections :: [Section] -> [Item Entry] -> [Section] 
makeSections = foldl' addToSection

-- Adds an entry to the association list according to its section.
-- The entry is added to the section "default" if it has no section field.
addToSection :: [Section] -> Item Entry -> [Section]
addToSection [] entry = [(getField "section" entry, [entry])]
addToSection ((sec, es):rest) entry
  | sec == getField "section" entry   = (sec, es ++ [entry]):rest
  | otherwise                         = (sec, es):addToSection rest entry

-- Build a context for a section using the given titling function.
--    section = title of section
--    papers  = rendered list of papers
sectionContext :: Item Entry -> Context Section
sectionContext conf =
  field         "sectionid"   (return . secID)              <>
  field         "section"     (return . sectionTitles conf . secID)   <>
  listFieldWith "papers"     entryContext (return . sectionEntries . itemBody) 
  where
    secID = sectionID . itemBody

-- Build a function that maps sections keywords to their corresponding titles
-- by parsing a "sections" field of a conference that has the form
--	  key1=Title Number 1|key2=Title Number Two|default=Default Title
sectionTitles :: Item Entry -> String -> String
sectionTitles entry = case getField "sections" entry of
    Nothing     -> const "Accepted Papers"
    Just val    -> \key -> fromMaybe "Accepted Papers" . lookup key . convert $ val
    where 
	  tuplify [x,y] = (x,y)
	  convert = map tuplify . parseSections

-- Get the section IDs from the sections field in the order they appear
sectionOrder :: Item Entry -> [String]
sectionOrder = maybe [] (map head . parseSections) . getField "sections"

parseSections :: String -> [[String]]
parseSections = map (chop (=='=')) . chop (=='|')

--------------------------------------------------------------------------------
-- Data type for the supplementary contents for a paper
data Supplementary = Supplementary {
  suppID :: String,
  filename :: FilePath
} deriving (Show, Eq)

kind :: Supplementary -> String
kind supp = map toUpper (tail . takeExtension . filename $ supp)

suppDefRegex :: Regex
suppDefRegex = mkRegex "^[ ]*([a-z|A-Z|0-9]+):(.*)[ ]*$"

parseSupplementary :: String -> Maybe Supplementary
parseSupplementary str = case matchRegex suppDefRegex str of
  Just [suppname, file]	  -> Just (Supplementary suppname file)
  Just _				  -> Nothing
  Nothing				  -> Nothing

entrySupplementary :: Item Entry -> Maybe Supplementary
entrySupplementary item = 
  getField "supplementary" item >>= parseSupplementary

--------------------------------------------------------------------------------
-- Miscellaneous functions
-- listField' should really be part of the Hakyll library.
-- listField' :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
-- listField' key c value = field' key $ fmap (ListField c) . value

-- field' :: String -> (Item a -> Compiler ContextField) -> Context a
-- field' key value = Context $ \k i -> if k == key then value i else empty
