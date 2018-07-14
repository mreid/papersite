--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module SiteJournal where

--------------------------------------------------------------------------------
import            Author
-- import            Page

import            Control.Applicative ((<$>), (<|>), empty)
import            Control.Monad       (forM_, liftM, liftM3)
import			  Data.Char
import            Data.Function       (on)
import            Data.List           (foldl', intersperse, intercalate, sortBy, 
                                       elemIndex)
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
import qualified Text.Regex.XMLSchema.String as RE

-- Base URI for the W&CP site
baseURI = "http://jmlr.org/proceedings/papers/"

--------------------------------------------------------------------------------
realMain :: String -> IO ()
realMain regex = hakyllWith config $ do
  let onlyVols = fromRegex regex
  
  -- Papers are in, e.g., @db/v26/reid12b.bib
  match ("db-scraped/*/*.bib" .&&. onlyVols) $ do
    route $ 
      gsubRoute "db-scraped/" (const "") `composeRoutes` 
      setExtension "meta"

    compile $ do
      entryCompiler
        >>= saveSnapshot "test"
        >>= loadAndApplyTemplate "templates/scholar/paper-journal.html" entryContext
        -- >>= relativizeUrls

  -- Templates
  match "templates/scholar/*" $ 
    compile templateCompiler

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

-- Define a context for template fields using a given entry.
-- This context will look for a matching field in the Entry's BibTeX fields
-- if it is one of "title", "author", "abstract", or "pages".
-- If it is not one of these it will look up the associated conference entry
-- and look for the BibTeX field there.
entryContext :: Context Entry
entryContext = 
  constField "baseURI" baseURI     -- Base URI
  <> constField "booktitle" "Journal of Machine Learning Research"
  <> listField' "authors" authorContext (return . entryNames "author")
  <> field "journalvolume" (maybe empty return . entryVolumeIssue "volume")
  <> field "journalissue" (maybe empty return . entryVolumeIssue "issue")
  <> entryContext'      -- Fields from BibTeX entry

entryVolumeIssue :: String -> Item Entry -> Maybe String
entryVolumeIssue key entry = do 
  volField <- getField "volume" entry
  lookup key $ RE.matchSubex volIssueRE volField

volIssueRE = "({volume}\\d+)\\(({issue}.+)\\)"

entryNames :: String -> Item Entry -> [Item Author]
entryNames key = map (Item "") . maybe [] toAuthors . getField key

entryContext' :: Context Entry
entryContext' = Context $ 
  \key item -> liftM StringField (maybeGetField key item) 

-- Return a Compiler String for a looked-up value or the empty Compiler
-- If the key search for is a default key then log a warning and return a
-- placeholder value if it is missing
maybeGetField :: String -> Item Entry -> Compiler String
maybeGetField key  
 | key `elem` defaultKeys = maybe (defaultKeyWarning key) return . getField key
 | otherwise              = maybe empty return . getField key
  where
    defaultKeys = ["abstract", "title", "author", "pages", "year", "volume"]
    defaultKeyWarning key = 
      debugCompiler ("WARNING: Could not find " ++ key)
      >> return ("[Not found: " ++ key ++ "]")
      
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

--------------------------------------------------------------------------------
-- Miscellaneous functions
-- listField' should really be part of the Hakyll library.
listField' :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
listField' key c value = field' key $ fmap (ListField c) . value

field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k i -> if k == key then value i else empty
