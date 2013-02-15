--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import            Author
import            Control.Applicative ((<$>))
import            Control.Monad       (forM_, liftM)
import            Data.List           (intersperse, sortBy)
import            Data.List.HT        (segmentBefore)
import            Data.Map            (keys, (!))
import            Data.Maybe
import            Data.Monoid         (mappend)
import            Data.String         (fromString)
import            Hakyll
import            Paper
import            Text.Pandoc
import qualified  Text.BibTeX.Entry   as BibTex
import            System.FilePath

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
  -- Compile BibTeX information for each conference and save as an Entry
  match "db/*.bib" $ do
    compile $ 
      entryCompiler >>= saveSnapshot "conference"

  -- Compile conference details in, e.g., @db/conf/ICML/2012.bib@ to HTML
  match "db/*.bib" $ version "html" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      gsubRoute ".bib" (const "/index.html")

    compile $ do 
      confID <- getUnderlying
      let pattern = fromGlob $ (dropExtension . toFilePath $ confID) ++ "/*.bib"
      papers <- firstPageSort <$> (loadAllSnapshots (pattern `withVersion` "entry") $ toFilePath confID)

      linkTpl     <- loadBody "templates/paper-item.html"
      paperLinks  <- applyTemplateList linkTpl entryContext papers

      let papersCtx =
            constField "title"  "All Papers"  `mappend`
            constField "papers" paperLinks    `mappend`
            conferenceContext 

      entryCompiler
        >>= loadAndApplyTemplate "templates/papers.html" papersCtx
        >>= loadAndApplyTemplate "templates/default.html" 
              (constField "metadata" "" `mappend` defaultContext) 
        >>= relativizeUrls

  -- Compile each paper BibTeX to an Entry and save
  match "db/*/*.bib" $ version "entry" $ do
    compile $ 
      entryCompiler >>= saveEntryCompiler

  -- Papers are in, e.g., @db/conf/ICML/2012/reid12b.bib
  match "db/*/*.bib" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      setExtension "html"

    compile $ do
      entry      <- entryCompiler
      -- FIXME: This is ugly
      let authors = fmap (Item "") $ toAuthors . fromJust . getField "author" . itemBody $ entry

      authorTpl  <- loadBody "templates/scholar/author.html"
      authorMeta <- applyTemplateList authorTpl authorContext authors


      let entryContextWithAuthors = 
            constField "authors" authorMeta `mappend`
			constField "baseURI" "http://jmlr.csail.mit.edu/proceedings/papers" `mappend`
            entryContext
            
      metaTpl    <- loadBody "templates/scholar/paper.html" 
      meta       <- applyTemplateList metaTpl entryContextWithAuthors [entry]
	  

      let metaContext = 
			constField "metadata" meta `mappend` 
			defaultContext

      -- suppTpl	 <- loadBody "templates/supplementary.html"
	   
      -- let suppContext = 
			-- entryContext

      entryCompiler
        >>= loadAndApplyTemplate "templates/paper.html" entryContext
        >>= loadAndApplyTemplate "templates/default.html" metaContext 

  -- All files (PDFs, Zip, BibTeX)
  match "db/*/*.*" $ do
	route $ gsubRoute "db/" (const "")
	compile copyFileCompiler

  -- Supplementary files
  -- match "db/*/supplementary/*" $ do
  --   route $ gsubRoute "db/" (const "")
  --   compile copyFileCompiler

  -- Templates
  match "templates/**" $ 
    compile templateCompiler

  -- Static HTML
  match "static/*.html" $ do
    route (gsubRoute "static/" (const ""))
    compile $ do
        let metaContext = constField "metadata" "" `mappend` defaultContext

        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" metaContext
          >>= relativizeUrls

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
  { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
            \_site/* mreid@login.csail.mit.edu:"
  }

--------------------------------------------------------------------------------
-- | Set a field of a page to a listing of pages
joinTemplateList :: Template
          -> Context a
          -> [Item a]
          -> String
          -> Compiler String
joinTemplateList tpl context items delimiter = do
  items' <- mapM (applyTemplate tpl context) items
  return $ concat $ intersperse delimiter $ map itemBody items'


--------------------------------------------------------------------------------
firstPageSort :: [Item Entry] -> [Item Entry]
firstPageSort = sortBy (\i1 i2 -> compare (firstPage $ itemBody i1) (firstPage $ itemBody i2))
  where
	firstPage e = (read . fromJust $ (getField "firstpage" e)) :: Int

suppContext :: Context Entry
suppContext = Context $ \key item ->
  return $ case (getField "supplementary" . itemBody $ item) of
	Just suppStr  -> ""
	Nothing		  -> ""

conferenceContext :: Context Entry
conferenceContext = Context $ \key item ->
  return $ case (getField key . itemBody $ item) of
    Just value -> value
    Nothing    -> "FIXME"

-- Define a context for template fields using a given entry.
-- This context will look for a matching field in the Entry's BibTeX fields
-- if it is one of "title", "author", "abstract", or "pages".
-- If it is not one of these it will look up the associated conference entry
-- and look for the BibTeX field there.
entryContext :: Context Entry
entryContext = Context $ \key item ->
  let entry = itemBody item
      conf  = conferenceEntry . itemIdentifier $ item
  in entryLookup entry conf key

entryLookup :: Entry -> Compiler (Item Entry) -> String -> Compiler String
entryLookup entry conf key 
  | key `elem` paperFields   = return $ fromJust $ getField key entry
  | key `elem` confFields    = fmap (fromJust . getField key . itemBody) conf
  | True                     = return $ "FIXME"
  where
    paperFields = ["identifier", "title", "author", "abstract", "pages", "firstpage", "lastpage", "url", "pdf"]
    confFields  = ["booktitle", "volume", "year", "editor", "shortname"]

-- Compile an entry by parsing its associated BibTeX file
entryCompiler :: Compiler (Item Entry)
entryCompiler = getResourceString >>= makeItem . parseEntry . itemBody

-- Save a snapshot of the paper with snapshot name equal to its conference ID
saveEntryCompiler :: Item Entry -> Compiler (Item Entry)
saveEntryCompiler paper = saveSnapshot conferenceID paper
  where
    conferenceID = confBib . toFilePath . itemIdentifier $ paper

-- Compute the path for the conference BibTeX file given the path for a paper
-- e.g., this takes "db/ICML/2012/reid12a.bib" to "db/ICML/2012.bib"
confBib :: FilePath -> FilePath
confBib path = (takeDirectory path) ++ ".bib"

-- Load the conference entry associated with the paper with the given ID.
conferenceEntry :: Identifier -> Compiler (Item Entry)
conferenceEntry paperID =
  let confID = fromFilePath . confBib . toFilePath $ paperID
  in loadSnapshot confID "conference"


