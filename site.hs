--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import            Author
import            Control.Applicative ((<$>), (<|>), empty)
import            Control.Monad       (forM_, liftM, liftM3)
import			  Data.Char
import            Data.List           (foldl', intersperse, sortBy)
import            Data.List.HT        (chop, segmentBefore)
import            Data.Map            (keys, (!))
import            Data.Maybe
import            Data.Monoid         (mappend, (<>), mconcat)
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
  -- Compile BibTeX information for each conference and save as an Entry
  match "db/*.bib" $ do
    compile $ 
      entryCompiler >>= saveSnapshot "conference"

  -- Compile conference details in, e.g., @db/v31.bib@ to HTML
  match "db/*.bib" $ version "html" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      gsubRoute ".bib" (const "/index.html")

    compile $ do 
      confID <- getUnderlying
      conf   <- entryCompiler

      let pattern = fromGlob $ (dropExtension . toFilePath $ confID) ++ "/*.bib"
      papers <- pageSort 
                <$> (loadAllSnapshots (pattern .&&. hasVersion "entry") 
                    $ toFilePath confID)

      let titles = sectionTitles conf
      let sections = fmap (Item "") $ makeSections [] papers
      let sectionsCtx = 
            templateField "sections" "templates/section.html" (sectionContext titles) sections
            <> conferenceContext

      entryCompiler
        >>= loadAndApplyTemplate "templates/papers.html" sectionsCtx
        >>= loadAndApplyTemplate "templates/default.html" 
              (constField "metadata" "" 
			  <> constField "title"  "All Papers"
			  <> defaultContext) 
        >>= relativizeUrls

  -- Compile each paper BibTeX to an Entry and save
  match "db/*/*.bib" $ version "entry" $ do
    compile $ 
      entryCompiler >>= saveEntryCompiler

  -- Papers are in, e.g., @db/v26/reid12b.bib
  match "db/*/*.bib" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      setExtension "html"

    compile $ do
      entry <- entryCompiler

      let metaContext = 
		  defaultContext
		  <> templateField "metadata" "templates/scholar/paper.html" 
              entryContext [entry]  

      entryCompiler
        >>= loadAndApplyTemplate "templates/paper.html" entryContext
        >>= loadAndApplyTemplate "templates/default.html" metaContext 
        >>= relativizeUrls

  -- All files (PDFs, Zip, BibTeX)
  match "db/*/*.*" $ do
	route $ gsubRoute "db/" (const "")
	compile copyFileCompiler

  -- Templates
  match "templates/**" $ 
    compile templateCompiler

  -- Static HTML
  match "static/*.html" $ do
    route (gsubRoute "static/" (const ""))
    compile $ do
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" 
				(constField "metadata" "" <> defaultContext)
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
  { deployCommand = 
      "rsync --checksum -avz _site/* " 
        ++ "mreid@login.csail.mit.edu:"
        ++ "/afs/csail.mit.edu/group/jmlr/docroot/proceedings/papers/"
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

-- | Set a field according to a render of a set of items with a given template
templateField key templateID ctx items = 
  Context $ \k _ -> if (k==key) then do
      template  <- loadBody templateID
      applyTemplateList template ctx items
	else empty

--------------------------------------------------------------------------------
-- Render the given item with using the String template if the test field exists
-- An empty string is returned if the test field does not exist.
-- Field names in the template text are delimited by %key% instead of $key$.
maybeRenderWith args item = do
  let [test , tplID] = args
  template <- loadBody (fromFilePath tplID)
  conf	   <- conferenceEntry item  
  case entryLookup item conf test of
	(Just _)	-> applyTemplateList template entryContext [item]
	Nothing		-> return ""

-- Render the given item with using the String template if the test field exists
-- An empty string is returned if the test field does not exist.
-- Field names in the template text are delimited by %key% instead of $key$.
maybeRenderAs args item = do
  let (test : rawtpl) = args
      template        = map (\c -> if c=='%' then '$' else c) (unwords rawtpl)
  
  conf <- conferenceEntry item  
  case entryLookup item conf test of
	(Just _)	-> applyTemplateList (readTemplate template) entryContext [item]
	Nothing		-> return ""

-- Renders against template given in first arg the structure ("authors", "supps")
-- given in the second arg, as derived from the given entry item.
renderListWith :: [String] -> Item Entry -> Compiler String
renderListWith (templateID : name : rest) item = do
  template <- loadBody . fromFilePath $ templateID 
  let render ctx  = applyTemplateList template ctx
  case name of 
    "authors"	-> render authorContext $ entryAuthors item

-- Renders against template given in first arg the structure ("authors", "supps")
-- given in the second arg, as derived from the given entry item.
renderJoinListAs :: [String] -> Item Entry -> Compiler String
renderJoinListAs (name : delim : tplStrs) item = do
  let template = map (\c -> if c=='%' then '$' else c) (unwords tplStrs)
  let render ctx  = applyJoinTemplateList delim (readTemplate template) ctx
  case name of 
    "authors"	-> render authorContext $ entryAuthors item

-- Renders against template given in first arg the structure ("authors", "supps")
-- given in the second arg, as derived from the given entry item.
renderListAs :: [String] -> Item Entry -> Compiler String
renderListAs (name : tplStrs) item = do
  let template = map (\c -> if c=='%' then '$' else c) (unwords tplStrs)
  let render ctx  = applyTemplateList (readTemplate template) ctx
  case name of 
    "authors"	-> render authorContext $ entryAuthors item

-- FIXME: Make sure this can handle roman (e.g., "xvi") page numbers.
pageSort :: [Item Entry] -> [Item Entry]
pageSort = sortBy (\i1 i2 -> compare (page $ itemBody i1) (page $ itemBody i2))
  where
	page e = (read . fromJust $ (getField "firstpage" e)) :: Int

conferenceContext :: Context Entry
conferenceContext = Context $ \key item ->
  return $ case entryLookup item item key of
    Just value -> value
    Nothing    -> empty 

-- Define a context for template fields using a given entry.
-- This context will look for a matching field in the Entry's BibTeX fields
-- if it is one of "title", "author", "abstract", or "pages".
-- If it is not one of these it will look up the associated conference entry
-- and look for the BibTeX field there.
entryContext = 
  constField "baseURI" "http://jmlr.csail.mit.edu/proceedings/papers"
  <> functionField "renderListWith" renderListWith
  <> functionField "renderListAs" renderListAs
  <> functionField "renderJoinListAs" renderJoinListAs
  <> functionField "maybeRenderAs" maybeRenderAs
  <> functionField "maybeRenderWith" maybeRenderWith
  <> functionField "supplementary" supplementary
  <> entryContext'

-- Derive supplementary file details from an entry item
-- This recognises the keywords "url", "name", and "kind"
supplementary [key] item = do
  let supp = entrySupplementary item 
  case supp of
	Nothing		  -> empty
	Just suppVal  -> case key of
		"url"   -> return $ filename suppVal
		"name"	-> return $ suppID suppVal
		"kind"	-> return $ kind suppVal
supplementary _ item = empty

-- Parse and return the authors for an Entry as a list of Item Author
entryAuthors = 
  fmap (Item "") . toAuthors . fromJust . getField "author" . itemBody

-- Parse and return the supplementary files for an Entry
entrySupps entry = 
  case (getField "supplementary" . itemBody) entry of
	(Just suppStr)	  -> toSupps suppStr
	Nothing			  -> []

-- FIXME: Parse supplementary string into list of (name,file)
toSupps :: String -> [(String,FilePath)]
toSupps str = undefined

entryContext' :: Context Entry
entryContext' = Context $ \key item -> do
  conf <- conferenceEntry item
  return $ case entryLookup item conf key of
	(Just value)  -> value
	Nothing		  -> empty

entryLookup :: (Item Entry) -> (Item Entry) -> String -> Maybe String
entryLookup entry conf key =
  getField key (itemBody entry) <|> getField key (itemBody conf)

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
conferenceEntry :: (Item Entry) -> Compiler (Item Entry)
conferenceEntry paperID = do
  let confID = fromFilePath . confBib . toFilePath . itemIdentifier $ paperID
  loadSnapshot confID "conference"

--------------------------------------------------------------------------------
-- Sections in the proceedings
type Section = (Maybe String, [Item Entry])


-- Break the papers into sections
section = getField "section" . itemBody

-- Build an association list of sections
makeSections :: [Section] -> [Item Entry] -> [Section] 
makeSections = foldl' addToSection

-- Adds an entry to the association list according to its section.
-- The entry is added to the section "none" if it has no section field.
addToSection :: [Section] -> (Item Entry) -> [Section]
addToSection [] entry = [(section entry, [entry])]
addToSection ((sec, es):rest) entry
  | sec == section entry   = (sec, es ++ [entry]):rest
  | True                   = (sec, es):(addToSection rest entry)

-- Build a context for a section using the given titling function.
--    section = title of section
--    papers  = rendered list of papers
sectionContext :: (String -> String) -> Context Section
sectionContext titleFor = Context $ \key item -> 
  case key of
    "section"     -> return $ titleFor . fromMaybe "none" . fst . itemBody $ item
    "papers"      -> do
        tpl <- loadBody "templates/paper-item.html"
        applyTemplateList tpl entryContext (snd . itemBody $ item)
    _             -> empty

-- Build a function that mas sections keywords to their corresponding titles
-- by parsing a "sections" field of a conference that has the form
--	  key1=Title Number 1|key2=Title Number Two|none=Default Title
sectionTitles entry = case getField "sections" . itemBody $ entry of
    Nothing     -> \_ -> ""
    Just val    -> \key -> fromMaybe "" . lookup key . convert $ val
    where 
	  tuplify [x,y] = (x,y)
	  convert = map tuplify . map (chop (=='=')) . chop (=='|') 

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
  
entrySupplementary item = 
  entryLookup item item "supplementary" >>= parseSupplementary
