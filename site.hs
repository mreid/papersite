--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import            Author
import            Control.Applicative ((<$>))
import            Control.Monad       (forM_, liftM)
import            Data.List           (intersperse)
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
  match "db/*/*.bib" $ do
    compile $ 
      entryCompiler >>= saveSnapshot "conference"

  -- Compile conference details in, e.g., @db/conf/ICML/2012.bib@ to HTML
  match "db/*/*.bib" $ version "html" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      gsubRoute ".bib" (const "/index.html")

    compile $ do 
      confID <- getUnderlying
      let pattern = fromGlob $ (dropExtension . toFilePath $ confID) ++ "/*.bib"
      papers <- loadAllSnapshots (pattern `withVersion` "entry") $ toFilePath confID

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
  match "db/*/*/*.bib" $ version "entry" $ do
    compile $ 
      entryCompiler >>= saveEntryCompiler

  -- Papers are in, e.g., @db/conf/ICML/2012/reid12b.bib
  match "db/*/*/*.bib" $ do
    route $ 
      gsubRoute "db/" (const "") `composeRoutes` 
      setExtension "html"

    compile $ do
      entry   <- entryCompiler
      metaTpl <- loadBody "templates/scholar/paper.html" 
      meta    <- applyTemplateList metaTpl entryContext [entry]

      let metaContext = constField "metadata" meta `mappend` defaultContext

      -- entryCompiler
      entryHtml <- loadAndApplyTemplate "templates/paper.html" entryContext entry
      loadAndApplyTemplate "templates/default.html" metaContext entryHtml

  -- Templates
  match "templates/**" $ 
    compile templateCompiler

  -- Static HTML
  match "static/*.html" $ do
    route (gsubRoute "static/" (const ""))
    compile $ do
        pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" 
              (constField "metadata" "" `mappend` defaultContext)
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
  -- { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
  --           \_site/* jaspervdj@jaspervdj.be:jaspervdj.be/tmp/hakyll4"
  -- }

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
  -- if key `elem` paperFields 
  -- then return $ fromJust $ getField key entry
  -- else fmap (fromJust . getField key . itemBody) conf

entryLookup :: Entry -> Compiler (Item Entry) -> String -> Compiler String
entryLookup entry conf key 
  | key `elem` paperFields  = return $ fromJust $ getField key entry
  | key `elem` confFields   = fmap (fromJust . getField key . itemBody) conf
  | True                    = return $ "FIXME"
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


{-
runHakyll :: IO()
runHakyll = hakyllWith config $ do
  
  -- Preprocessing: read in paper and author identifiers
  bibdb <- preprocess $ parseBibFile <$> readFile bibFilePath
  
  let authorsPattern  = fromList $ map authorID $ keys $ authorToPapers bibdb
  let papersPattern   = fromList $ map paperID $ keys $ paperToAuthors bibdb

  -- Templates
  match "templates/*" $ 
    compile templateCompiler

  -- Create a reference for every paper
  forM_ (keys $ paperToAuthors bibdb) $ \paper -> do
    let paperPattern = paperID paper
    
    create [ paperPattern ] $ compile $ makeItem paper
    
    create [ paperURI paper ] $ do
      route (setExtension "html")
      compile $ do
        let authors         = (paperToAuthors bibdb) ! paper
        
        authorItems <- loadAll $ fromList $ map authorID authors
        linkTpl     <- loadBody "templates/author-link.html"
        authorLinks <- joinTemplateList linkTpl authorContext authorItems ", "
        
        let paperCtx = constField "authors" authorLinks `mappend` paperContext
        
        makeItem paper
          >>= loadAndApplyTemplate "templates/paper.html" paperCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  -- Create a reference for every author
  forM_ (keys $ authorToPapers bibdb) $ \author -> do
    
    create [ authorID author ] $ compile $ makeItem author

    create [ authorURI author ] $ do
      route (setExtension ".html")
      compile $ do
        let papers        = (authorToPapers bibdb) ! author
        let papersPattern = fromList (map paperID papers)
        
        paperItems  <- loadAll papersPattern
        linkTpl     <- loadBody "templates/paper-link.html"
        paperLinks  <- applyTemplateList linkTpl paperContext paperItems
        
        let authorCtx =
                constField "title" (name author)  `mappend`
                constField "papers" paperLinks    `mappend`
                authorContext
        
        makeItem author
          >>= saveSnapshot "author"
          >>= loadAndApplyTemplate "templates/author.html" authorCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  -- Author list
  create ["authors.html"] $ do
    route idRoute
    compile $ do
      
      authors <- loadAll authorsPattern   
      itemTpl <- loadBody "templates/author-item.html"
      list    <- applyTemplateList itemTpl authorContext authors
      
      let authorsCtx =
            constField "title" "All Authors"  `mappend`
            constField "authors" list         `mappend`
            defaultContext
        
      makeItem ""
        >>= loadAndApplyTemplate "templates/authors.html" authorsCtx
        >>= loadAndApplyTemplate "templates/default.html" authorsCtx
        >>= relativizeUrls

  -- Paper list
  create ["papers.html"] $ do
    route idRoute
    compile $ do
      
      papers  <- loadAll papersPattern
      itemTpl <- loadBody "templates/paper-item.html"
      list  <- applyTemplateList itemTpl paperContext papers
      
      let papersCtx =
            constField "title" "All Papers" `mappend`
            constField "papers" list        `mappend`
            defaultContext
        
      makeItem ""
        >>= loadAndApplyTemplate "templates/papers.html" papersCtx
        >>= loadAndApplyTemplate "templates/default.html" papersCtx
        >>= relativizeUrls
-}
