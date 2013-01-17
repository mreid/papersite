--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import      Author
import      Control.Applicative ((<$>))
import      Control.Monad       (forM_)
import      Data.List           (intersperse)
import      Data.Map            (keys, (!))
import      Data.Monoid         (mappend)
import      Data.String         (fromString)
import      Hakyll
import      BibDB
import      Paper
import      Text.Pandoc

--------------------------------------------------------------------------------
bibFilePath :: String
bibFilePath = "testdata/ACML12Papers.bib"
bibFileIdentifier :: Identifier
bibFileIdentifier = fromString bibFilePath
bibFilePattern :: Pattern
bibFilePattern = fromString bibFilePath

main :: IO ()
main = do
  runHakyll

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

  -- Static HTML
  match "static/*.html" $ do
    route (gsubRoute "static/" (const ""))
    compile $ do
        pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- CSS
  match "static/css/*.css" $ do
    route (gsubRoute "static/" (const ""))
    compile copyFileCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
            \_site/* jaspervdj@jaspervdj.be:jaspervdj.be/tmp/hakyll4"
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


