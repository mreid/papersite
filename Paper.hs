--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Paper
    ( Paper (..)
  , getField
  , paperID
  , paperURI
  , paperContext
    ) where

--------------------------------------------------------------------------------
import            Control.Applicative (empty, (<$>), (<*>))
import            Control.Monad (liftM)
import            Data.Binary
import            Data.Typeable
import            Hakyll
import qualified  Text.BibTeX.Entry   as BibTex
import qualified  Text.BibTeX.Format  as BibTex.Format
import            Text.Pandoc         ( 
  Pandoc (..), Block( Para, Plain ), HTMLMathMethod ( MathJax ), 
  defaultParserState, defaultWriterOptions,
  readLaTeX, writerHTMLMathMethod, writeHtmlString
  )
--------------------------------------------------------------------------------
newtype Paper = Paper BibTex.T
    deriving (Show, Typeable)

instance Binary Paper where
    put (Paper t) = do
        put $ BibTex.entryType t
        put $ BibTex.identifier t
        put $ BibTex.fields t

    get = Paper <$> (BibTex.Cons <$> get <*> get <*> get)

instance Writable Paper where
    write fp item =
        let Paper t = itemBody item
        in writeFile fp (BibTex.Format.entry t)

instance Eq Paper where
  (==) paper paper' = paperID paper == paperID paper'

instance Ord Paper where
  compare paper paper' = compare (paperID paper) (paperID paper')
--------------------------------------------------------------------------------
-- Get the paper's indentifier
paperID :: Paper -> Identifier
paperID (Paper t) = fromFilePath $ BibTex.identifier t

paperURI :: Paper -> Identifier
paperURI paper = fromFilePath ("paper/" ++ (toFilePath $ paperID paper) ++ ".html") 

paperContext :: Context Paper
paperContext = Context $ \key item ->
    let Paper t = itemBody item
    in case key of
        "identifier" -> return $ BibTex.identifier t
        "url"        -> return $ "/" ++ (toFilePath $ paperURI (Paper t))
        _            -> case lookup key (BibTex.fields t) of
            Nothing  -> empty
            Just val -> return $ latexToHtml val

--------------------------------------------------------------------------------
-- Converts a TeX string into HTML + MathJax
-- (Adapted from Jasper Van der Jeugt's Hakyll-BibTeX code)
latexToHtml tex =
  let p = case readLaTeX defaultParserState tex of
        Pandoc meta [Para para] -> Pandoc meta [Plain para]
        x                       -> x
  in writeHtmlString defaultWriterOptions { writerHTMLMathMethod = MathJax "" } p

--------------------------------------------------------------------------------
getField field (Paper t) = lookup field $ BibTex.fields t 

abstracts = liftM latexToHtml . getField "abstract"
