--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Paper
  ( Paper (..)
  , Entry
  , parseEntry
  , getField
  , paperID
  , paperConferenceID
  , paperURI
  -- , paperContext
  , parsePaper
  ) where

--------------------------------------------------------------------------------
import            Control.Applicative (empty, (<$>), (<*>))
import            Control.Monad (liftM)
import            Control.Monad.Error
import            Data.Binary
import            Data.List.HT
import            Data.Maybe
import            Data.Typeable
import            Hakyll
import            Network.URI
import qualified  Text.BibTeX.Entry   as BibTex
import qualified  Text.BibTeX.Format  as BibTex.Format
import qualified  Text.BibTeX.Parse   as BibTex.Parse
import qualified  Text.Parsec         as Parsec           
import            Text.Pandoc         ( 
  Pandoc (..), Block( Para, Plain ), HTMLMathMethod ( MathJax ), 
  def,
  readLaTeX, writerHTMLMathMethod, writeHtmlString
  )
--------------------------------------------------------------------------------
newtype Entry = Entry BibTex.T
  deriving (Show, Typeable, Writable, Binary)

data Paper = Paper { entry :: BibTex.T, conference :: BibTex.T }
  deriving (Show, Typeable)

instance Writable BibTex.T where
  write fp item =
    writeFile fp $ BibTex.Format.entry . itemBody $ item

instance Binary BibTex.T where
  put t = do
    put $ BibTex.entryType t
    put $ BibTex.identifier t
    put $ BibTex.fields t

  get = BibTex.Cons <$> get <*> get <*> get

instance Binary Paper where
  put (Paper entry conf) = do
    put $ entry
    put $ conf

  get = Paper <$> get <*> get

instance Writable Paper where
  write fp item =
    let Paper entry conf = itemBody item
    in writeFile fp ((BibTex.Format.entry entry) ++ (BibTex.Format.entry conf))

instance Eq Paper where
  (==) paper paper' = paperID paper == paperID paper'

instance Ord Paper where
  compare paper paper' = compare (paperID paper) (paperID paper')

parseEntry :: FilePath -> Entry
parseEntry path =
  case Parsec.parse BibTex.Parse.file "<BibTeX entry>" path of
    Left err       -> error $ show err
    Right [entry]  -> Entry entry 
    Right []       -> error $ "Empty BibTeX file: " ++ path
    Right _        -> error "BibTeX files must only have a single entry"

-- Parses a Paper from the given BibTeX file in the site's database.
parsePaper :: FilePath -> Paper
parsePaper path = 
  case Parsec.parse BibTex.Parse.file "<BibTeX paper>" path of
    Left err       -> error $ show err
    Right [paper]  -> 
      case Parsec.parse BibTex.Parse.file "<BibTeX conference>" (confPath path) of
        Left err      -> error $ show err
        Right [conf]  -> Paper paper conf
        Right _       -> error "More than one entry in the conference BibTeX file"
    Right _ -> error "More than a single paper in the BibTeX file"

-- Compute the path for the conference BibTeX file given the path for a paper
-- e.g., this takes "db/ICML/2012/reid12a.bib" to "db/ICML/2012.bib"
confPath :: FilePath -> FilePath
confPath path = (concat . init . segmentBefore (== '/') $ path) ++ ".bib"

--------------------------------------------------------------------------------
-- Get the paper's indentifier
paperID :: Paper -> Identifier
paperID (Paper entry _) = fromFilePath $ BibTex.identifier entry

paperURI :: Paper -> Identifier
paperURI paper = fromFilePath ("paper/" ++ (toFilePath $ paperID paper) ++ ".html") 

paperConferenceID :: Paper -> Identifier
paperConferenceID (Paper _ conf) = fromFilePath $ BibTex.identifier conf


--------------------------------------------------------------------------------
-- Converts a TeX string into HTML + MathJax
-- (Adapted from Jasper Van der Jeugt's Hakyll-BibTeX code)
latexToHtml tex =
  let p = case readLaTeX def tex of
        Pandoc meta [Para para] -> Pandoc meta [Plain para]
        x                       -> x
  in writeHtmlString 
    def { writerHTMLMathMethod = MathJax "" } p

--------------------------------------------------------------------------------
getField :: String -> Entry -> Maybe String
getField key entry@(Entry t) =
  case key of
  "identifier"  -> Just $ BibTex.identifier t
  "firstpage"   -> firstpage
  "lastpage"    -> lastpage
  "url"         -> fmap (toURI "html") $ identifier
  "pdf"         -> fmap (toURI "pdf") $ identifier
  "metatitle"   -> lookup "title" . BibTex.fields $ t
  _             -> fmap latexToHtml (lookup key . BibTex.fields $ t)
  where
    pages = getField "pages" entry
    identifier = getField "identifier" entry
    firstpage = fmap (takeWhile isNumber) pages
    lastpage  = fmap (reverse . takeWhile isNumber . reverse) pages
    isNumber c = c `elem` ['0'..'9'] ++ ['x','v','i']

toURI :: String -> FilePath -> String
toURI ext path = (escapeURIString isUnreserved path) ++ "." ++ ext
