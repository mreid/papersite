--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BibDB
    ( BibDB (..)
    , parseBibFile
    ) where


--------------------------------------------------------------------------------
import           Author
import           Control.Applicative  (empty, (<$>), (<*>))
import           Control.Monad        (liftM)
import           Data.Binary          (Binary (..))
import           Data.Char
import           Data.Maybe
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Typeable        (Typeable)
import           Hakyll
import           Network.URI
import           Paper
import qualified Text.BibTeX.Entry    as BibTex
import qualified Text.BibTeX.Parse    as BibTex.Parse
import qualified Text.Parsec          as Parsec
import           Text.LaTeX.Character


--------------------------------------------------------------------------------
data BibDB = BibDB {
  paperToAuthors  :: Map Paper [Author],
  authorToPapers  :: Map Author [Paper]
} deriving (Show, Typeable)

instance Binary BibDB where
  put (BibDB p2a a2p) = do
    put p2a
    put a2p

  get = BibDB <$> get <*> get


--------------------------------------------------------------------------------
parseBibFile :: String -> BibDB
parseBibFile string = case Parsec.parse BibTex.Parse.file "<bib file>" string of
  Left err -> error $ show err
  Right xs -> scanBibFileEntries xs

scanBibFileEntries :: [BibTex.T] -> BibDB
scanBibFileEntries entries = foldr updateBibFile newBibFile entries
  where
    newBibFile = BibDB Map.empty Map.empty 

updateBibFile :: BibTex.T -> BibDB -> BibDB
updateBibFile entry bibdb = 
  case map toUpper $ BibTex.entryType entry of
    "INPROCEEDINGS" -> updateBibFile' (Paper entry) bibdb
    _               -> bibdb
  -- where
  --   newPaper = (Paper paperID entry)
  --   paperID  = case BibTex.

updateBibFile' paper (BibDB p2a a2p) = BibDB p2a' a2p'
  where
    p2a' = Map.insertWith (++) paper authors p2a
    a2p' = foldr (\author mp -> Map.insertWith (++) author [paper] mp) a2p authors
    authors = paperAuthors paper

paperAuthors paper = case getField "author" paper of
  (Just authorStr)  -> toAuthors authorStr
  Nothing       -> []

--------------------------------------------------------------------------------
-- getField field = lookup field . BibTex.fields 
