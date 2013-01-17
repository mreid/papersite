--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
import            Control.Monad
import            Data.Char
import            Data.Functor
import            Data.List
import            System.Environment
import            System.IO
import qualified  Text.BibTeX.Entry    as BibTex
import qualified  Text.BibTeX.Parse    as BibTex.Parse
import qualified  Text.Parsec          as Parsec

--------------------------------------------------------------------------------
data Proceedings = 
  Proceedings { conference :: BibTex.T, entries :: [BibTex.T] }
  deriving (Show)

instance Eq BibTex.T where
  (==) t t' = (BibTex.entryType t) == (BibTex.entryType t')
--------------------------------------------------------------------------------

main :: IO()
main = do
  args <- getArgs 
  case args of
    [name, dir]  -> do
      print $ "Importing " ++ name ++ " to " ++ dir
      handle  <- openFile name ReadMode
      bibtex  <- hGetContents handle

      let proceedings = parseBibFile bibtex

      print $ proceedings

    _            -> print "Usage: import bibtex destination"

parseBibFile :: String -> Maybe Proceedings
parseBibFile string = case Parsec.parse BibTex.Parse.file "<bib file>" string of
  Left err -> error $ show err
  Right xs -> makeProceedings xs

makeProceedings entries =
  liftM2 Proceedings conference entries'
  where
    conference = find isConference entries
    entries'   = liftM2 delete conference (Just entries)

isConference entry =
  map toLower (BibTex.entryType entry) `elem` ["conference", "proceedings"]
