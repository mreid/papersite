--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
import            Control.Monad
import            Data.Char
import            Data.Functor
import            Data.List
import            System.Directory
import            System.Environment
import            System.IO
import qualified  Text.BibTeX.Entry    as BibTex
import qualified  Text.BibTeX.Format   as BibTex
import qualified  Text.BibTeX.Parse    as BibTex.Parse
import qualified  Text.LaTeX.Character as LaTeX
import qualified  Text.Parsec          as Parsec

--------------------------------------------------------------------------------
data Proceedings = 
  Proceedings { conference :: BibTex.T, entries :: [BibTex.T] }
  deriving (Show)

instance Eq BibTex.T where
  (==) t t' = (BibTex.entryType t) == (BibTex.entryType t')

fieldMap :: (String -> String) -> BibTex.T -> BibTex.T
fieldMap f t = 
  BibTex.Cons 
    (BibTex.entryType t) 
    (BibTex.identifier t)
    (map (\(k,v) -> (k,f v)) $ BibTex.fields t)
--------------------------------------------------------------------------------

main :: IO()
main = do
  args <- getArgs 
  case args of
    [name, dir]  -> do
      print $ "Importing " ++ name ++ " to " ++ dir
      handle  <- openFile name ReadMode
      bibtex  <- hGetContents handle

      let parsed = parseBibFile bibtex
      case parsed of
        (Just proceedings)  ->  do
          writeProceedings proceedings dir 
          forM_ (entries proceedings) $ \entry -> writeEntry entry dir
        Nothing             -> error $ "Could not parse " ++ name


    _            -> print "Usage: import bibtex destination"

parseBibFile :: String -> Maybe Proceedings
parseBibFile string = case Parsec.parse BibTex.Parse.file "<bib file>" string of
  Left err -> error $ show err
  Right xs -> makeProceedings xs

makeProceedings :: [BibTex.T] -> Maybe Proceedings
makeProceedings entries =
  liftM2 Proceedings conference entries'
  where
    conference = find isConference entries
    entries'   = liftM2 delete conference (Just entries)

isConference :: BibTex.T -> Bool
isConference entry =
  map toLower (BibTex.entryType entry) `elem` ["conference", "proceedings"]

writeProceedings :: Proceedings -> FilePath -> IO ()
writeProceedings proceedings dirPath = do
  procHandle <- openFile (dirPath ++ "/conference.bib") WriteMode
  hPutStr procHandle $ BibTex.entry $ fieldMap LaTeX.fromUnicodeString $ conference proceedings
  hClose procHandle

writeEntry :: BibTex.T -> FilePath -> IO ()
writeEntry entry dirPath = do
  let entryID = BibTex.identifier entry
  handle <- openFile (dirPath ++ "/" ++ entryID ++ ".bib") WriteMode
  hPutStr handle $ BibTex.entry $ entry
