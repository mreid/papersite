--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main ( main, hasSupplementary, fieldAdd ) where
--------------------------------------------------------------------------------
import            Control.Applicative
import            Control.Monad
import            Data.Char
import            Data.Functor
import            Data.List
import            Data.Maybe
import            System.Directory
import            System.Environment
import            System.FilePath
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

-- Add a new key-value mapping to the given BibTeX entry
-- This will override any previous value with the same key.
fieldAdd :: String -> String -> BibTex.T -> BibTex.T
fieldAdd key value t =
  BibTex.Cons
    (BibTex.entryType t)
    (BibTex.identifier t)
    ((key, value) : (BibTex.fields t))

fieldMap :: (String -> String) -> BibTex.T -> BibTex.T
fieldMap f t = 
  BibTex.Cons 
    (BibTex.entryType t) 
    (BibTex.identifier t)
    (map (\(k,v) -> (k,f v)) $ BibTex.fields t)

fieldFilter :: (String -> Bool) -> BibTex.T -> BibTex.T
fieldFilter p t =
  BibTex.Cons
    (BibTex.entryType t)
    (BibTex.identifier t)
    (filter (p.fst) (BibTex.fields t))

value :: String -> BibTex.T -> String
value key entry = fromJust $ lookup key $ BibTex.fields entry

year :: Proceedings -> String
year = value "year" . conference

shortname :: Proceedings -> String
shortname = value "shortname" . conference

volume :: Proceedings -> String
volume = value "volume" . conference


--------------------------------------------------------------------------------
-- | Imports a single BibTeX file into the given directory. The
--   BibTeX file must contain:
--
--    * One or more @\@InProceedings@ entries. These must contain the following
--      fields: title, author, abstract, and pages
--    
--    * Exactly one @\@Proceedings@ or @\@Conference@ entries. This must
--      contain the following fields: shortname, booktitle, year, editor, and volume
--   
--   The importer converts the entries in the BibTeX file to a number of
--   files in the @db/@ directory. It does so as follows:
--
--   1. The \@Proceedings / \@Conference entry (henceforth, "conference")
--      is saved to a file @db/vNN.bib@ where @NN@ is the volume number.
--
--      If the @booktitle@ field is not present it is set to the value of
--      the @title@ field.
--
--   2. A modified version of each other entry (the @\@InProceedings@ entries) 
--      is written to @db/vNN/ID.bib@ where @NN@ is the volume number
--      and @ID@ is value of the entry's BibTeX indentifier.
main :: IO()
main = do
  args <- getArgs 
  case args of
    [name, dbPath]  -> do
      print $ "Importing " ++ name ++ " to " ++ dbPath
      handle  <- openFile name ReadMode
      bibtex  <- hGetContents handle

      let parsed = parseBibFile bibtex
      case parsed of
        (Just procs)  ->  do
          let targetdir = intercalate "/" [dbPath, "v" ++ (volume procs)]
          let sourcedir = dropFileName name
          createDirectoryIfMissing True targetdir
          writeProceedings procs targetdir 
          suppFiles <- copyBinaries sourcedir targetdir
          forM_ (entries procs) $ \entry ->
            writeEntry (addSupps suppFiles entry) targetdir

        Nothing       -> error $ "Could not parse " ++ name

    _            -> print "Usage: import bibtex database_directory"

addSupps :: [String] -> BibTex.T -> BibTex.T
addSupps suppFiles entry =
  case hasSupplementary suppFiles entry of
    (Just suppFile)   -> addSupp suppFile entry
    Nothing           -> entry

addSupp :: String -> BibTex.T -> BibTex.T
addSupp suppFile entry =
  let value = "Supplementary:" ++ suppFile in
    fieldAdd "supplementary" value entry

-- Test whether the given filename is for a supplementary file
-- TODO: Currently only checks whether it ends in "-supp"
isSupplementary :: FilePath -> Bool
isSupplementary = (isSuffixOf "-supp") . takeBaseName

-- Test whether the given list of supplementary files names contains the given
-- entry identifier and returns the filename of the supplementary, if so.
hasSupplementary :: [String] -> BibTex.T -> Maybe String
hasSupplementary suppFiles entry = 
  let entryID = BibTex.identifier entry 
      suppIDs = map takeFileName suppFiles
  in find (entryID `isPrefixOf`) suppIDs

-- Moves all binary files (e.g., PDFs) for the given file to the target directory
-- and returns a list of the supplementary file names
copyBinaries :: FilePath -> FilePath -> IO [String]
copyBinaries sourceDir targetDir = do
  -- let suppDir = combine targetDir "supplementary"
  -- createDirectoryIfMissing True suppDir
  
  contents <- getDirectoryContents sourceDir
  -- TODO: This currently copies sup files that PDF twice
  let suppNames = filter isSupplementary contents
  let pdfNames  = filter ((==) ".pdf" . takeExtension) contents
  forM_ (suppNames ++ pdfNames) $ \filename ->
    copyFile (sourceDir </> filename) (targetDir </> filename)
  
  return $ map takeFileName suppNames

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
writeProceedings procs dirPath = do
  let conf = conference procs
  procHandle <- openFile (dirPath ++ ".bib") WriteMode
  hPutStr procHandle $ BibTex.entry $ fieldMap LaTeX.fromUnicodeString $ conf
  hClose procHandle

writeEntry :: BibTex.T -> FilePath -> IO ()
writeEntry entry dirPath = do
  let entryID = BibTex.identifier entry
  handle <- openFile (dirPath ++ "/" ++ entryID ++ ".bib") WriteMode
  hPutStr handle $ BibTex.entry $ cleanEntry entry
  hClose handle

cleanEntry = 
  fieldFilter 
    (`elem` ["author", "title", "abstract", "pages","supplementary", "section", "note"])

