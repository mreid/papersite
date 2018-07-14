module Page (firstPage, lastPage, orderValue) where

import Data.Functor         ((<$>))
import Data.Char            (toUpper, isDigit)
import Data.Function        (on)
import Data.Maybe           (fromJust)
import Text.BibTeX.Entry    (T(..))

--------------------------------------------------------------------------------
-- Handling pages that can be specified in Roman numerals, integers, or
-- (thanks to COLT), decimal (e.g., page 1.1)

-- Flag denoting whether entry is in preface or not
data Location = Preface | Regular deriving (Show, Eq, Ord)

-- Compare entries by their first page
instance Eq T where (==) = (==) `on` (fmap orderValue . firstPage)
instance Ord T where compare = compare `on` (fmap orderValue . firstPage)

-- Look up the given key in a BibTeX entry
get :: String -> T -> Maybe String
get key = lookup key . fields

-- Parse a roman or arabic numeral into a (Location,Int) pair
orderValue :: String -> (Location, Float)
orderValue str
  | isRomanStr str      = (Preface, readRoman . map toUpper $ str) 
  | otherwise           = (Regular, read str :: Float)

-- Get the first page, parsing Roman numeral pages into integers
-- Roman numerals indicate a Preface entry, otherwise it is Regular
firstPage :: T -> Maybe String
firstPage entry = takeWhile isPageDigit <$> get "pages" entry 

-- Get the last page of an entry.
lastPage :: T -> Maybe String
lastPage entry = reverse . (takeWhile isPageDigit) . reverse <$> get "pages" entry

-- Is the character a valid roman or arabic digit?
isPageDigit :: Char -> Bool
isPageDigit c = ('.' == c) || isDigit c || isRoman c

--------------------------------------------------------------------------------
-- Handle roman numerals in page numbers
romanChars = "IVXLCDM"

isRoman :: Char -> Bool
isRoman = flip elem romanChars . toUpper

isRomanStr :: String -> Bool
isRomanStr = isRoman . head

readRoman :: String -> Float
readRoman = fst . foldr (\p (t,s) -> 
    if p >= s then (t+p,p) else (t-p,p)) (0,0) . map 
      (fromJust . flip lookup (zip "IVXLCDM" [1,5,10,50,100,500,1000]))

