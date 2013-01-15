--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import Control.Monad (liftM)
import Text.Parsec.String
import Text.BibTeX.Parse
import Text.BibTeX.Entry
import Text.LaTeX.Character
import Text.Regex
import Data.Maybe
import Data.Char
import qualified Data.Text 		as T
import qualified Data.Text.IO 	as T
import qualified Text.Pandoc	as PD
import qualified Text.Parsec	as Parsec
import Hakyll.Core.Rules

data Author = Author { 
	firstNames :: [T.Text],
	lastName :: T.Text
} deriving (Show)

main = do { result <- parseFromFile file "testdata/ACML12Papers.bib"
	; case result of
		Left err	-> print err
		-- Right xs 	-> print $ map authors $ filter inProceedings xs
		Right xs 	-> print $ map abstracts $ filter inProceedings xs
}

inProceedings = (==) "InProceedings" . entryType

getField field = lookup field . fields . lowerCaseFieldNames

authors = liftM (mapM toAuthor) . liftM splitAuthorList . getField "author"
abstracts = liftM latexToHtml . getField "abstract"

--------------------------------------------------------------------------------
-- Converts a raw BibTeX author name into an Author
-- e.g., toAuthor "Gr\"unwald, Peter D." = Author ["Peter", "D."] "Grünwald"
toAuthor :: String -> Maybe Author
toAuthor name =
	-- Handle either "Last, Firstnames" case or "Firstnames Lastnames"
	case T.breakOn "," unicode of
		(_, "")			->	parseFirstLast unicode
		(last, firsts)	->  Just (Author (splitnames firsts) last)
	where
		unicode 	= T.strip $ T.pack $ toUnicodeString name
		splitnames 	= filter (/= ",") . T.words

-- Parse author names of the form "First Names Lastname"
-- FIX: Currently does not handle multipart last names (e.g., "von ...")
parseFirstLast :: T.Text -> Maybe Author
parseFirstLast fullname = Just (Author (init names) (last names))
	where 
		names = T.words fullname 

-- Creates a canonical ID for a given author
authorID author =
	T.concat ( 
		[(clean $ lastName author), "_"] ++ 
		map (T.singleton . T.head) (firstNames author) 
	)
	where
		clean = T.filter isAlpha

-- Converts a TeX string into HTML + MathJax
-- (Adapted from Jasper Van der Jeugt's Hakyll-BibTeX code)
latexToHtml tex =
	let p = case PD.readLaTeX PD.defaultParserState tex of
				PD.Pandoc meta [PD.Para para] -> PD.Pandoc meta [PD.Plain para]
				x                             -> x
	in PD.writeHtmlString PD.defaultWriterOptions { PD.writerHTMLMathMethod = PD.MathJax "" } p

