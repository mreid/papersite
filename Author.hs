--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Author
  ( Author (..)
	, authorURI
	, authorContext
	, toAuthors
  , name
  ) where

--------------------------------------------------------------------------------
import           Control.Applicative (empty, (<$>), (<*>))
import 			 Control.Monad       (liftM)
import           Data.Binary         (Binary (..))
import			 Data.Char
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Hakyll
import			 Network.URI
import qualified Text.BibTeX.Parse   as BibTex.Parse
import qualified Text.Parsec         as Parsec
import 			 Text.LaTeX.Character

--------------------------------------------------------------------------------
-- newtype AuthorID = AuthorID String

data Author = Author { 
	authorID	:: Identifier,
	firstNames 	:: [String],
	lastName 	:: String
} deriving (Show, Typeable)

-- Author IDs are canonical and unique so just compare those.
instance Eq Author where
	(==) (Author aID _ _) (Author aID' _ _) = aID == aID'

-- Authors are sorted by their IDs, which is equivalent to by last name.
instance Ord Author where
	compare (Author aID _ _ ) (Author aID' _ _ ) = compare aID aID'

instance Binary Author where
	put (Author aID fnames lname) = do
		put aID
		put fnames
		put lname

	get = Author <$> get <*> get <*> get

instance Writable Author where
	write fp item  = 
		let (Author aID fnames lname) = itemBody item in
		writeFile fp $ concat $ [toFilePath aID] ++ fnames ++ [lname]

--------------------------------------------------------------------------------
name :: Author -> String
name author = (unwords $ firstNames author) ++ lastName author

authorURI :: Author -> Identifier
authorURI author = 
    fromFilePath ("author/" ++ (toFilePath $ authorID author) ++ ".html")

authorContext :: Context Author
authorContext =
  field "id"          (return . toFilePath . authorID . itemBody)             <>
  field "name"        (return . name . itemBody)                              <>
  field "firstnames"  (return . unwords . firstNames . itemBody)              <>
  field "surname"     (return . lastName . itemBody )                         <>
  field "url"         (return . ("/" ++) . toFilePath . authorURI . itemBody) 

-- Creates a Hakyll context for an author
-- authorContext :: Context Author
-- authorContext = Context $ \key item ->
-- 	let author = itemBody item
-- 	in case key of
-- 		"id"	      -> return $ toFilePath $ authorID author
-- 		"name"	      -> return $ name author
-- 		"firstnames"  -> return $ unwords $ firstNames author
-- 		"surname"     -> return $ lastName author
-- 		"url"	      -> return $ "/" ++ (toFilePath $ authorURI author)
-- 		_		      -> return $ "NA" 

--------------------------------------------------------------------------------
-- Converts a raw BibTeX author name into an Author
-- e.g., toAuthor "Gr\"unwald, Peter D." = Author ["Peter", "D."] "Grünwald"
toAuthor :: String -> Author
toAuthor name =
	-- Handle either "Last, Firstnames" case or "Firstnames Lastnames"
	case break (== ',') unicode of
		(_, "")			->	parseFirstLast unicode
		(last, firsts)	->  parseLastFirst (splitnames firsts) last
	where
		unicode 	= trim $ toUnicodeString name
		splitnames 	= filter (/= ",") . words

-- Creates a canonical ID for a given author
makeAuthorID :: [String] -> String -> Identifier
makeAuthorID firstNames lastName = fromFilePath 
		-- $ escapeURIString isAscii 
    $ cleanUnicode
		$ lastName ++ "_" ++ map head firstNames

toAuthors :: String -> [Author]
toAuthors = map toAuthor . BibTex.Parse.splitAuthorList

-- Parse author names of the form "First Names Lastname"
-- FIX: Currently does not handle multipart last names (e.g., "von ...")
parseFirstLast :: String -> Author
parseFirstLast fullname = Author aID firstNames lastName
	where
		names 		= words fullname
		firstNames 	= init names
		lastName	= last names
		aID			= makeAuthorID firstNames lastName

parseLastFirst :: [String] -> String -> Author
parseLastFirst firstNames lastName = 
	Author (makeAuthorID firstNames lastName) firstNames lastName

--------------------------------------------------------------------------------
-- Simple (and lossy) conversion of unicode characters to ASCII for URIs
-- Hand coded from the Unicode Latin 1 and (parts of) A Supplement table here:
--   http://en.wikipedia.org/wiki/List_of_Unicode_characters
cleanUnicode :: String -> String
cleanUnicode s = map convert s
    where
      convert c = case (lookup c mapping) of
        (Just c') -> c'
        Nothing   -> c
      mapping = 
        concatMap 
          (\(list,char) -> [(char',char) | char' <- list])
          [ (['\x00C0'..'\x00C6']++['\x0100','\x0102','\x0104'], 'A'),
            (['\x00C7','\x0106','\x0108','\x010A','\x010C'],     'C'),
            (['\x00C8'..'\x00CB'], 'E'),
            (['\x00CC'..'\x00CF'], 'I'),
            (['\x00D0','\x010E','\x0110'], 'D'),
            (['\x00D1']          , 'N'),
            (['\x00D2'..'\x00D6'], 'O'),
            (['\x00D9'..'\x00DC'], 'U'),
            (['\x00DD']          , 'Y'),
            (['\x00D2'..'\x00D6'], 'O'),
            (['\x00DF']          , 's'),
            (['\x00E0'..'\x00E6']++['\x0101','\x0103','\x0105'], 'a'),
            (['\x00E7','\x0107','\x0109','\x010B','\x010D'],     'c'),
            (['\x00E8'..'\x00EB']++['\x0113','\x0115','\x0117','\x0119','\x011B'],'e'),
            (['\x00EC'..'\x00EF'], 'i'),
            (['\x00F1']          , 'n'),
            (['\x00F2'..'\x00F6'], 'o'),
            (['\x00F8']          , 'o'),
            (['\x00F9'..'\x00FC'], 'u'),
            (['\x00FD']          , 'y'),
            (['\x00FF']          , 'y') ]
