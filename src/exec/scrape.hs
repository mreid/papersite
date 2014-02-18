module Main (main, parseConf, parseOne, LinkType) where
import Control.Arrow
import Control.Monad (forM_, liftM3, ap)
import Control.Applicative 
import Data.Char
import Data.List
import Data.Maybe
import Data.List.HT (replace, segmentBefore, chop, dropWhileRev)
import Network.HTTP
import Network.URI
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Text.HTML.TagSoup
import qualified Data.Text as T
import qualified Text.BibTeX.Entry as Bib
import qualified Text.BibTeX.Format as Bib
import qualified Text.Regex.XMLSchema.String as RE

type PaperKey = String 
data LinkType = Abstract | PDF | Supplementary deriving (Eq,Show)
data Volume = Volume {
  url     :: URI,
  papers  :: [(PaperKey, LinkType -> Maybe URI)],
  orphans :: [[(LinkType, URI)]]
}
instance Show Volume where
  show (Volume url papers orphans) =
    "Volume @ " ++ (show url) ++ "\n" ++
    "Papers:\n" ++ (unlines . map showPaper $ papers) ++ "\n"
    where
      showPaper p = 
        "  " ++ (fst p) ++ ":\n" ++
        "    Abs:  " ++ (show . snd p $ Abstract) ++ "\n" ++
        "    PDF:  " ++ (show . snd p $ PDF) ++ "\n" ++
        "    Supp: " ++ (show . snd p $ Supplementary) ++ "\n"

--------------------------------------------------------------------------------
-- Main
main = forM_ volumes $ \vID -> do
  let vURI = fromJust . parseRelativeReference . (++ "/") $ vID
  conf <- parseConf $ vURI `relativeTo` baseURI 
  putStrLn $ "Volume " ++ (show vURI) ++ "\n---------------"
  forM_ (papers conf) $ \paper -> do
    bib <- parseOne vID paper
    putStrLn . Bib.entry $ bib
    let filepath = "testbib/" ++ (Bib.identifier bib) ++ ".bib"
    writeFile filepath $ Bib.entry bib

-- Testing the representative papers from each volume
-- testReps = forM_ volReps $ \pURL -> do
--   bib <- parseOne .fromJust . parseAbsoluteURI $ pURL
--   putStrLn . Bib.entry $ bib
--   let filepath = "testbib/" ++ (Bib.identifier bib) ++ ".bib"
--   writeFile filepath $ Bib.entry bib

--------------------------------------------------------------------------------
-- Constants, etc.
baseURI    = fromJust . parseAbsoluteURI $ "http://jmlr.org/proceedings/papers/"
baseURIold = fromJust . parseAbsoluteURI $ "http://jmlr.csail.mit.edu/proceedings/papers/"
volumes = map (("v"++) . show) [1..27]

--------------------------------------------------------------------------------
-- Parse conference page of links
parseConf url = do
  let links = fromUrl (show url) >>> css "a"
 
  -- Filter links to those for abstracts, PDFs, or supplementary material
  materials <- runX $ links >>>
                  ((deep $ hasText isLink >>> getText >>> arr tagLink) &&& 
                   (getAttrValue "href" >>> arr (cleanUrl url)) )

  -- Build list of related materials by partitioning on each Abstract
  let papers = segmentBefore ((== Abstract) . fst) materials
  return $ makeVolume url papers

-- Convert a URL to standard, relative form
cleanUrl :: URI -> String -> URI
cleanUrl volURI str 
  | isRelativeReference str = (parseRel str) `relativeTo` volURI 
  | isAbsoluteURI str = 
      ((parseRel str) `relativeFrom` baseURIold) `relativeTo` baseURI 
  where
    parseRel = fromJust . parseURIReference

-- Turn a list of related material links into a conference data structure
makeVolume :: URI -> [[(LinkType, URI)]] -> Volume
makeVolume url lists =
  Volume {
    url     = url,
    papers  = map (\p -> (makePaperID p, flip lookup p)) withAbs,
    orphans = noAbs
  }
  where
    (withAbs, noAbs) = partition (any ((== Abstract) . fst)) lists
    makePaperID = fromJust . fmap stripLink . lookup Abstract 
    stripLink   = last . init . chop (`elem` "./") . uriPath 

paperKeys = map fst . papers

getItem conf key kind = case lookup key (papers conf) of
  Just table  -> table kind
  Nothing     -> Nothing

abstractUrl :: Volume -> PaperKey -> Maybe URI
abstractUrl conf key = fullUrl <$> getItem conf key Abstract
  where
    fullUrl = (`relativeTo` (url conf))

-- Convert the text in a link into an Abstract, PDF, or Supplementary tag
tagLink str
  | isAbstract str  = Abstract
  | isPDF str       = PDF
  | isSupp str      = Supplementary

-- Test whether link is a content link
isLinkType key = isInfixOf key . map toLower
isAbstract = isLinkType "abs"
isPDF = isLinkType "pdf"
isSupp = isLinkType "supp"
isLink str = isAbstract str || isPDF str || isSupp str

--------------------------------------------------------------------------------
-- Parse single pages

-- Get the contents of a given URL
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- Parse a single JMLR abstract page for a paper.
parseOne vID (pID,links) = do
  let absURL = fromJust . links $ Abstract
  -- putStrLn $ "Getting " ++ (parseURIReference . fromJust . links $ Abstract) ++ "\n------\n"
  -- let result = RE.matchSubex ".*/({vID}v\\d+)/({pID}.*)\\.html?" (uriPath url)
  -- let vID       = fromJust $ lookup "vID" result
  -- let pID       = fromJust $ lookup "pID" result
  putStrLn $ "Volume ID: " ++ vID ++ "; Paper ID: " ++ pID

  tags <- parseTags <$> openURL (show absURL)

  let title    = extract $ findTitle vID tags
  let abstract = deligature . extract . substitute $ findAbstract vID tags
  let (authors,volume,pages,year) = getBibInfo vID $ findInfo vID tags

  let get = maybe "NA" id
  -- putStrLn $ "Title: " ++ title
  -- putStrLn $ "Authors: " ++ authors
  -- putStrLn $ "Volume: " ++ get volume ++
  --            "; Pages: " ++ (get . fst $ pages) ++ "–" ++ (get . snd $ pages) ++
  --            "; Year: " ++ (get year) 
  -- putStrLn $ "Abstract\n--------\n" ++ abstract
  -- putStrLn "\n======="
   
  let pdfField = 
        case (links PDF) of
          Just url    -> [("pdf", show url)]
          Nothing     -> []
   
  let suppField = 
        case (links Supplementary) of
          Just url    -> [("supplementary", "Supplementary:"++(show url))]
          Nothing     -> []

  let bibEntry = Bib.Cons {
    Bib.entryType   = "InProceedings",
    Bib.identifier  = pID,
    Bib.fields      = [
      ("title",    title),
      ("author",   authors),
      ("pages",    (get . fst $ pages ) ++ "--" ++ (get . snd $ pages)),
      ("abstract", abstract)
    ] ++ pdfField ++ suppField
  }
  
  return $ bibEntry

--------------------------------------------------------------------------------
-- Parsing Rules
-- Per volume rules for extracting title, abstract, and info lines from pages

-- Find title in a tag list
findTitle :: String -> [Tag String] -> [Tag String]
findTitle "v12" = flip (!!) 0 . partitions (~== "<p>")
findTitle "v14" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
findTitle "v16" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
findTitle "v18" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
findTitle "v20" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
findTitle _     = between (~== "<h2") (~== "</h2") 

findInfo :: String -> [Tag String] -> [Tag String]
findInfo "v12"  = flip (!!) 1 . partitions (~== "<p>")
findInfo "v14"  = between (~== "<span") (~== "</div>") 
findInfo "v16"  = between (~== "<span") (~== "</div>") 
findInfo "v18"  = between (~== "<span") (~== "</div>") 
findInfo "v20"  = between (~== "<span") (~== "</div>")
findInfo _      = between (~== "</h2") (~== "Abstract") 

findAbstract :: String -> [Tag String] -> [Tag String]
findAbstract "v12"  = flip (!!) 3 . partitions (~== "<p>")
findAbstract _      = between (~== "Abstract") (~== "</div") 

fixAuthors :: String -> String -> String
fixAuthors _        = stripSpace . 
                      replace "," " and" .
                      replace "&" " and " .
                      replace "JMLR" "" . replace ";" ""

stripSpace = dropWhile (== ' ') . dropWhileRev (== ' ')

-- Substitute <em> and <i> tags for \emph{} and <tt> for \texttt{}
-- and <sup> and <sub> for ^ and _, respectively
substitute :: [Tag String] -> [Tag String]
substitute = map subfn 
  where subfn (TagOpen "em" _)  = TagText "\\emph{"
        subfn (TagClose "em")   = TagText "}"
        subfn (TagOpen "i" _)   = TagText "\\emph{"
        subfn (TagClose "i")    = TagText "}"
        subfn (TagOpen "tt" _)  = TagText "\\texttt{"
        subfn (TagOpen "sup" _) = TagText "^{"
        subfn (TagClose "sup")  = TagText "}"
        subfn (TagOpen "sub" _) = TagText "_{"
        subfn (TagClose "sub")  = TagText "}"
        subfn (TagClose "tt")   = TagText "}"
        subfn other             = other

deligature :: String -> String
deligature =
  T.unpack .
    T.replace (T.pack "\239\172\128") (T.pack "ff") .   -- ff
    T.replace (T.pack "\239\172\129") (T.pack "fi") .   -- fi
    T.replace (T.pack "\239\172\130") (T.pack "fl") .   -- fl
    T.replace (T.pack "\239\172\131") (T.pack "ffi") .   -- ffi
    T.replace (T.pack "\226\128\156") (T.pack "``") .   -- ``
    T.replace (T.pack "\226\128\157") (T.pack "''") .   -- ''
    T.replace (T.pack "\226\128\148") (T.pack "--") .   -- --
    T.replace (T.pack "\195\175") (T.pack "\\\"{\\i}") .   -- ï
    T.pack

-- Returns text strictly between the matching start and end conditions
between :: (Tag String -> Bool) -> (Tag String -> Bool) -> [Tag String] -> [Tag String]
between start end = takeWhile (not . end) . tail . dropWhile (not . start)

-- Function to extract the JMLR bibliographic details from a line of
-- (roughly) the form
--    Authors; JMLR W&CP Vol:First-Last, Year  
-- Note: Volume 23 does not fit this pattern perfectly, e.g., 
--    Parikshit Gopalan, Adam R. Klivans and Raghu Meka JMLR W&CP 23: 15.1 - 15.10, 2012
getBibInfo vID tags = (fixAuthors vID authors, volume, pages, year)
  where
    (authors, info) = RE.split ".+\\s+JMLR\\s*;?" . extract $ tags
    pat = infoPattern
    result = RE.matchSubex pat info
    volume = lookup "vol" result
    pages = (lookup "first" result, lookup "last" result)
    year = lookup "year" result

-- Horrible bibliographic information extraction regular expression
infoPattern = ".*\\s+({vol}\\d+):\\s*({first}\\d+\\.?\\d*)\\s*[^0-9]+\\s*({last}\\d+\\.?\\d*)\\s*,\\s*({year}\\d{4}).*" 

-- Tidies up strings 
extract :: [Tag String] -> String
extract = 
  replace "\" " "'' " . replace " \"" " ``" . -- Replace "quotes" with ``quotes''
  reverse . dropWhile isSpace . reverse . dropWhile isSpace .   -- Trim whitespace
  replace "**PARA**" "\n\n" . replace "\n" " " . replace "\n\n" "**PARA**" . 
  replace "\r\n" "\n" .
  innerText


--------------------------------------------------------------------------------
-- Test code
volReps = [
    "http://jmlr.org/proceedings/papers/v1/archambeau07a.html"
  , "http://jmlr.org/proceedings/papers/v2/cook07a.html"
  , "http://jmlr.org/proceedings/papers/v3/yin08a.html"
  , "http://jmlr.org/proceedings/papers/v4/zhao08a.html"
  , "http://jmlr.org/proceedings/papers/v5/eaton09a.html"
  , "http://jmlr.org/proceedings/papers/v6/tillman10a.html"
  , "http://jmlr.org/proceedings/papers/v7/doetsch09.html"
  , "http://jmlr.org/proceedings/papers/v8/nadeem10a.html"
  , "http://jmlr.org/proceedings/papers/v9/blanchard10a.html"
  , "http://jmlr.org/proceedings/papers/v10/gorodetsky10a.html"
  , "http://jmlr.org/proceedings/papers/v11/ali10a.html"
  , "http://jmlr.org/proceedings/papers/v12/popescu11.htm"  -- Hand fix
  , "http://jmlr.org/proceedings/papers/v13/sugiyama10b.html"
  , "http://jmlr.org/proceedings/papers/v14/gulin11a.html"  -- Hand fix
  , "http://jmlr.org/proceedings/papers/v15/lawrence11a.html"  -- Notable papers
  , "http://jmlr.org/proceedings/papers/v15/bengio11b.html" -- Long author list with unicode
  , "http://jmlr.org/proceedings/papers/v16/ho11a.html"  -- Same issue as v14
  , "http://jmlr.org/proceedings/papers/v17/zliobaite11a.html"
  , "http://jmlr.org/proceedings/papers/v18/xie12a.html" -- Same as v14
  , "http://jmlr.org/proceedings/papers/v19/amin11a.html"
  , "http://jmlr.org/proceedings/papers/v20/tran11.html" -- Same as v14
  , "http://jmlr.org/proceedings/papers/v21/eyraud12a.html"
  , "http://jmlr.org/proceedings/papers/v22/balakrishnan12b.html"
  , "http://jmlr.org/proceedings/papers/v23/gopalan12.html" -- No ; after author list
  , "http://jmlr.org/proceedings/papers/v24/valko12a.html"
  , "http://jmlr.org/proceedings/papers/v24/metzen12a.html"
  , "http://jmlr.org/proceedings/papers/v25/fan12.html"
  , "http://jmlr.org/proceedings/papers/v26/lovell12a.html"
  , "http://jmlr.org/proceedings/papers/v27/luxburg12a.html"  -- pages by hand? 
  ]

problemURLs = [
    "http://jmlr.org/proceedings/papers/v6/haufe10a.html"
  , "http://jmlr.org/proceedings/papers/v6/mani10a.html"
  , "http://jmlr.org/proceedings/papers/v6/tillman10a.html"
  , "http://jmlr.org/proceedings/papers/v6/mooij10a.html"
  , "http://jmlr.org/proceedings/papers/v6/zhou10a.html"
  , "http://jmlr.org/proceedings/papers/v6/voortman10a.html" 
  , "http://jmlr.org/proceedings/papers/v5/crammer09a.html"
  , "http://jmlr.org/proceedings/papers/v2/agarwal07a.html"
  , "http://jmlr.org/proceedings/papers/v1/archambeau07a.html"
  ]
