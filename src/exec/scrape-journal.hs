module MainJournal (main, parseConf, parseOne, LinkType) where
import Control.Arrow
import Control.Monad (forM_, liftM3, ap, liftM)
import Control.Applicative 
import Control.Exception (SomeException, try, evaluate)
import Data.Char
import Data.List
import Data.Maybe
import Data.List.HT (replace, segmentBefore, chop, dropWhileRev)
import Network.HTTP
import Network.URI
import System.Directory
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Text.HTML.TagSoup
import qualified Data.Text as T
import qualified Text.BibTeX.Entry as Bib
import qualified Text.BibTeX.Format as Bib
import qualified Text.Regex.XMLSchema.String as RE

type PaperKey = String 
type Paper    = (PaperKey, LinkType -> Maybe URI)
type VolID    = String
data LinkType = Abstract | PDF | Supplementary deriving (Eq,Show)
data Volume = Volume {
  url     :: URI,
  papers  :: [Paper],
  orphans :: [[(LinkType, URI)]]
}

--------------------------------------------------------------------------------
-- Constants, etc.
targetDir   :: String
targetDir   = "db-scraped" 
baseURI     :: URI 
baseURI     = fromJust . parseAbsoluteURI $ "http://jmlr.org/papers/"
-- baseURI    = fromJust . parseAbsoluteURI $ "http://localhost/"
baseURIold :: URI
baseURIold = fromJust . parseAbsoluteURI $ "http://jmlr.csail.mit.edu/papers/"

volumes :: [ VolID ]
volumes = map (("v"++) . show) [1..17]

--------------------------------------------------------------------------------
-- Main
main :: IO ()
main = forM_ volumes $ \vID -> do
  let vURI = fromJust . parseRelativeReference . (++ "/") $ vID
  putStrLn $ "\nVolume " ++ (show vURI) ++ "\n---------------"

  let volDir = targetDir ++ "/" ++ vID
  createDirectoryIfMissing True volDir

  conf <- parseConf $ vURI `relativeTo` baseURI 
  forM_ (papers conf) $ \paper -> do
    bibOrNothing <- parseOne vID paper
    case bibOrNothing of
      Nothing -> putStrLn $ "--- ERROR: Could not parse " ++ vID ++ "/" ++ (fst paper)
      Just bib -> writeBib volDir bib 

writeBib :: String -> Bib.T -> IO ()
writeBib volDir bib = do
    putStrLn . Bib.identifier $ bib
    let filepath = volDir ++ "/" ++ (Bib.identifier bib) ++ ".bib"
    putStrLn filepath
    writeFile filepath $ Bib.entry bib

testOne :: VolID -> Int -> IO (Maybe Bib.T)
testOne vID n = do
  let vURI = fromJust . parseRelativeReference . (++ "/") $ vID
  conf <- parseConf $ vURI `relativeTo` baseURI
  parseOne vID $ (papers conf) !! n


-- ============================================================================

--------------------------------------------------------------------------------
-- Parse conference page of links
parseConf :: URI -> IO Volume
parseConf urlStart = do
  let links = fromUrl (show urlStart) >>> css "a"
 
  -- Filter links to those for abstracts, PDFs, or supplementary material
  materials <- runX $ links >>>
                  ((deep $ hasText isLink >>> getText >>> arr tagLink) &&& 
                   (getAttrValue "href" >>> arr (cleanUrl urlStart)) )

  -- Build list of related materials by partitioning on each Abstract
  let paperList = segmentBefore ((== Abstract) . fst) materials
  return $ makeVolume urlStart paperList

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
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- Get the raw tags for a paper URI
-- getTags :: String -> 
getTags absURL = do
  parseTags <$> openURL absURL


-- Parse a single JMLR abstract page for a paper.
parseOne :: VolID -> Paper -> IO (Maybe Bib.T)
parseOne vID (pID,links) = do
  let absURL = fromJust . links $ Abstract
  tags <- parseTags <$> openURL (show absURL)
  return $ constructBib vID (pID, links) tags

constructBib :: VolID -> Paper -> [Tag String] -> Maybe Bib.T
constructBib vID (pID, links) tags = do
  let title    = extract $ findTitle vID tags
  let abstract = deligature . extract . substitute $ findAbstract vID tags

  let pdfField = 
        case (links PDF) of
          Just pdfurl -> [("pdf", show pdfurl)]
          Nothing     -> []
   
  let suppField = 
        case (links Supplementary) of
          Just supurl -> [("supplementary", "Supplementary:"++(show supurl))]
          Nothing     -> []

  (authors,volume,pages,year) <- getBibInfo vID $ findInfo vID tags
  let bibEntry = Bib.Cons {
    Bib.entryType   = "InProceedings",
    Bib.identifier  = pID,
    Bib.fields      = [
      ("title",    title),
      ("author",   authors),
      ("pages",    (fst pages) ++ "--" ++ (snd pages)),
      ("abstract", abstract),
      ("year",     year),
      ("volume",   volume)
    ] ++ pdfField ++ suppField
  }
  
  return $ bibEntry

--------------------------------------------------------------------------------
-- Parsing Rules
-- Per volume rules for extracting title, abstract, and info lines from pages

-- Find title in a tag list
findTitle :: String -> [Tag String] -> [Tag String]
-- findTitle "v12" = flip (!!) 0 . partitions (~== "<p>")
-- findTitle "v14" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
-- findTitle "v16" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
-- findTitle "v18" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
-- findTitle "v20" = between (~== "<h2 class=titleHead>") (~== "</h2>") 
findTitle _     = between (~== "<h2") (~== "</h2") 

findInfo :: String -> [Tag String] -> [Tag String]
-- findInfo "v12"  = flip (!!) 1 . partitions (~== "<p>")
-- findInfo "v14"  = between (~== "<span") (~== "</div>") 
-- findInfo "v16"  = between (~== "<span") (~== "</div>") 
-- findInfo "v18"  = between (~== "<span") (~== "</div>") 
-- findInfo "v20"  = between (~== "<span") (~== "</div>")
findInfo _      = between (~== "</h2") (~== "Abstract") 

findAbstract :: String -> [Tag String] -> [Tag String]
-- findAbstract "v12"  = flip (!!) 3 . partitions (~== "<p>")
findAbstract _      = between (~== "Abstract") (~== "<p>") 

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
getBibInfo :: String -> [Tag String] -> Maybe (String, String, (String, String), String)
getBibInfo vID tags = do
    let (authors, info) = RE.split ".+\\s*;" . 
                          replace "\n\n" "" . extract $ tags
    let pat = infoPattern
    let result = RE.matchSubex pat info
    volume <- lookup "vol" result
    firstPage <- lookup "first" result
    lastPage <- lookup "last" result
    year <- lookup "year" result
    return (fixAuthors vID authors, volume, (firstPage, lastPage), year)
  where
-- Horrible bibliographic information extraction regular expression
-- infoPattern = ".*\\s+({vol}\\d+):\\s*({first}\\d+\\.?\\d*)\\s*[^0-9]+\\s*({last}\\d+\\.?\\d*)\\s*,\\s*({year}\\d{4}).*" 
infoPattern = "\\s*[^0-9]*({vol}[^:]+):\\s*({first}\\d+\\.?\\d*)\\s*[^0-9]+\\s*({last}\\d+\\.?\\d*)\\s*,\\s*({year}\\d{4}).*"

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
    "http://www.jmlr.org/papers/v5/evendar03a.html"
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
