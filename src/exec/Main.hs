import Data.List
import Site (realMain)
import System.Environment

-- Parses and modifies incoming args so as to selectively build site
main = do
  args <- getArgs
  putStrLn $ show args

  let (hakyllArgs, only) = break (== "only") args
  withArgs hakyllArgs $ realMain . makeRegex $ only

makeRegex :: [String] -> String
makeRegex []            = ".*"
makeRegex ("only":vols) = intercalate "|" . concatMap expandRegex $ vols
  where expandRegex v = [v++"/", v++".bib"]
