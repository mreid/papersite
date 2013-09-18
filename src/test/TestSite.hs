{-# LANGUAGE OverloadedStrings #-}
module Main where

import Paper
import Site
import Hakyll.Core.Compiler
import Hakyll.Core.Item

import Data.Maybe
import Control.Applicative ((<$>), (<|>), empty)
import Test.Framework
import Test.Framework.Providers.HUnit 
import qualified Test.HUnit as H
import System.Exit (exitFailure)

type Name = String

testField :: FilePath -> Name -> String -> Maybe String -> Test
testField name path key expected = 
  buildTest $ do 
    readFile ("src/test/data/" ++ name)
    >>= return . getField' key . parseEntry
    >>= return . testCase name . H.assertEqual name expected

testID        = testField "single.bib" "ID"         "identifier"  (Just "feraud12") 
testFirstPage = testField "single.bib" "First Page" "firstpage"   (Just "129")
testLastPage  = testField "single.bib" "Last Page"  "lastpage"    (Just "143")
testTitle     = testField "single.bib" "Title"      "title"     
                          (Just "A Stochastic Bandit Algorithm for Scratch Games")
testNotThere  = testField "single.bib" "Fooble"     "fooble"      Nothing

tests = [ testGroup "Test Parsed Entry Fields" 
            [ testID, testFirstPage, testLastPage, testTitle, testNotThere ],
          testGroup "Simple Entry Supplementary"
            [ testCase "No Supp" 
              . H.assertEqual "No" Nothing 
              . maybe Nothing parseSupplementary 
              . getField' "supplementary" . parseEntry $ entryNoSupp,
              testCase "Has Supp" 
              . H.assertEqual "Supp" (Just (Supplementary "Supplementary" "abernethy13-supp.pdf")) 
              . maybe Nothing parseSupplementary 
              . getField' "supplementary" . parseEntry $ entrySupp ]
        ]

main :: IO ()
main = defaultMain tests

entryNoSupp = "@InProceedings{test13, author = {Reid, Mark and Smith, John}, title = {A Test}, abstract = {Yes.}}"
entrySupp = "@InProceedings{test13, author = {Reid, Mark and Smith, John}, title = {A Test}, abstract = {Yes.}, supplementary={Supplementary:abernethy13-supp.pdf} }"
