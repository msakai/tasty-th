-----------------------------------------------------------------------------
--
-- Module      :  Test.Tasty.TH
-- Copyright   :  Oscar Finnsson, Benno Fünfstück
-- License     :  BSD3
--
-- Maintainer  :  Benno Fünfstück
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides TemplateHaskell functions to automatically generate
-- tasty TestTrees from specially named functions. See the README of the package
-- for examples.
--
-- Important: due to to the GHC staging restriction, you must put any uses of these
-- functions at the end of the file, or you may get errors due to missing definitions.
module Test.Tasty.TH
  ( testGroupGenerator
  , defaultMainGenerator
  ) where

import Language.Haskell.TH
import Language.Haskell.Extract

import Test.Tasty

-- | Convenience function that directly generates an `IO` action that may be used as the
-- main function. It's just a wrapper that applies 'defaultMain' to the 'TestTree' generated
-- by 'testGroupGenerator'.
--
-- Example usage:
--
-- @
-- -- properties, test cases, ....
--
-- main :: IO ()
-- main = $('defaultMainGenerator')
-- @
defaultMainGenerator :: ExpQ
defaultMainGenerator =
  [| defaultMain $ testGroup $(locationModule) $ $(propListGenerator) ++ $(caseListGenerator) ++ $(testListGenerator)|]

-- | This function generates a 'TestTree' from functions in the current module. 
-- The test tree is named after the current module.
--
-- The following definitions are collected by `testGroupGenerator`:
--
-- * a test_something definition in the current module creates a sub-testGroup with the name "something"
-- * a prop_something definition in the current module is added as a QuickCheck property named "something"
-- * a case_something definition leads to a HUnit-Assertion test with the name "something"
--
-- Example usage:
--
-- @
-- prop_example :: Int -> Int -> Bool
-- prop_example a b = a + b == b + a
--
-- tests :: 'TestTree'
-- tests = $('testGroupGenerator')
-- @
testGroupGenerator :: ExpQ
testGroupGenerator =
  [| testGroup $(locationModule) $ $(propListGenerator) ++ $(caseListGenerator) ++ $(testListGenerator) |]

listGenerator :: String -> String -> ExpQ
listGenerator beginning funcName =
  functionExtractorMap beginning (applyNameFix funcName)

propListGenerator :: ExpQ
propListGenerator = listGenerator "^prop_" "testProperty"

caseListGenerator :: ExpQ
caseListGenerator = listGenerator "^case_" "testCase"

testListGenerator :: ExpQ
testListGenerator = listGenerator "^test_" "testGroup"

applyNameFix :: String -> ExpQ
applyNameFix n = do
  fn <- [|fixName|]
  return $ LamE [VarP (mkName "n")] (AppE (VarE (mkName n)) (AppE fn (VarE (mkName "n"))))

fixName :: String -> String
fixName = replace '_' ' ' . tail . dropWhile (/= '_')

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)
