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
  , testGroupGeneratorFor
  , defaultMainGeneratorFor
  , extractTestFunctions
  , locationModule
  ) where

import Control.Monad (join)
import Language.Haskell.TH
import Data.List
import Data.Maybe

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
defaultMainGenerator = [| defaultMain $(testGroupGenerator) |]

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
testGroupGenerator = join $ testGroupGeneratorFor <$> fmap loc_module location <*> testFunctions
 where
  testFunctions = location >>= runIO . extractTestFunctions . loc_filename

-- | Retrieves all function names from the given file that would be discovered by 'testGroupGenerator'.
extractTestFunctions :: FilePath -> IO [String]
extractTestFunctions filePath = do
  file <- readFile filePath
  let functions = map fst . concat . map lex . lines $ file
      filtered pattern = filter (pattern `isPrefixOf`) functions
  return . nub $ concat [filtered "prop_", filtered "case_", filtered "test_"]

-- | Extract the name of the current module.
locationModule :: ExpQ
locationModule = do
  loc <- location
  return $ LitE $ StringL $ loc_module loc

-- | Like 'testGroupGenerator', but generates a test group only including the specified function names.
-- The function names still need to follow the pattern of starting with one of @prop_@, @case_@ or @test_@.
testGroupGeneratorFor
  :: String   -- ^ The name of the test group itself
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
testGroupGeneratorFor name functionNames = [| testGroup name $(listE (mapMaybe test functionNames)) |]
 where
  testFunctions = [("prop_", "testProperty"), ("case_", "testCase"), ("test_", "testGroup")]
  getTestFunction fname = fmap snd $ find ((`isPrefixOf` fname) . fst) testFunctions
  test fname = do
    fn <- getTestFunction fname
    return $ appE (appE (varE $ mkName fn) (stringE (fixName fname))) (varE (mkName fname))

-- | Like 'defaultMainGenerator', but only includes the specific function names in the test group.
-- The function names still need to follow the pattern of starting with one of @prop_@, @case_@ or @test_@.
defaultMainGeneratorFor
  :: String   -- ^ The name of the top-level test group
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
defaultMainGeneratorFor name fns = [| defaultMain $(testGroupGeneratorFor name fns) |]

fixName :: String -> String
fixName = replace '_' ' ' . tail . dropWhile (/= '_')

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)
