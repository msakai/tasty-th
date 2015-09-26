# tasty-th

Automatically generate tasty TestTrees from functions of the current module, using TemplateHaskell.
This is a fork the original [test-framework-th](https://github.com/finnsson/test-generator) package, modified to work with tasty instead of test-framework. Usage is exactly the same as for the orginal package.

### Usage

To use this package, you need to enable TH and import the package plus the required tasty modules:

```
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
```

Then, write a few unit tests and QuickCheck properties:

```
prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]
```

`tasty-th` assumes that all QuickCheck properties are named `prop_something` and HUnit tests are named `case_something`. You can also group tests under a test tree and have them be discovered by `tasty-th` so they are added to the main test tree. In that case, you can define a `test_something` definition:

```
test_plus :: [TestTree]
test_plus = 
  [ testCase "3 + 4" (7 @=? (3 + 4))
    -- ...
  ]
```

After you've defined all your tests in this way, you can now automatically generate a main function using the `defaultMainGenerator` TH macro provided by `tasty-th`:

```
main :: IO ()
main = $(defaultMainGenerator)
```

It is important that you place this at the end of the file, since GHC only sees the definitions prior to the TH call.

If you don't wish to generate a main function, but instead just want a TestTree, you can use the `testGroupGenerator` macro for that:

```
tests :: IO ()
tests = $(testGroupGenerator)
```

Running the example, we get the following output:

```
./example
Main
  length append: OK (0.01s)
    +++ OK, passed 100 tests.
  length 1:      OK
  plus
    3 + 4:       OK

All 3 tests passed (0.01s)
```

You can find this whole example in the file `example.hs` at the root of the source code tree.

### Contributing

If you have any questions or issues with the package, feel free to open an issue on the repo. Pull requests adding new features are also welcome if the features make sense. You can also find me in the #haskell IRC channel with the nick bennofs to ask a question about this package.
