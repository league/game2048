{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort)
import Game2048.Util
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.Framework.TH (defaultMainGenerator)

main :: IO ()
main = $(defaultMainGenerator)

prop_rsort_is_reverse_sort :: [Int] -> Bool
prop_rsort_is_reverse_sort xs = rsort xs == reverse (sort xs)
