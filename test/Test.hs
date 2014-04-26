{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort)
import Game2048.AI (scoreMoves)
import Game2048.Board as Board
import Game2048.Util
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit (assert)

main :: IO ()
main = $(defaultMainGenerator)

prop_rsort_is_reverse_sort :: [Int] -> Bool
prop_rsort_is_reverse_sort xs = rsort xs == reverse (sort xs)

case_foldl_matches_indices =
  assert $ all (uncurry (==)) $ foldlWithIndex f [] [0..4]
  where f i ys x = (i,x):ys
