module Util where

import System.Random (RandomGen, Random(..))

class Zero a where
  zero :: a

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs g = (xs !! i, g')
  where (i, g') = randomR (0, length xs - 1) g

padLeft :: Int -> String -> String
padLeft n s = replicate i ' ' ++ s
  where k = length s
        i = if k > n then 0 else n-k

update :: (a -> a) -> Int -> [a] -> [a]
update _ _ [] = []
update f 0 (a:as) = f a : as
update f i (a:as) = a : update f (i-1) as

replace :: a -> Int -> [a] -> [a]
replace a = update (\_ -> a)
