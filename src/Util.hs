module Util where

import Control.Monad.State (MonadState, state)
import Control.Monad (liftM)
import System.Random (RandomGen, randomR)

class Zero a where
  zero :: a

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

choose :: (RandomGen g, MonadState g m) => [a] -> m a
choose xs = (xs !!) `liftM` state (randomR (0, length xs - 1))

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
