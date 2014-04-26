{- Util • various helper functions
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

module Util where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, state)
import Data.IORef
import Data.List (sortBy)
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

average :: Fractional a => [a] -> a
average [] = -1
average as = sum as / fromIntegral (length as)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

mapIf :: (a -> b) -> (b -> b -> Bool) -> a -> a -> a
mapIf f p a1 a2 = if p (f a1) (f a2) then a1 else a2

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [a] = [a]
everyOther (a:_:as) = a : everyOther as

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f as = zipWith f as (tail as)

monotonicity :: Ord a => [a] -> Ordering
monotonicity xs =
  if not(steps GT) then LT      -- non-decreasing
  else if not(steps LT) then GT -- non-increasing
       else EQ                  -- non-monotonic
  where steps o = any (== o) os
        os = pairwise compare xs

countIf :: (a -> Bool) -> [a] -> Int
countIf ok = length . filter ok

modifyReturnIORef :: MonadIO m => IORef a -> (a -> a) -> m a
modifyReturnIORef ref f = liftIO $ do
  a <- readIORef ref
  let a' = f a
  writeIORef ref a'
  return a'

rsort :: Ord a => [a] -> [a]
rsort = sortBy r
  where r x y = case compare x y of
          EQ -> EQ
          GT -> LT
          LT -> GT
