{- Board • representing and manipulating the grid
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

module Board
       ( Board
       , Board.score
       , Move(..)
       , freeCells
       , maybeMove
       , move
       , movesByChar
       , placeRandom
       , placeRandom'
       , placeTile
       , show2D
       , size
       , start
       , zero
       )
       where

import Control.Monad (liftM)
import Control.Monad.State (MonadState, state)
import Data.Foldable (Foldable(..))
import Data.List (findIndices, transpose)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..))
import Prelude hiding (Left, Right, foldr)
import System.Random (RandomGen, random)
import Tile
import Util (Zero(..), padLeft, update, replace, every, choose)

newtype Board' a = Board {unBoard :: [[a]]}
  deriving (Show, Eq)

type Board = Board' Tile
type Coord = (Int,Int)

size :: Coord
size = (4, 4)

instance Zero a => Zero (Board' a) where
  zero = Board $ replicate (fst size) $ replicate (snd size) zero

instance Foldable Board' where
  foldr f z = foldr (flip (foldr f)) z . unBoard

show2D :: Board -> String
show2D = unlines . map each . unBoard
  where each = concat . map (padLeft 6 . show)

score :: Board -> Int
score = getSum . foldMap (Sum . Tile.score)

placeTile :: Tile -> Coord -> Board -> Board
placeTile t (i,j) = Board . update (replace t j) i . unBoard

freeCells :: Board -> [Coord]
freeCells = concat . zipWith f [0..] . unBoard
  where f i = map (i,) . findIndices isEmpty

data Move = Left | Right | Up | Down
  deriving (Enum, Bounded, Show, Eq)

movesByChar :: [(Char, Move)]
movesByChar = map f every
  where f :: Move -> (Char, Move)
        f m = (head (show m), m)

move :: Board -> Move -> Board
move (unBoard -> b) m = Board (f m b)
  where
    f Left  = map (squeezeL c)
    f Right = map (squeezeR c)
    f Up    = transpose . map (squeezeL r) . transpose
    f Down  = transpose . map (squeezeR r) . transpose

    (r,c) = size

    squeezeR k = reverse . squeezeL k . reverse

    squeezeL k = loop k . filter (not . isEmpty)
      where loop n [] = replicate n zero
            loop n (t1:t2:ts') | t1 == t2 = succ t1 : loop (n-1) ts'
            loop n (t:ts) = t : loop (n-1) ts

maybeMove :: Board -> Move -> Maybe (Move, Board)
maybeMove b m = if b' == b then Nothing else Just (m, b')
  where b' = move b m

placeRandom :: (RandomGen g, MonadState g m) => Board -> m (Maybe Board)
placeRandom b = case freeCells b of
  [] -> return Nothing
  cs -> do
    c <- choose cs
    t <- state random
    return $ Just $ placeTile t c b

placeRandom' :: (RandomGen g, MonadState g m) => Board -> m Board
placeRandom' = liftM fromJust . placeRandom

start :: (RandomGen g, MonadState g m) => m Board
start = placeRandom' zero >>= placeRandom'
