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
       , Coord
       , Move(..)
       , corners
       , edges
       , freeCells
       , maybeMove
       , move
       , movesByChar
       , placeRandom
       , placeRandom'
       , placeTile
       , row, col
       , rows, cols
       , show2D
       , size
       , start
       , straits
       , tileAt
       , zero
       )
       where

import Control.DeepSeq (NFData(..))
import Control.Monad (liftM)
import Control.Monad.State (MonadState, state)
import Data.Foldable (Foldable(..))
import Data.List (findIndices, transpose)
import Data.Maybe (fromJust)
import Prelude hiding (Left, Right, foldr)
import System.Random (RandomGen, random)
import Tile
import Util (Zero(..), padLeft, update, replace, every, choose)

newtype Board' a = Board {unBoard :: [[a]]}
  deriving (Show, Eq)

type Board = Board' Tile

data Coord = Coord { row, col :: Int }
           deriving Eq

size :: Coord
size = Coord 4 4

instance Show Coord where
  show (Coord i j) = show (i,j)

instance Bounded Coord where
  minBound = Coord 0 0
  maxBound = Coord (row size - 1) (col size - 1)

instance Enum Coord where
  fromEnum (Coord r c) = r * (col size) + c
  toEnum i = k $ i `divMod` (col size)
    where k (r,_) | r < row minBound = err
          k (r,_) | r > row maxBound = err
          k (_,c) | c < col minBound = err
          k (_,c) | c > col maxBound = err
          k (r,c) = Coord r c
          err = error "Out of bounds"

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound

rowRange, colRange :: [Int]
rowRange = [0 .. row maxBound]
colRange = [0 .. col maxBound]

rows, cols, straits :: [[Coord]]
rows = map (\r -> map (Coord r) colRange) rowRange
cols = map (\c -> map (flip Coord c) rowRange) colRange
straits = rows ++ cols

type Corner = (Coord, ([Coord], [Coord]))

edges :: [Corner]
edges = [(Coord 0 0, (top, left)),
         (Coord 0 c, (reverse top, right)),
         (Coord r 0, (bottom, reverse left)),
         (Coord r c, (reverse bottom, reverse right))]
  where r      = row maxBound
        c      = col maxBound
        top    = map (Coord 0) colRange
        bottom = map (Coord r) colRange
        left   = map (flip Coord 0) rowRange
        right  = map (flip Coord c) rowRange

corners :: [Coord]
corners = map fst edges

instance Zero a => Zero (Board' a) where
  zero = Board $ replicate (row size) $ replicate (col size) zero

instance Foldable Board' where
  foldr f z = foldr (flip (foldr f)) z . unBoard

show2D :: Board -> String
show2D = unlines . map each . unBoard
  where each = concat . map (padLeft 6 . show)

tileAt :: Board -> Coord -> Tile
tileAt b (Coord i j) = unBoard b !! i !! j

placeTile :: Tile -> Coord -> Board -> Board
placeTile t (Coord i j) = Board . update (replace t j) i . unBoard

freeCells :: Board -> [Coord]
freeCells = concat . zipWith f [0..] . unBoard
  where f i = map (Coord i) . findIndices isEmpty

data Move = Left | Right | Up | Down
  deriving (Enum, Bounded, Show, Eq)

instance NFData Move where

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

    Coord r c = size

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
