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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Board
       ( Board
       , Move(..)
       , edges
       , freeCells
       , freeCount
       , fromList
       , maybeMove
       , move
       , movesByChar
       , placeRandom
       , placeTile
       , rows, cols
       , show2D
       , start
       , straits
       , tileAt
       )
       where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM)
import           Control.Monad.State (MonadState, state)
import           Coord
import           Data.Foldable (Foldable)
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map
import           Data.Maybe (fromJust)
import           Prelude hiding (Left, Right, foldr)
import           System.Random (RandomGen, random)
import           Tile
import           Util

newtype Board' a = Board {unBoard :: IntMap a}
                 deriving (Eq, Show, Foldable)

type Board = Board' Tile

instance Zero a => Zero (Board' a) where
  zero = Board Map.empty

fromList :: [[Tile]] -> Board
fromList tt = Board $ Map.fromList $ foldlWithIndex (foldlWithIndex . f) [] tt
  where f i j cts t =
          if isEmpty t then cts
          else (fromEnum(coord i j), t) : cts

rowRange, colRange :: [Int]
rowRange = [0 .. row maxBound]
colRange = [0 .. col maxBound]

rows, cols, rowsRev, colsRev, straits, edges :: [[Coord]]

rows = map (\i -> map (coord i) colRange) rowRange
cols = map (\j -> map (flip coord j) rowRange) colRange
straits = rows ++ cols

rowsRev = map reverse rows
colsRev = map reverse cols

edges = [top, bottom, left, right]
  where r      = row maxBound
        c      = col maxBound
        top    = map (coord 0) colRange
        bottom = map (coord r) colRange
        left   = map (flip coord 0) rowRange
        right  = map (flip coord c) rowRange

tileAt :: Board -> Coord -> Tile
tileAt b c = Map.findWithDefault zero (fromEnum c) (unBoard b)

show2D :: Board -> String
show2D b = unlines (map eachRow rows)
  where eachRow = concat . map eachCol
        eachCol = padLeft 6 . show . tileAt b

placeTile :: Tile -> Coord -> Board -> Board
placeTile t c = Board . Map.insert (fromEnum c) t . unBoard

freeCount :: Board -> Int
freeCount b = gridSize - Map.size (unBoard b)

freeCells :: Board -> [Coord]
freeCells (unBoard -> b) = filter p every
  where p c = Map.notMember (fromEnum c) b

data Move = Left | Right | Up | Down
  deriving (Enum, Bounded, Show, Eq)

instance NFData Move where

movesByChar :: [(Char, Move)]
movesByChar = map f every
  where f :: Move -> (Char, Move)
        f m = (head (show m), m)

squeeze' :: Int -> [Tile] -> [Tile]
squeeze' k = loop k . filter (not . isEmpty)
  where loop n [] = replicate n zero
        loop n (t1:t2:ts') | t1 == t2 = succ t1 : loop (n-1) ts'
        loop n (t:ts) = t : loop (n-1) ts

squeeze :: Int -> [(a,Tile)] -> [(a,Tile)]
squeeze k cts = zip (map fst cts) $ squeeze' k $ map snd cts

move :: Board -> Move -> Board
move b m = Board $ Map.fromList $ filter p $ concat $ map (squeeze k) $ cts
  where
    cts = map (map f) cc
    f c = (fromEnum c, tileAt b c)
    p = not . isEmpty . snd
    (k, cc) = case m of
          Left  -> (rowSize, rows)
          Right -> (rowSize, rowsRev)
          Up    -> (colSize, cols)
          Down  -> (colSize, colsRev)

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

{-
     -     -     -     -
     -     -     -     -
     -     -     -     2
     2     -     -     8

let bl = fromList $ map (map Tile) [[0,0,0,0],[0,0,0,0],[0,0,0,1],[1,0,0,3]]

-}
