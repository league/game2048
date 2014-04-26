{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Game2048.Board.Base
       ( Move(..)
       , Board
       , Board'(..)
       , edges
       , movesByChar
       , placeRandom
       , rows, cols
       , rowsRev, colsRev
       , squeeze
       , start
       , straits
       ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (liftM)
import Control.Monad.State (MonadState, state)
import Data.Foldable (Foldable(..))
import Data.Maybe (fromJust)
import Game2048.Coord
import Game2048.Tile
import Game2048.Util
import System.Random (RandomGen, Random, random)

data Move = Left | Right | Up | Down
  deriving (Enum, Bounded, Show, Eq)

instance NFData Move where

movesByChar :: [(Char, Move)]
movesByChar = map f every
  where f :: Move -> (Char, Move)
        f m = (head (show m), m)

class Foldable b => Board' b where
  freeCells :: b Tile -> [Coord]
  fromList  :: [[Tile]] -> b Tile
  move      :: b Tile -> Move -> b Tile
  placeTile :: Tile -> Coord -> b Tile -> b Tile
  show2D    :: b Tile -> String
  tileAt    :: b Tile -> Coord -> Tile

  freeCount :: b Tile -> Int
  freeCount = length . freeCells

  maybeMove :: Board b Tile => b Tile -> Move -> Maybe (Move, b Tile)
  maybeMove b m = if b' == b then Nothing else Just (m, b')
    where b' = move b m

type Board b t = (Board' b, Eq (b t), Zero (b t))

squeeze' :: Int -> [Tile] -> [Tile]
squeeze' k = loop k . filter (not . isEmpty)
  where loop n [] = replicate n zero
        loop n (t1:t2:ts') | t1 == t2 = succ t1 : loop (n-1) ts'
        loop n (t:ts) = t : loop (n-1) ts

squeeze :: Int -> [(a,Tile)] -> [(a,Tile)]
squeeze k cts = zip (map fst cts) $ squeeze' k $ map snd cts

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

placeRandom :: (RandomGen g, MonadState g m, Board b Tile)
               => b Tile -> m (Maybe (b Tile))
placeRandom b = case freeCells b of
  [] -> return Nothing
  cs -> do
    c <- choose cs
    t <- state random
    return $ Just $ placeTile t c b

placeRandom' :: (RandomGen g, MonadState g m, Board b Tile) => b Tile -> m (b Tile)
placeRandom' = liftM fromJust . placeRandom

start :: (RandomGen g, MonadState g m, Board b Tile) => m (b Tile)
start = placeRandom' zero >>= placeRandom'
