{-# LANGUAGE TupleSections #-}

module Board
       ( Board
       , size
       , zero
       , show2D
       , placeTile
       , Board.score
       , freeCells
       , move
       , maybeMove
       )
       where

import Data.Foldable (Foldable(..))
import Data.List (findIndices, transpose)
import Data.Monoid (Sum(..))
import Prelude hiding (foldr)
import Tile
import Util (Zero(..), padLeft, update, replace)

newtype Board' a = Board {unBoard :: [[a]]}
  deriving (Show, Eq)

type Board = Board' Tile
type Coord = (Int,Int)

size :: Coord
size = (4, 6)

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

data Move = MoveLeft | MoveRight | MoveUp | MoveDown
  deriving (Enum, Bounded, Show, Eq)

move :: Move -> Board -> Board
move m = Board . f m . unBoard
  where
    f MoveLeft  = map (squeezeL c)
    f MoveRight = map (squeezeR c)
    f MoveUp    = transpose . map (squeezeL r) . transpose
    f MoveDown  = transpose . map (squeezeR r) . transpose

    (r,c) = size

    squeezeR k = reverse . squeezeL k . reverse

    squeezeL k = loop k . filter (not . isEmpty)
      where loop n [] = replicate n zero
            loop n (t1:t2:ts') | t1 == t2 = succ t1 : loop (n-1) ts'
            loop n (t:ts) = t : loop (n-1) ts

maybeMove :: Board -> Move -> Maybe (Move, Board)
maybeMove b m = if b' == b then Nothing else Just (m, b')
  where b' = move m b
