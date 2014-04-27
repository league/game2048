{- Game2048.Board.ListBoard • represent grid as list of lists
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Game2048.Board.ListBoard(BoardT, BoardT') where

import Data.Foldable (Foldable(..))
import Data.List (findIndices, transpose)
import Game2048.Board.Base
import Game2048.Coord
import Game2048.Tile
import Game2048.Util
import Prelude as P hiding (Left, Right)

newtype BoardT' a = Board {unBoard :: [[a]]}
                    deriving (Eq, Show, Foldable)
type BoardT = BoardT' Tile

instance Zero BoardT where
  zero = Board $ replicate rowSize $ replicate colSize zero

instance Board' BoardT' where
  fromList = Board

  tileAt b c = unBoard b !! row c !! col c

  placeTile t c = Board . update (replace t j) i . unBoard
    where i = row c
          j = col c

  freeCells = concat . zipWith f [0..] . unBoard
    where f i = map (coord i) . findIndices isEmpty

  move (unBoard -> b) m = Board (f m b)
    where
      f Left  = map (squeezeL colSize)
      f Right = map (squeezeR colSize)
      f Up    = transpose . map (squeezeL rowSize) . transpose
      f Down  = transpose . map (squeezeR rowSize) . transpose

      squeezeR k = reverse . squeezeL k . reverse

      squeezeL k = loop k . filter (not . isEmpty)
        where loop n [] = replicate n zero
              loop n (t1:t2:ts') | t1 == t2 = succ t1 : loop (n-1) ts'
              loop n (t:ts) = t : loop (n-1) ts

  foldr f z b = P.foldr (\r c -> P.foldr f c r) z (unBoard b)
