{- Game2048.Board.VectorBoard • represent grid as a Vector
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game2048.Board.VectorBoard(BoardT, BoardT') where

import Data.Vector.Unboxed as V hiding (concat)
import Game2048.Board.Base
import Game2048.Coord
import Game2048.Tile
import Game2048.Util

newtype BoardT' a = Board {unBoard :: Vector a}
                    deriving (Eq, Show)
type BoardT = BoardT' Tile

instance Zero BoardT where
  zero = Board $ V.replicate gridSize zero

instance Board' BoardT' where

  tileAt b c = unBoard b ! fromEnum c

  fromList = Board . V.fromList . concat

  placeTile t c b = Board $ unBoard b // [(fromEnum c, t)]

  freeCells b = V.ifoldr f [] (unBoard b)
    where f i t xs =
            if isEmpty t then toEnum i : xs
            else xs

  move b m = Board $ unBoard zero // moveViaCoordLists b m

  foldr f z = V.foldr f z . unBoard
  foldr1 f = V.foldr1 f . unBoard
