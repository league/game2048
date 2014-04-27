{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Game2048.Board.VectorBoard(BoardT, BoardT') where

import Data.Foldable (Foldable(..))
import Data.Vector as V hiding (concat)
import Game2048.Board.Base
import Game2048.Coord
import Game2048.Tile
import Game2048.Util

newtype BoardT' a = Board {unBoard :: Vector a}
                    deriving (Eq, Show)
type BoardT = BoardT' Tile

instance Zero BoardT where
  zero = Board $ V.replicate gridSize zero

instance Foldable BoardT' where
  foldr f z b = V.foldr f z (unBoard b)
  foldr1 f b = V.foldr1 f (unBoard b)

instance Board' BoardT' where

  tileAt b c = unBoard b ! fromEnum c

  fromList = Board . V.fromList . concat

  placeTile t c b = Board $ unBoard b // [(fromEnum c, t)]

  freeCells b = V.ifoldr f [] (unBoard b)
    where f i t xs =
            if isEmpty t then toEnum i : xs
            else xs

  move b m = Board $ unBoard zero // moveViaCoordLists b m
