{- Game2048.Board.IntMapBoard • represent grid as IntMap (patricia tree)
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Game2048.Board.IntMapBoard(BoardT, BoardT') where

import qualified Data.Foldable as F
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map
import           Game2048.Board.Base
import           Game2048.Coord
import           Game2048.Tile
import           Game2048.Util
import           Prelude hiding (Left, Right)

newtype BoardT' a = Board {unBoard :: IntMap a}
                 deriving (Eq, Show, F.Foldable)
type BoardT = BoardT' Tile

instance Zero BoardT where
  zero = Board Map.empty

instance Board' BoardT' where
  freeCells (unBoard -> b) = filter p every
    where p c = Map.notMember (fromEnum c) b

  freeCount b = gridSize - Map.size (unBoard b)

  fromList tt = Board $ Map.fromList $ foldlWithIndex (foldlWithIndex . f) [] tt
    where f i j cts t =
            if isEmpty t then cts
            else (fromEnum(coord i j), t) : cts

  move b = Board . Map.fromList . moveViaCoordLists b

  placeTile t c = Board . Map.insert (fromEnum c) t . unBoard

  tileAt b c = Map.findWithDefault zero (fromEnum c) (unBoard b)

  foldr = F.foldr
  foldr1 = F.foldr1
