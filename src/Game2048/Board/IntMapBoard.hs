{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Game2048.Board.IntMapBoard(BoardT, BoardT') where

import           Data.Foldable (Foldable)
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map
import           Game2048.Board.Base
import           Game2048.Coord
import           Game2048.Tile
import           Game2048.Util
import           Prelude hiding (Left, Right)

newtype BoardT' a = Board {unBoard :: IntMap a}
                 deriving (Eq, Show, Foldable)
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

  placeTile t c = Board . Map.insert (fromEnum c) t . unBoard

  show2D b = unlines (map eachRow rows)
    where eachRow = concat . map eachCol
          eachCol = padLeft 6 . show . tileAt b

  tileAt b c = Map.findWithDefault zero (fromEnum c) (unBoard b)
