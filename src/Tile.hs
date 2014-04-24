{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Tile(Tile, zero, one, two, isEmpty, score) where

import Data.Bits (shiftL)
import Data.Word (Word8)
import System.Random (Random(..))
import Util (Zero(..))

newtype Tile = Tile Word8
  deriving (Enum, Eq)

instance Zero Tile where
  zero = Tile 0

one, two :: Tile
one = Tile 1
two = Tile 2

instance Random Tile where
  random g = (if p < 0.1 then two else one, g')
    where (p :: Float, g') = random g
  randomR = undefined

instance Show Tile where
  show (Tile 0) = "-"
  show (Tile k) = show n
    where n :: Word8 = shiftL 1 $ fromIntegral k

isEmpty :: Tile -> Bool
isEmpty (Tile 0) = True
isEmpty _ = False

score :: Tile -> Int
score (Tile (fromIntegral -> k)) = k * (shiftL 1 k)
