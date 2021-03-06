{- Game2048.Tile • representing individual tiles on the board
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Game2048.Tile
       ( Tile
       , distance
       , goal
       , index
       , isEmpty
       , log
       , merge
       , one
       , two
       , value
       , zero
       ) where

import           Data.Bits (shiftL)
import           Data.Vector.Generic.Base
import           Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word8, Word16)
import           Game2048.Util (Zero(..))
import           System.Random (Random(..))

newtype Tile = Tile {index :: Word8}
  deriving (Enum, Ord, Eq, Vector U.Vector, MVector U.MVector, U.Unbox)

instance Zero Tile where
  zero = Tile 0

one, two, goal :: Tile
one = Tile 1
two = Tile 2
goal = Tile 11

instance Random Tile where
  random g = (if p < 0.1 then two else one, g')
    where (p :: Float, g') = random g
  randomR = undefined

instance Show Tile where
  show (Tile 0) = "-"
  show t = show (value t)

isEmpty :: Tile -> Bool
isEmpty (Tile 0) = True
isEmpty _ = False

value :: Tile -> Word16
value (Tile (fromIntegral -> k)) = 1 `shiftL` k

merge :: Tile -> Tile -> Maybe Tile
merge t1 t2 | t1 == t2 = Just (succ t1)
merge _ _ = Nothing

distance :: Tile -> Tile -> Int
distance (Tile x) (Tile y) = fromIntegral x - fromIntegral y
