{- Game2048.Coord • representation of 2D coordinates
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

module Game2048.Coord
       ( Coord
       , coord
       , gridSize
       , row, col
       , rowSize, colSize
       ) where

newtype Coord = Coord { asInt :: Int }
                deriving Eq

rowSize, colSize, gridSize :: Int
rowSize = 4
colSize = 4
gridSize = rowSize * colSize

coord :: Int -> Int -> Coord
coord i j = Coord (i * colSize + j)

row, col :: Coord -> Int
row c = asInt c `div` colSize
col c = asInt c `mod` colSize

asPair :: Coord -> (Int,Int)
asPair c = asInt c `divMod` colSize

instance Show Coord where
  show = show . asPair

instance Bounded Coord where
  minBound = coord 0 0
  maxBound = coord (rowSize - 1) (colSize - 1)

instance Enum Coord where
  fromEnum = asInt
  toEnum = Coord  -- could error-check

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound
