{- Coord • representation of 2D coordinates
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

module Coord
       ( Coord
       , coord
       , row, col
       , size
       ) where

data Coord = Coord { row, col :: Int }
           deriving Eq

coord :: Int -> Int -> Coord
coord = Coord

size :: Coord
size = Coord 4 4

instance Show Coord where
  show (Coord i j) = show (i,j)

instance Bounded Coord where
  minBound = Coord 0 0
  maxBound = Coord (row size - 1) (col size - 1)

instance Enum Coord where
  fromEnum (Coord r c) = r * col size + c
  toEnum i = k $ i `divMod` col size
    where k (r,_) | r < row minBound = err
          k (r,_) | r > row maxBound = err
          k (_,c) | c < col minBound = err
          k (_,c) | c > col maxBound = err
          k (r,c) = Coord r c
          err = error "Out of bounds"

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound
