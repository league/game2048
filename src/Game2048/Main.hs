{- Game2048.Main • main program launcher
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Game2048.Main where

import Control.Applicative ((<$>))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Game2048.AI (auto)
import Game2048.Board.Base (start)
import Game2048.Board.VectorBoard (BoardT)
import Game2048.Game (loop)
import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen)

seedRand :: IO StdGen
seedRand = mkStdGen . fromEnum . utctDayTime <$> getCurrentTime

runRand :: StateT StdGen IO a -> IO a
runRand a = seedRand >>= evalStateT a

plausibleDepth :: Int
plausibleDepth = 3


main :: IO ()
main = do
  args <- getArgs
  runRand $ do
    b :: BoardT <- start
    prog args b
  where prog ("auto":_) = auto plausibleDepth
        prog _ = loop plausibleDepth