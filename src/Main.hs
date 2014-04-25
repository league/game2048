{- Main • main program launcher
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

module Main where

import AI (auto)
import Board (start)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Game (loop)
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
  runRand (start >>= prog args)
  where prog ("auto":_) = auto plausibleDepth
        prog _ = loop plausibleDepth
