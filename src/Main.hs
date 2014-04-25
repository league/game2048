module Main where

import Control.Applicative ((<$>))
import Control.Monad.Trans.State (evalStateT)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Game
import System.Random (StdGen, mkStdGen)

seedRand :: IO StdGen
seedRand = mkStdGen . fromEnum . utctDayTime <$> getCurrentTime

main :: IO ()
main = seedRand >>= evalStateT (start >>= loop)
