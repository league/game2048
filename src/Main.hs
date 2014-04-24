module Main where

import Board
import Tile

sample :: Board
sample = placeTile one (2,1) $ placeTile two (3,0) zero

main :: IO ()
main = putStrLn $ show2D sample
