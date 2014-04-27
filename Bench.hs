{- Bench.hs • program for benchmarking AI search
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Data.Time.Clock
import Game2048.AI (scoreMoves)
import Game2048.Board.Base
import Game2048.Board.IntMapBoard as IMB
import Game2048.Board.ListBoard as LB
import Game2048.Board.VectorBoard as VB
import Game2048.Tile

type FLT b = [[Tile]] -> b Tile

run :: Board b Tile => String -> FLT b -> IO ()
run label fromL = do
  putStrLn label
  begin <- getCurrentTime
  let ts = [[2,0,5,6],[2,5,0,0],[0,3,0,2],[1,1,3,0]]
  let b = fromL $ map (map toEnum) ts
  putStrLn $ show $ scoreMoves 4 b
  end <- getCurrentTime
  putStrLn $ show (diffUTCTime end begin) ++ " elapsed."

main :: IO ()
main = do
  run "VectorBoard" (fromList :: FLT VB.BoardT')
  run "ListBoard"   (fromList :: FLT LB.BoardT')
  run "IntMapBoard" (fromList :: FLT IMB.BoardT')
