{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Data.Time.Clock
import Game2048.AI (scoreMoves)
import Game2048.Board.Base
import Game2048.Board.IntMapBoard as IMB
import Game2048.Board.ListBoard as LB
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
  run "ListBoard" (fromList :: FLT LB.BoardT')
  run "IntMapBoard" (fromList :: FLT IMB.BoardT')
