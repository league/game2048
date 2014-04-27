{- Game2048.AI • deciding which move is best
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game2048.AI where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Control.Parallel.Strategies
import Data.Foldable (foldr, foldr1)
import Data.IORef
import Data.Maybe (mapMaybe, isNothing)
import Data.Time.Clock
import Game2048.Board.Base
import Game2048.Coord
import Game2048.Tile
import Game2048.Util
import Prelude hiding (foldr, foldr1)
import System.Random (RandomGen)

{- Reward making rows (respectively, columns) monotonic. The ideal situation
is having all rows monotonic in the SAME direction, but mixed monotonicity is
better than non-monotonicity. -}

mono :: Board b Tile => [[Coord]] -> b Tile -> [Ordering]
mono cc b = map (monotonicity . filter p . map (tileAt b)) cc
  where p = not . isEmpty

monoFactor :: [Ordering] -> Float
monoFactor = (/k) . fromIntegral . countIf (/= EQ)
  where k = fromIntegral (length straits)

sameFactor :: [Ordering] -> [Ordering] -> Float
sameFactor ro co = fromIntegral (rs + cs) / k
  where rs' = countSame ro
        cs' = countSame co
        rs = if rs' > 2 then rs' else 0
        cs = if cs' > 2 then cs' else 0
        k = fromIntegral (length straits)
        countSame os = max (g LT) (g GT)
          where g o = countIf (== o) os

{- Minimize number of tiles on the grid -}

elbowRoomFactor :: Board b Tile => b Tile -> Float
elbowRoomFactor b = fromIntegral (freeCount b) / fromIntegral gridSize

{- Keep largest tiles on the same edge -}

bigEnough :: Tile -> Bool
bigEnough t = index t > 3  -- don't bother with 2,4,8

largest :: Board b Tile => Int -> b Tile -> [Tile]
largest n = take n . rsort . filter bigEnough . foldr (:) []

howManyPerEdge :: Board b Tile => b Tile -> [Tile] -> [Coord] -> Int
howManyPerEdge b ts = length . filter p
  where p c = tileAt b c `elem` ts

edgeFactor :: Board b Tile => b Tile -> Float
edgeFactor b = fromIntegral(foldr1 max es) / fromIntegral n
  where n = 3
        es = map (howManyPerEdge b (largest n b)) edges

{- Combining above factors -}

data Factors = Factors { monoF, sameF, elbowF, edgeF :: Float }
             deriving Show

calc :: Board b Tile => b Tile -> Factors
calc b = Factors mf sf rf ef
  where mf = monoFactor so
        sf = sameFactor ro co
        rf = elbowRoomFactor b
        ef = edgeFactor b
        so = ro ++ co
        ro = mono rows b
        co = mono cols b

boardScore :: Board b Tile => b Tile -> Float
boardScore b = 2 * monoF f +
               3 * sameF f +
               3 * elbowF f +
               2 * edgeF f
  where f = calc b

allPlaces :: Board b Tile => b Tile -> [b Tile]
allPlaces b = map (g one) cs
  where g t ij = placeTile t ij b
        cs = freeCells b

deepScore :: Board b Tile => Int -> b Tile -> Float
deepScore 0 = boardScore
deepScore d = k . map snd . mapMaybe f . {-everyOther .-} allPlaces
  where f = best . scoreMoves' (d-1)
        k [] = -1
        k xs = foldr1 min xs    -- take the worst case

scoreMoves', scoreMoves :: Board b Tile => Int -> b Tile -> [(Move, Float)]
scoreMoves' d b = map f $ mapMaybe (maybeMove b) every
  where f = mapSnd $ deepScore d

scoreMoves d b = scoreMoves' d b `using` parList rdeepseq

best :: [(Move, Float)] -> Maybe (Move, Float)
best [] = Nothing
best xs = Just $ foldr1 (mapIf snd (>)) xs

type Status = Maybe (Int, NominalDiffTime)

auto' :: (RandomGen g, MonadState g m, MonadIO m, Board b Tile) =>
         UTCTime -> IORef Status -> IORef Int -> Int -> b Tile -> m ()
auto' begin status count depth board = loop board
  where
    loop b = case best (scoreMoves depth b) of
      Nothing -> liftIO $ putStrLn "GAME OVER"
      Just mk -> do
        i <- modifyReturnIORef count (+1)
        liftIO $ putStrLn $ show2D b ++ show i ++ ": " ++ show mk
        let b' = move b (fst mk)
        st <- liftIO $ readIORef status
        when (isNothing st && foldr1 max b == goal) $ liftIO $ do
          end <- getCurrentTime
          putStrLn "========================= CONGRATS!"
          writeIORef status (Just (i, diffUTCTime end begin))
        mb <- placeRandom b'
        maybe (liftIO $ putStrLn "GAME OVER!") loop mb

auto :: (RandomGen g, MonadState g m, MonadIO m, Board b Tile) => Int -> b Tile -> m ()
auto depth board = do
  begin  <- liftIO getCurrentTime
  status <- liftIO $ newIORef Nothing
  count  <- liftIO $ newIORef 0
  auto' begin status count depth board
  st <- liftIO $ readIORef status
  case st of
    Nothing -> return ()
    Just (steps, elapsed) -> liftIO $ do
      putStrLn $ "Reached " ++ show goal ++ " after " ++ show steps ++
                 " steps and " ++ show elapsed

{-
434: (Up,4.666667)
     2    16    32    64
     4    32    64   128
     -     8    32   512
     2     4     8    64
GAME OVER

Here is an example of a board where it didn't get very far, because we
apparently didn't prioritize aligning numbers that COULD be joined: look at
those two 64s and two 32s taking up space.
-}
