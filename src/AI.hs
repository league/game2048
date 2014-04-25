{- AI • deciding which move is best
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}
{-# LANGUAGE ScopedTypeVariables #-}

module AI where

import Board
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Foldable (foldr1)
import Data.IORef
import Data.Maybe (mapMaybe)
import Prelude hiding (foldr1)
import System.Random (RandomGen)
import Tile (Tile, isEmpty, one)
import Util (every, everyOther, mapSnd, mapIf, countIf, monotonicity)

{- Reward making rows (respectively, columns) monotonic. The ideal situation
is having all rows monotonic in the SAME direction, but mixed monotonicity is
better than non-monotonicity. -}

stripEmpty :: [Tile] -> [Tile]
stripEmpty = filter (not . isEmpty)

mono :: [[Coord]] -> Board -> [Ordering]
mono cc b = map (monotonicity . stripEmpty . map (tileAt b)) cc

monoFactor :: [Ordering] -> Float
monoFactor = (/k) . fromIntegral . countIf (/= EQ)
  where k = fromIntegral (length straits)

countSame :: [Ordering] -> Int
countSame os = max (k LT) (k GT)
  where k o = countIf (== o) os

sameFactor :: [Ordering] -> [Ordering] -> Float
sameFactor ro co = fromIntegral (rs + cs) / k
  where rs' = countSame ro
        cs' = countSame co
        rs = if rs' > 2 then rs' else 0
        cs = if cs' > 2 then cs' else 0
        k = fromIntegral (length straits)

{- Minimize number of tiles on the grid -}

elbowRoomFactor :: Board -> Float
elbowRoomFactor b = fromIntegral n / fromIntegral k
  where n = length (freeCells b)
        k = row size * col size

stats :: Board -> String
stats b = "monoFactor " ++ show mf ++ " sameFactor " ++ show sf ++
          " elbowRoomFactor " ++ show ef ++ "\n"
  where mf = monoFactor so
        sf = sameFactor ro co
        ef = elbowRoomFactor b
        so = ro ++ co
        ro = mono rows b
        co = mono cols b

boardScore :: Board -> Float
boardScore b = monoFactor so +
               2 * sameFactor ro co +
               3 * elbowRoomFactor b
  where so = ro ++ co
        ro = mono rows b
        co = mono cols b

allPlaces :: Board -> [Board]
allPlaces b = map (g one) cs
  where g t ij = placeTile t ij b
        cs = freeCells b

deepScore :: Int -> Board -> Float
deepScore 0 = boardScore
deepScore d = k . map snd . mapMaybe f . everyOther . allPlaces
  where f = best . scoreMoves (d-1)
        k [] = -1
        k xs = foldr1 min xs    -- take the worst case

scoreMoves :: Int -> Board -> [(Move, Float)]
scoreMoves d b = map f $ mapMaybe (maybeMove b) every
  where f = mapSnd $ deepScore d

best :: [(Move, Float)] -> Maybe (Move, Float)
best [] = Nothing
best xs = Just $ foldr1 (mapIf snd (>)) xs

incr :: IORef Int -> IO Int
incr ref = do
  i <- readIORef ref
  let j = i + 1
  writeIORef ref j
  return j

auto :: (RandomGen g, MonadState g m, MonadIO m) => Int -> Board -> m ()
auto d b0 = do
  count <- liftIO $ newIORef 0
  loop count b0
  where
    loop count b = case best (scoreMoves d b) of
      Nothing -> liftIO $ putStrLn "GAME OVER"
      Just mk -> do
        i <- liftIO $ incr count
        liftIO $ putStrLn $ show2D b ++ show i ++ ": " ++ show mk
        mb <- placeRandom $ move b (fst mk)
        maybe (liftIO $ putStrLn "GAME OVER!") (loop count) mb
