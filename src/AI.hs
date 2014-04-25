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
import Data.Foldable (foldr1, foldMap)
import Data.IORef
import Data.List (find)
import Data.Maybe (isJust)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Prelude hiding (foldr1)
import System.Random (RandomGen)
import Tile (Tile, one, index, value)
import Util (every, mapSnd, mapIf)

allPlaces :: Board -> [Board]
allPlaces b = map (g one) cs
  where g t ij = placeTile t ij b
        cs = freeCells b

tileScore :: Tile -> Int
tileScore t = i * v
  where i = fromIntegral $ index t
        v = fromIntegral $ value t

tileSums :: Board -> Int
tileSums = getSum . foldMap (Sum . tileScore)

biggestInCorner :: Board -> Maybe Coord
biggestInCorner b = find ok corners
  where ok c = tileAt b c == biggest
        biggest = foldr1 max b

descendingValues :: Board -> [Coord] -> Bool
descendingValues b cs = foldr1 (&&) $ zipWith f cs (tail cs)
  where f c1 c2 = tileAt b c1 >= tileAt b c2

rewardCorners :: Board -> Float
rewardCorners b = case biggestInCorner b of
  Nothing -> 1
  Just c -> case lookup c edges of
    Just (e1,e2) -> if g1 && g2 then 2 else if g1 || g2 then 1.5 else 1.2
      where g1 = descendingValues b e1
            g2 = descendingValues b e2

boardScore :: Board -> Float
boardScore b = multiplier * (fromIntegral $ tileSums b)
  where multiplier = rewardCorners b

deepScore :: Int -> Board -> Float
deepScore 0 = boardScore
deepScore d = k . map snd . mapMaybe f . allPlaces
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
