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
import Data.IORef
import Data.Maybe (mapMaybe)
import System.Random (RandomGen)
import Tile (one)
import Util (every, mapSnd, mapIf)


allPlaces :: Board -> [Board]
allPlaces b = map (g one) cs
  where g t ij = placeTile t ij b
        cs = freeCells b

foldr1' :: String -> (a -> a -> a) -> [a] -> a
foldr1' mesg _ [] = error ("foldr1': " ++ mesg)
foldr1' _ f as = foldr1 f as

deepScore :: Int -> Board -> Float
deepScore 0 = fromIntegral . score
deepScore d = k . map snd . mapMaybe f . allPlaces
  where f = best . scoreMoves (d-1)
        k [] = -1
        k xs = foldr1 min xs    -- take the worst case

scoreMoves :: Int -> Board -> [(Move, Float)]
scoreMoves d b = map f $ mapMaybe (maybeMove b) every
  where f = mapSnd $ deepScore d

best :: [(Move, Float)] -> Maybe (Move, Float)
best [] = Nothing
best xs = Just $ foldr1' "best" (mapIf snd (>)) xs

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

{- BUG:

(Left,18544.814)
     2     4    16     4
     2    64  1024   256
     8   512    16     -
    32   128     4     -
(Down,NaN)
     2     4    16     -
     4    64  1024     -
     8   512    16     4
    32   128     4   256
(Up,NaN)
     2     4    16     4
     4    64  1024   256
     8   512    16     -
    32   128     4     2
(Down,NaN)
     2     4    16     2
     4    64  1024     4
     8   512    16   256
    32   128     4     2
game2048: Maybe.fromJust: Nothing

NaN comes from division by zero, when we hit bottom.

-}
