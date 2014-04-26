{- Game • interacting with user to play the game
 - Copyright ©2014 Christopher League <league@contrapunctus.net>
 -
 - This program is free software: you can redistribute it and/or modify it
 - under the terms of the GNU General Public License as published by the Free
 - Software Foundation, either version 3 of the License, or (at your option)
 - any later version.
 -}
{-# LANGUAGE ViewPatterns #-}

module Game where

import AI (scoreMoves, best, calc)
import Board
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Char (toUpper, isSpace)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)
import System.Random (RandomGen)
import Util (every)

data Response = Move Move | Quit | Error deriving Show

parseMove :: String -> Response
parseMove = k . fmap toUpper . find (not . isSpace)
  where k Nothing = Error
        k (Just 'Q') = Quit
        k (Just c) = maybe Error Move $ lookup c movesByChar

askMove :: [Move] -> IO (Maybe Move)
askMove valid = do
  putStr "Your move: "
  hFlush stdout
  r <- parseMove `liftM` getLine
  case r of
    Move m | m `elem` valid -> return (Just m)
    Move _ -> err >> askMove valid
    Quit -> return Nothing
    Error -> err >> askMove valid
    where err = putStrLn $ "Error: specify " ++ choices ++ ", or Quit"
          choices = intercalate ", " $ map show valid

loop :: (RandomGen g, MonadState g m, MonadIO m) => Int -> Board -> m ()
loop d b = do
  let ms = scoreMoves d b
  liftIO $ putStr $ show2D b ++ show (calc b) ++ "\n" ++ show ms ++
    "\nAI says " ++ show (best ms) ++ "\n"
  m' <- liftIO $ askMove $ map fst nexts
  maybe (liftIO $ putStrLn "OK, quitter.") k m'
  where k m = do
          mb <- placeRandom $ fromJust $ lookup m nexts
          maybe (liftIO $ putStrLn "GAME OVER!") (loop d) mb
        nexts = mapMaybe (maybeMove b) every
