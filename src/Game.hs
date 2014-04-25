{-# LANGUAGE ViewPatterns #-}

module Game where

import Board
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, state)
import Data.Char (toUpper, isSpace)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)
import System.Random (RandomGen, random)
import Util (choose, every)

placeRandom :: (RandomGen g, MonadState g m) => Board -> m (Maybe Board)
placeRandom b = case freeCells b of
  [] -> return Nothing
  cs -> do
    c <- choose cs
    t <- state random
    return $ Just $ placeTile t c b

placeRandom' :: (RandomGen g, MonadState g m) => Board -> m Board
placeRandom' = liftM fromJust . placeRandom

start :: (RandomGen g, MonadState g m) => m Board
start = placeRandom' zero >>= placeRandom'

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

loop :: (RandomGen g, MonadState g m, MonadIO m) => Board -> m ()
loop b = do
  m' <- liftIO $ putStr (show2D b) >> askMove (map fst nexts)
  maybe (liftIO $ putStrLn "OK, quitter.") k m'
  where k m = do
          mb <- placeRandom $ fromJust $ lookup m nexts
          maybe (liftIO $ putStrLn "GAME OVER!") loop mb
        nexts = mapMaybe (maybeMove b) every
