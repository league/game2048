import Data.Time.Clock
import Game2048.AI (scoreMoves)
import Game2048.Board as Board

main :: IO ()
main = do
  begin <- getCurrentTime
  let ts = [[2,0,5,6],[2,5,0,0],[0,3,0,2],[1,1,3,0]]
  let b = Board.fromList $ map (map toEnum) ts
  putStrLn $ show $ scoreMoves 4 b
  end <- getCurrentTime
  putStrLn $ show (diffUTCTime end begin) ++ " elapsed."
