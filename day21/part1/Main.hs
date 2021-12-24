module Main where

import System.Environment
import Game
import Text.Printf

newtype Det123 = Det123 { _counter :: Integer } deriving Show

instance Generator Det123 where
  next (Det123 n) = (9*n + 6, Det123 $ n + 1)

new_det123 :: Det123
new_det123 = Det123 0

main :: IO ()
main = do
    (sp1:sp2:_) <- getArgs
    let p1 = read sp1 :: Integer 
        p2 = read sp2 :: Integer in do
            printf "Playing starting with p1=%d and p2=%d (deterministic dice)\n" p1 p2
            gs <- doPlay new_det123 p1 p2 1000
            printf "Player %s wins after %d turns (dice rolled %d times).\n" (if current_player gs then "2" else "1") (number_turns gs) (3*number_turns gs)
            printf "Player 1 score: %d\nPlayer 2 score: %d\n" (player1_score gs) (player2_score gs)
            printf "Number of rolls × loser score: %d\n" (3 * (number_turns gs) * (if current_player gs then player1_score gs else player2_score gs))




