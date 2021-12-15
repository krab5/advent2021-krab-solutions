module Main where

import Bingo
import Parser
import System.Environment

main :: IO ()
main = do
    (filename:_) <- getArgs
    (Config nums bbs) <- parse_config <$> lines <$> readFile filename
    let ((best_steps,best_winning_num,best_bb),(worst_steps,worst_winning_num,worst_bb)) = compete nums bbs in do
        putStrLn $ "Winner bingo board (in " ++ show best_steps ++ " steps):"
        putStrLn $ show best_bb
        putStrLn $ "Won with number " ++ show best_winning_num
        putStrLn $ "Score: " ++ (show $ score best_winning_num best_bb)
        putStrLn $ "Last winning bingo board (in " ++ show worst_steps ++ " steps):"
        putStrLn $ show worst_bb
        putStrLn $ "Won with number " ++ show worst_winning_num
        putStrLn $ "Score: " ++ (show $ score worst_winning_num worst_bb)



