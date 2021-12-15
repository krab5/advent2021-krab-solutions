module Main where

import Paper
import System.Environment

main :: IO ()
main = do
    (filename:_) <- getArgs
    (paper,instructions@(i:_)) <- parse_file <$> readFile filename
    putStrLn $ "Paper: "
    putStrLn $ show paper
    let paper' = exec_instruction paper i in do
        putStrLn $ "First fold (" ++ show i ++ "):"
        putStrLn $ show paper'
        putStrLn $ "Number of dots: " ++ (show $ num_dots paper')
    let paper'' = exec_instructions paper instructions in do
        putStrLn $ "After " ++ (show $ length instructions) ++ " folds:"
        putStrLn $ show paper''


