module Main where

import System.Environment
import Cucumber

main :: IO ()
main = do
    (filename:_) <- getArgs
    grid <- parse_cucumbers <$> readFile filename
    putStrLn $ "Starting configuration:"
    putStrLn $ show_cucumbers grid
    let (n,grid') = step_until_no_progress grid in do
        putStrLn $ "After " ++ (show n) ++ " steps, no further progress can be made"
        putStrLn $ "Resulting configuration:"
        putStrLn $ show_cucumbers grid'


