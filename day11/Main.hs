module Main where

import System.Environment
import System.IO
import Grid
import Octo

main :: IO ()
main = do
    (filename:snumstep:_) <- getArgs
    grid <- parse_grid <$> readFile filename
    numstep <- return $ (read snumstep :: Integer)
    putStrLn $ "Starting grid: "
    putStrLn $ show grid
    putStrLn $ "Number of steps: " ++ show numstep
    let (sum,grid') = simulateN numstep grid in do
        putStrLn $ "Grid at step " ++ show numstep
        putStrLn $ show grid'
        putStrLn $ "Total number of flashes: " ++ show sum
    let (step,grid'') = simulate_until (grid_all (== 0)) grid in do
        putStrLn $ "After " ++ (show step) ++ " steps, all octopuses flash:"
        putStrLn $ show grid''


