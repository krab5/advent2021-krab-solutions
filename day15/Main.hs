module Main where

import System.Environment
import Grid
import GridPath
import ComplexGridPath

main :: IO ()
main = do
    (filename:rcstr:rrstr:_) <- getArgs
    grid <- parse_grid_digits <$> readFile filename
    putStrLn $ "Grid (" ++ (show $ 1 + maxcol grid) ++ "×" ++ (show $ 1 + maxrow grid) ++ "):"
    putStrLn $ show grid
    (brc,brr) <- return $ bottom_right grid
    case simple_grid_shortest grid of
      Nothing -> putStrLn $ "No shortest path found"
      Just result -> putStrLn $ "Cost of shortest path: " ++ (show $ result)
    rc <- return $ (read rcstr :: Int)
    rr <- return $ (read rrstr :: Int)
    putStrLn $ "On grid repeated " ++ (show rr) ++ " times vertically and " ++ (show rc) ++ " times horizontally"
    case complex_grid_shortest (grid_map fromInteger grid) rc rr of
      Nothing -> putStrLn $ "No shortest path found"
      Just result -> putStrLn $ "Cost of shortest path: " ++ (show $ result)
    putStrLn ""



