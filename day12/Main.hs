module Main where

import Caves
import System.Environment
import Control.Monad

main :: IO ()
main = do
    (filename:_) <- getArgs
    cs <- parse_cave_system <$> readFile filename
    let result = explore_all cs in
        let size = length result in do
            putStrLn $ "Found paths: "
            forM_ result (putStrLn . show)
            putStrLn $ "Number of paths: " ++ (show size)
    let result2 = explore_all_with_small cs in
        let size2 = length result2 in do
            putStrLn $ "Found paths with one revisit: "
            forM_ result2 (putStrLn . show)
            putStrLn $ "Number of paths: " ++ (show size2)


