module Main where

import System.Environment
import Cuboid
import Inst

main :: IO ()
main = do
    (filename:_) <- getArgs
    insts <- parse_insts <$> readFile filename
    let cub = exec_insts insts in do
        putStrLn $ "Resulting cuboids:"
        putStrLn $ show cub
        putStrLn $ "(sanity check: " ++ (if coherent cub then "OK" else "KO") ++ ")"
        putStrLn $ "Size: " ++ (show $ discrete_sizes cub)
        let cub' = cub `intersects` (cuboid (-50,50) (-50,50) (-50,50)) in do
            putStrLn $ "Cut down to [-50,50]³:"
            putStrLn $ show cub'
            putStrLn $ "(sanity check: " ++ (if coherent cub' then "OK" else "KO") ++ ")"
            putStrLn $ "Size: " ++ (show $ discrete_sizes cub')


