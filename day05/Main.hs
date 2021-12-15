module Main where

import Grid
import VentLine
import System.Environment
import Control.Monad

main :: IO ()
main = do
    (filename:_) <- getArgs
    vents <- parseAllVents <$> lines <$> readFile filename
    let grid = init_grid (1000,1000) in
        let result = applyVents grid vents in do
            putStrLn $ "Number of dangerous zones (v > 1): " ++ (show $ countDangerous $ result)




