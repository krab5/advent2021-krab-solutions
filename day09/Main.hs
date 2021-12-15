module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Grid
import Basin

parseGrid :: String -> Grid
parseGrid filect = do
    let ls@(x:_) = lines filect in
        let content = map parseDigit $ concat ls
            numcol  = length x in
            new_grid numcol content
    where parseDigit c = (ord c - ord '0')

main :: IO ()
main = do
    (filename:_) <- getArgs
    grid <- parseGrid <$> readFile filename
    let res = wells grid in do
        (sum,bassizes,basins) <- foldM (proc1 grid) (0,[],[]) res
        putStrLn $ "Sum: " ++ show sum
        let (b1:b2:b3:_) = sortBy (flip compare) bassizes in do
          putStrLn $ "3 largest basins size: " ++ (show b1) ++ ", " ++ (show b2) ++ ", " ++ (show b3)
          putStrLn $ "Product: " ++ (show $ b1 * b2 * b3)
          putStrLn $ "Generating HTML map"
          writeFile "output.html" (hyper_pretty grid basins)
    where proc1 grid (sum,bassizes,basins) (p,r) =
              let (bassize,tbasin) = basin grid p in do
                putStrLn $ "Min @ " ++ (show p) ++ " = " ++ (show r) ++ ", basin of size " ++ (show bassize)
                return $ (sum + r + 1,bassize:bassizes,tbasin:basins)



