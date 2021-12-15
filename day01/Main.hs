module Main where

import System.Environment

process :: Ord a => [a] -> Integer
process l@(_:ls) =
    sum $ zipWith (\x y -> if y > x then 1 else 0) l ls

slidingSum :: Num a => Int -> [a] -> [a]
slidingSum n l@(_:xs)
    | length l < n = []
    | otherwise = 
        let s = sum $ take n $ l in
            s:(slidingSum n xs)

windowSize = 3

main :: IO ()
main = do
    (filename:_) <- getArgs
    ls <- lines <$> readFile filename
    let input = map read ls :: [Integer] in do
        putStrLn $ "Result (simple process): " ++ (show $ process input)
        putStrLn $ "Result (" ++ (show windowSize) ++ "-window): " ++ (show $ process $ slidingSum windowSize input)
        putStrLn "Done."


