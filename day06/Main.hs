module Main where

import System.IO
import System.Environment
import Text.Read (readMaybe)
import qualified Data.Vector as V

parseList :: Read a => Char -> String -> [a]
parseList c [] = []
parseList c l
  | c `elem` l =
      let (n,r) = span (/= c) l in
          let lr = parseList c $ dropWhile (== c) r in
              case readMaybe n of
                Nothing -> lr
                Just nn -> nn:lr
  | otherwise =
      case readMaybe l of
        Nothing -> []
        Just nn -> [nn]

mapFold :: (b -> a -> (b, a)) -> [a] -> b -> (b,[a])
mapFold f [] z = (z, [])
mapFold f (x:xs) z =
    let (z', x') = f z x in
        let (z'', xs') = mapFold f xs z' in
            (z'', x':xs')

-- Simple simulation using lists (very inefficient)
simulate :: [Integer] -> [Integer]
simulate l =
    let (num0, l') = mapFold next l 0 in
        l' ++ (rep num0 8)
    where next acc n 
            | n == 0 = (acc + 1, 6)
            | otherwise = (acc, n - 1)
          rep 0 x = []
          rep n x = x:(rep (n - 1) x)

simulateN :: Integer -> [Integer] -> [Integer]
simulateN 0 l = l
simulateN n l =
    simulateN (n - 1) $ simulate l

-- Another model using a fixed length vector (n,q) where n is
-- the timer (between 0 and 8) and q is the quantity of fishes with
-- this timer
init_pool :: [Int] -> V.Vector Int
init_pool =
    foldl categorize (V.replicate 9 0)
    where categorize v n = v V.// [(n, 1 + v V.! n)]

simulatev :: V.Vector Int -> V.Vector Int
simulatev v =
    let num0 = v V.! 0
        v' = V.update v shifting in
        v' V.// [(8, num0),(6, num0 + v' V.! 6)]
    where shifting = V.tail $ V.imap (\i x -> (i - 1, x)) v

simulatevN :: Integer -> V.Vector Int -> V.Vector Int
simulatevN 0 l = l
simulatevN n l =
    simulatevN (n - 1) $ simulatev l

main :: IO ()
main = do
    (filename:_) <- getArgs
    seed <- parseList ',' <$> readFile filename
    putStr "Number of generations: "
    hFlush stdout
    n <- read <$> getLine
    let result = simulatevN n $ init_pool seed in
        putStrLn $ "Number of fishes: " ++ (show $ V.sum result)
--    let result = simulateN n seed in
--        putStrLn $ "Number of fishes: " ++ (show $ length result)



