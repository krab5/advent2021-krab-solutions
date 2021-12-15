module Main where

import System.Environment
import Data.List (sort)
import Text.Read
import Text.Printf

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

{-
   The direct answer for case 1 is the median (easily proven by calculating
   derivatives). For case 2, it is average...
   But what the fun in that? I want to write a crab function.
-}

median :: [a] -> a
median l =
    let m = (length l) `div` 2 in
        l !! m

absdiff :: (Ord a, Num a) => a -> a -> a
absdiff x y
    | x == y = 0
    | x < y = y - x
    | otherwise = x - y

sumdiff :: (Ord a, Integral a) => a -> a -> a
sumdiff x y
  | x == y = 0
  | x < y = sumdiff y x
  | otherwise =
      let d = x - y in (d * (d + 1)) `div` 2 -- sum 1..d i

costs :: (Ord a, Integral a) => (a -> a -> a) -> a -> [a] -> a
costs costfun p =
    foldl (\acc x -> acc + costfun p x) 0

{-
   A "crab" function: moves along a function with a global minimum
   by following the decreasing direction.
   This one is a "lazy crab" function, as I could have increased the
   offset using gradients and what not; but for the size of the
   problem it works fine.
   It starts at the median because it is generally a good start.
-}
search :: (Ord a, Integral a) => (a -> a -> a) -> [a] -> (a, a)
search costfun unsorted =
    next m (thecost (m - 1) l) (thecost m l) (thecost (m + 1) l)
    where thecost = costs costfun
          l = sort unsorted
          m = median l
          next x costm1 costx costp1
              | costx < costm1 && costx < costp1 = (x, costx)
              | costm1 < costx = next (x - 1) (thecost (x - 2) l) costm1 costx
              | costp1 < costx = next (x + 1) costx costp1 (thecost (x + 2) l)


main :: IO ()
main = do
    (filename:_) <- getArgs
    input <- (parseList ',' <$> readFile filename) :: IO [Integer]
    let (point1,cost1) = search absdiff input 
        (point2,cost2) = search sumdiff input in do
        printf "Number of crabs: %d\n" (length input)
        printf "Using absdiff:\n"
        printf " - Optimal point  : % 12d\n" point1
        printf " - Associated cost: % 12d\n" cost1
        printf "Using sumdiff:\n"
        printf " - Optimal point  : % 12d\n" point2
        printf " - Associated cost: % 12d\n" cost2


