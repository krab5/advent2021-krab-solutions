{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import Parser
import Snailfish

import Data.List (maximumBy)

comb2 :: [a] -> [(a,a)]
comb2 [] = []
comb2 (x:xs) =
    let withx = map (x,) xs ++ map (,x) xs
        withoutx = comb2 xs in
        withx ++ withoutx

adds :: [Snailfish] -> (Snailfish,Snailfish,Snailfish,Integer)
adds =
    maximumBy ord4 . map (\(x,y) -> let z = add x y in (x,y,z,magnitude z)) . comb2
    where ord4 (_,_,_,x) (_,_,_,y) = compare x y
    

main :: IO ()
main = do
    (filename:_) <- getArgs
    ls <- lines <$> readFile filename
    let sns = map parse_snailfish ls in do
        let s = ssum sns in
            let m = magnitude s in do
                putStrLn $ "Parsed " ++ (show $ length sns) ++ " numbers"
                putStrLn $ "Sum: " ++ (show s) 
                putStrLn $ "Magnitude: " ++ (show m)
        let (x,y,z,m) = adds sns in do
            putStrLn $ "Maximum number: " ++ (show z)
            putStrLn $ "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
            putStrLn $ "Magnitude: " ++ (show m)



