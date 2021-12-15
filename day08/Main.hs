{-# LANGUAGE BinaryLiterals #-}
module Main where

import Data.Int
import Data.Char
import Data.Bits
import Data.Maybe
import Data.List
import Text.Printf
import qualified Data.Vector as V
import Control.Monad
import System.Environment

data Seg = A | B | C | D | E | F | G deriving (Eq,Ord,Enum)
data Aff = Aff Int8 deriving Eq -- _gfedcba

instance Show Seg where
  show A = "a"
  show B = "b"
  show C = "c"
  show D = "d"
  show E = "e"
  show F = "f"
  show G = "g"

instance Show Aff where
  show a = concat $ map show $ filter (`seg` a) (enumFrom A)

segs :: V.Vector Aff
segs =
    V.fromList [
        Aff $ 0b01110111, -- 0 abcefg
        Aff $ 0b00100100, -- 1 cf
        Aff $ 0b01011101, -- 2 acdeg
        Aff $ 0b01101101, -- 3 acdfg
        Aff $ 0b00101110, -- 4 bcdf
        Aff $ 0b01101011, -- 5 abdfg
        Aff $ 0b01111011, -- 6 abdefg
        Aff $ 0b00100101, -- 7 acf
        Aff $ 0b01111111, -- 8 abcdefg
        Aff $ 0b01101111  -- 9 abcdfg
    ]

num_segs :: V.Vector Int
num_segs = V.map num_seg segs

seg :: Seg -> Aff -> Bool
seg s (Aff a) = testBit a (fromEnum s)

set_seg :: Seg -> Aff -> Aff
set_seg s (Aff a) = Aff $ setBit a (fromEnum s)

num_seg :: Aff -> Int
num_seg (Aff a) = popCount a

contained_in :: Aff -> Aff -> Bool
contained_in (Aff a) (Aff a') =
    (a .&. a') == a

combine :: Aff -> Aff -> Aff
combine (Aff a) (Aff a') = Aff (a .|. a')

parseSeg :: Char -> Seg
parseSeg c = toEnum (ord c - ord 'a')

parseAff :: String -> Aff
parseAff =
    foldr set_seg (Aff 0) . map parseSeg

parseLine :: String -> ([Aff],[Aff])
parseLine s =
    let (left, _:right) = span (/= '|') s in
        (next [] left, next [] right)
    where next acc [] = reverse acc
          next acc (' ':xs) = next acc xs
          next acc l =
              let (aff,rem) = span (not . isSpace) l in
                  next ((parseAff aff):acc) $ (if null rem then [] else tail rem)

parseAll :: [String] -> [([Aff],[Aff])]
parseAll = map parseLine

count_1478 :: [Aff] -> Int
count_1478 =
    length . filter ((`elem` segs) . num_seg)
    where segs = map (num_segs V.!) [1,4,7,8]

find_aff :: [Aff] -> V.Vector Aff
find_aff affs0 =
    V.fromList [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9]
    where filter_out1 acc p (x:xs)
            | p x = (x,(reverse acc) ++ xs)
            | otherwise = filter_out1 (x:acc) p xs
          filter_out = filter_out1 []
          (a1,affs1) = filter_out ((== 2) . num_seg) affs0
          (a4,affs2) = filter_out ((== 4) . num_seg) affs1
          (a7,affs3) = filter_out ((== 3) . num_seg) affs2
          (a8,affs4) = filter_out ((== 7) . num_seg) affs3
          (a3,affs5) = filter_out ((&&) <$> (== 5) . num_seg <*> (a1 `contained_in`)) affs4
          (a9,affs6) = filter_out ((&&) <$> (== 6) . num_seg <*> (a4 `contained_in`)) affs5
          (a0,affs7) = filter_out ((&&) <$> (== 6) . num_seg <*> (a1 `contained_in`)) affs6
          (a6,affs8) = filter_out ((== 6) . num_seg) affs7 -- only remaining with 6 segments
          (a5,affs9) = filter_out ((&&) <$> (== 5) . num_seg <*> ((`contained_in` a9) . (`combine` a1))) affs8
          a2 = head affs9

decode :: V.Vector Aff -> [Aff] -> Int
decode v =
    foldl (\acc aff -> acc*10 + (fromJust $ V.elemIndex aff v)) 0

main :: IO ()
main = do
    (filename:_) <- getArgs
    ls <- parseAll <$> lines <$> readFile filename
    putStrLn $ "Number of inputs: " ++ (show $ length ls)
    let result = map (\(affs,digits) -> let code = find_aff affs in (code,digits,decode code digits)) ls in do
        forM_ (zip ([1..] :: [Integer]) result) $ \(n,(code,dig,res)) -> printf "% 4d. %s | %s -> %04d\n" n (show code) (show dig) res
        putStrLn $ "Sum: " ++ (show $ foldl (\acc (_,_,r) -> acc + toInteger r) 0 result)




