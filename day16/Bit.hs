module Bit where

import Data.Char

data Bit = B0 | B1 deriving (Eq,Ord,Enum)

to_bit :: Integral a => a -> Bit
to_bit 0 = B0
to_bit 1 = B1

to_num :: Integral a => Bit -> a
to_num B0 = 0
to_num B1 = 1

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

type Bits = [Bit]

showBits :: Bits -> String
showBits = concat . map show

integral_to_bits :: Integral a => Int -> a -> Bits
integral_to_bits n x =
    next [] n x
    where next acc 0 _ = acc
          next acc n x =
              let (q,r) = x `quotRem` 2 in
                  (next ((to_bit r):acc) (n - 1) q)

bits_to_integral :: Integral a => Bits -> a
bits_to_integral [] = 0
bits_to_integral [x] = to_num x
bits_to_integral bs =
    foldl (\acc x -> acc * 2 + to_num x) 0 bs

parse_hex_digit :: Char -> Bits
parse_hex_digit d
    | '0' <= d && d <= '9' = integral_to_bits 4 $ toInteger $ ord d - ord '0'
    | 'A' <= d && d <= 'F' = integral_to_bits 4 $ toInteger $ ord d - ord 'A' + 10
    | otherwise = []

parse_hex :: String -> Bits
parse_hex =
    foldr (\x acc -> (parse_hex_digit x) ++ acc) []

take_parse :: Integral a => [Int] -> Bits -> ([a],Bits)
take_parse [] bits = ([],bits)
take_parse (n:ns) bits =
    let (tk,rem) = splitAt n bits in
        let (tks,rem') = take_parse ns rem in
            ((bits_to_integral tk):tks,rem')





