module Main where

import Data.List
import System.Environment

data Bit = Zero | One deriving (Show,Eq,Ord,Enum)
type Bits = [Bit]

parseBit :: Char -> Bit
parseBit '1' = One
parseBit '0' = Zero

parse :: String -> Bits
parse = map parseBit

parseAll :: [String] -> [Bits]
parseAll = map parse

example =
    parseAll $
        [ "00100",
          "11110",
          "10110",
          "10111",
          "10101",
          "01111",
          "00111",
          "11100",
          "10000",
          "11001",
          "00010",
          "01010" ]

leastMost :: Bits -> (Bit,Bit)
leastMost =
    decide . foldl count (0,0)
    where count (numZ,numO) Zero = (numZ + 1, numO)
          count (numZ,numO) One  = (numZ, numO + 1)
          decide (numZ,numO) = if numZ > numO then (One,Zero) else (Zero,One)

calculate :: [Bits] -> (Bits,Bits)
calculate ([]:_) = ([],[])
calculate l =
    let (hl,tl) = (head <$> l, tail <$> l) in
        let (bl ,br ) = leastMost hl
            (bls,brs) = calculate tl in
            (bl:bls,br:brs)

calculate2 :: [Bits] -> (Bits,Bits)
calculate2 bs =
    (nextLeast bs, nextMost bs)
    where partZO = partition (\(x:_) -> x == Zero)
          nextLeast :: [Bits] -> Bits
          nextLeast ([]:_) = []
          nextLeast [ll] = ll
          nextLeast l =
              let (zs,os) = partZO l in
                  if length zs > length os 
                    then One:(nextLeast (tail <$> os))
                    else Zero:(nextLeast (tail <$> zs))
          nextMost :: [Bits] -> Bits
          nextMost ([]:_) = []
          nextMost [ll] = ll
          nextMost l =
              let (zs,os) = partZO l in
                  if length zs <= length os 
                    then One:(nextMost (tail <$> os))
                    else Zero:(nextMost (tail <$> zs))


showBits :: Bits -> String
showBits =
    foldr (\x acc -> showBit x ++ acc) ""
    where showBit Zero = "0"
          showBit One = "1"

eval :: Num a => Bits -> a
eval =
    foldl (\acc x -> 2 * acc + evalBit x) 0
    where evalBit Zero = 0
          evalBit One = 1

main :: IO ()
main = do
    (filename:_) <- getArgs
    bits <- parseAll <$> lines <$> readFile filename
    let (least,most) = calculate bits 
        (leastest,mostest) = calculate2 bits in 
        let eleast = eval least
            emost = eval most 
            eleastest = eval leastest
            emostest = eval mostest in
            let prod = eleast * emost 
                prod2= eleastest * emostest in do
                putStrLn $ "Gamma rate: " ++ showBits most ++ " (" ++ (show emost) ++ ")"
                putStrLn $ "Epsilon rate: " ++ showBits least ++ " (" ++ (show eleast) ++ ")"
                putStrLn $ "Product: " ++ (show prod)
                putStrLn $ "Oxygen rating: " ++ showBits mostest ++ " (" ++ show emostest ++ ")"
                putStrLn $ "C02 rating: " ++ showBits leastest ++ " (" ++ show eleastest ++ ")"
                putStrLn $ "Product: " ++ (show prod2)


