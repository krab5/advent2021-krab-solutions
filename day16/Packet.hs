module Packet where

import Data.Int
import Data.List

data OpType =
    Sum | Product | Min | Max | GreaterThan | LessThan | EqualsTo
    deriving (Show, Eq, Ord, Enum)
data Packet =
      PLiteral  Int8 Integer
    | POperator Int8 OpType [Packet]
    deriving Show

show_nice :: Packet -> String
show_nice p =
    nice1 "" p
    where nice1 indent (PLiteral v l) = indent ++ showver v ++ " L " ++ (show l)
          nice1 indent (POperator v t ps) =
              let sub = intercalate "\n" $ map (nice1 ("  " ++ indent)) ps in
                  indent ++ showver v ++ " O " ++ show t ++ "\n" ++ sub
          showver v = "(v" ++ show v ++ ")"

fold :: (Integer -> Integer -> b -> b) -> (Integer -> OpType -> b -> b) -> b -> Packet -> b
fold forLiteral forOperator acc pack =
    case pack of
      PLiteral v c -> forLiteral (toInteger v) c acc
      POperator v t s -> 
          let acc' = foldl next acc s in
              forOperator (toInteger v) t acc'
    where fold0 = fold forLiteral forOperator
          next acc p =
              fold0 acc p

sum_versions :: Packet -> Integer
sum_versions =
    fold (\v _ acc -> acc + v) (\v _ acc -> acc + v) 0

eval :: Packet -> Integer
eval (PLiteral _ n) = n
eval (POperator _ ty ps) =
    let eps = map eval ps in
        evalOp ty eps

evalOp :: OpType -> [Integer] -> Integer
evalOp Sum          vals        = sum vals
evalOp Product      vals        = product vals
evalOp Min          vals        = minimum vals
evalOp Max          vals        = maximum vals
evalOp GreaterThan  (x1:x2:_)
    | x1 > x2 = 1
    | otherwise = 0
evalOp LessThan     (x1:x2:_)
    | x1 < x2 = 1
    | otherwise = 0
evalOp EqualsTo     (x1:x2:_)
    | x1 == x2 = 1
    | otherwise = 0



