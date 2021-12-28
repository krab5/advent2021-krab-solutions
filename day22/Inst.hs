module Inst where

import Cuboid

data Op = On | Off deriving (Eq,Ord,Enum,Show)
type ICuboid = Cuboid Integer
type ICuboids = Cuboids Integer
data Inst = Inst Op ICuboid deriving (Eq,Show)

parse_inst :: String -> Inst
parse_inst ('o':'n':_:xs) =
    Inst On $ parse_int_cuboid xs
parse_inst ('o':'f':'f':_:xs) =
    Inst Off $ parse_int_cuboid xs

parse_insts :: String -> [Inst]
parse_insts = map parse_inst . lines

exec_inst :: ICuboids -> Inst -> ICuboids
exec_inst cs (Inst On  c) = unions cs c
exec_inst cs (Inst Off c) = differences cs c

exec_insts :: [Inst] -> ICuboids
exec_insts =
    foldl exec_inst empty_cuboids



