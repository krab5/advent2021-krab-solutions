module Paper where

import Data.List (intercalate)
import qualified Data.Set as S

data Paper = Paper {
    content :: S.Set (Integer,Integer),
    width :: Integer,
    height :: Integer
}

instance Show Paper where
  show paper =
      intercalate "\n" $ map line $ [0..he - 1]
      where ct = content paper
            wi = width   paper
            he = height  paper
            line y = map (showLoc y) [0..wi - 1]
            showLoc y x = if (x,y) `S.member` ct then '#' else '.'

new_paper :: [(Integer,Integer)] -> Paper
new_paper l =
    let (fsts,snds) = unzip l in
        Paper { content = S.fromList l, width = 1 + maximum fsts, height = 1 + maximum snds }

num_dots :: Paper -> Int
num_dots = S.size . content

parse_paper :: [String] -> Paper
parse_paper ls =
    new_paper $ map parsePair ls
    where parsePair str =
            let (left,_:right) = span (/= ',') str in
                (read left, read right)

fold_horizontal :: Paper -> Integer -> Paper
fold_horizontal paper y =
    paper { content = content', height = y }
    where ct = content paper
          (beforey,aftery) = S.partition ((< y) . snd) ct
          mirror (i,j) = (i, 2 * y - j)
          content' = S.union beforey (S.map mirror aftery)

fold_vertical :: Paper -> Integer -> Paper
fold_vertical paper x =
    paper { content = content', width = x }
    where ct = content paper
          (beforex,afterx) = S.partition ((< x) . fst) ct
          mirror (i,j) = (2 * x - i, j)
          content' = S.union beforex (S.map mirror afterx)


data Instruction =
      FoldY Integer
    | FoldX Integer

instance Show Instruction where
  show (FoldY y) = "fold along y=" ++ show y
  show (FoldX x) = "fold along x=" ++ show x

parse_instruction :: String -> Instruction
parse_instruction str =
    let (dir:_:rem) = drop (length "fold along ") str in
        parseDir dir $ read rem
    where parseDir 'x' = FoldX
          parseDir 'y' = FoldY

parse_instructions :: [String] -> [Instruction]
parse_instructions = map parse_instruction

exec_instruction :: Paper -> Instruction -> Paper
exec_instruction p (FoldY y) = fold_horizontal p y
exec_instruction p (FoldX x) = fold_vertical   p x

exec_instructions :: Paper -> [Instruction] -> Paper
exec_instructions = foldl exec_instruction

parse_file :: String -> (Paper,[Instruction])
parse_file str =
    let ls = lines str in
        let (head,_:insts) = span (not . null) ls in
            (parse_paper head, parse_instructions insts)



