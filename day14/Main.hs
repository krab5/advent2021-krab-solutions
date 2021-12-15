module Main where

import System.Environment
import Data.Char (isAlpha)
import Control.Monad
import Text.Printf
import qualified Data.Set as S
import Data.List (find)
import qualified Data.Map as M

type Rule = ((Char,Char),Char)

parse_rule :: String -> Rule
parse_rule str =
    let (lhs1:lhs2:_,rem) = span (/= ' ') str in
        let (rhs:_) = dropWhile (not . isAlpha) rem in
            ((lhs1,lhs2),rhs)

type Rules = [(S.Set (Char,Char), Char)]

add_rule :: Rules -> Rule -> Rules
add_rule []               (lhs',rhs') = [(S.singleton lhs', rhs')]
add_rule ((lhs,rhs):xs) r@(lhs',rhs')
    | rhs == rhs' = (S.insert lhs' lhs, rhs):xs
    | otherwise = (lhs,rhs):(add_rule xs r)

parse_rules :: [String] -> Rules
parse_rules = 
    foldl add_rule [] . map parse_rule

num_rules :: Rules -> Int
num_rules =
    foldl (\acc (x,_) -> acc + S.size x) 0

check_rule :: Rules -> (Char,Char) -> Maybe Char
check_rule rls input =
    snd <$> find ((S.member input) . fst) rls

step :: Rules -> M.Map (Char,Char) Integer -> M.Map (Char,Char) Integer
step rules input =
    M.foldrWithKey add_pair M.empty input
    where add_pair (x1,x2) n acc
              | Just c <- check_rule rules (x1,x2) =
                  M.insertWith (+) (c,x2) n $ M.insertWith (+) (x1,c) n acc
              | otherwise = M.insertWith (+) (x1,x2) n acc

stepN :: Integer -> Rules -> M.Map (Char,Char) Integer -> M.Map (Char,Char) Integer
stepN 0 _     input = input
stepN n rules input =
    step rules $ stepN (n - 1) rules input

count :: M.Map (Char,Char) Integer -> [(Char,Integer)]
count pairs =
    M.toList $ M.map (\n -> (n + 1) `div` 2) $ M.foldrWithKey count1 M.empty pairs
    where count1 (k1,k2) n acc = M.insertWith (+) k1 n $ M.insertWith (+) k2 n acc

make_pairs :: [Char] -> M.Map (Char,Char) Integer
make_pairs l = 
    foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty $ zip l (tail l)

main :: IO ()
main = do
    (filename:nstr:_) <- getArgs
    (patt:_:rls) <- lines <$> readFile filename
    let rules = parse_rules rls
        n     = read nstr :: Integer in
        let result = stepN n rules $ make_pairs patt in
            let counts = count result in do
                putStrLn $ "Start: " ++ patt
                putStrLn $ "No. of rules: " ++ (show $ num_rules rules)
                putStrLn $ "No. of steps: " ++ nstr
                putStrLn $ "Counts: "
                forM_ counts $ \(c,n) -> printf " %c: % 16d\n" c n
                let max = maximum $ map snd counts
                    min = minimum $ map snd counts in do
                    putStrLn $ "Max: " ++ show max
                    putStrLn $ "Min: " ++ show min
                    putStrLn $ "Diff: " ++ show (max - min)


