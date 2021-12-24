module Main where

import qualified Data.Map as M
import System.Environment

type MultiSet a = M.Map a Integer

add_mod :: Integer -> Integer -> Integer
add_mod start fwd =
    1 + (start + fwd - 1) `mod` 10

one_two_three :: [Integer]
one_two_three = [ i + j + k | i <- [1..3], j <- [1..3], k <- [1..3] ]

step :: Bool -> (Integer,Integer,Integer,Integer) -> Integer -> MultiSet (Integer,Integer,Integer,Integer)
step player (p1pos,p1score,p2pos,p2score) order =
    foldl roll M.empty one_two_three
    where roll acc r =
              if player
                  then let pos = add_mod r p2pos in M.insertWith (+) (p1pos,p1score,pos,p2score + pos) order acc
                  else let pos = add_mod r p1pos in M.insertWith (+) (pos,p1score + pos,p2pos,p2score) order acc



won :: (Integer,Integer,Integer,Integer) -> Maybe Bool
won (_,p1score,_,p2score)
    | p1score >= 21 = Just False
    | p2score >= 21 = Just True
    | otherwise = Nothing

superstep :: Integer -> Integer -> MultiSet (Integer,Integer,Integer,Integer) -> Bool 
          -> (Bool, (Integer,Integer,MultiSet (Integer,Integer,Integer,Integer)))
superstep p1win p2win multiset player =
    (not player, M.foldrWithKey step1 (p1win,p2win,M.empty) multiset)
    where step1 state order (p1win,p2win,multi) =
            let result = step player state order in
                let (p1win',p2win',rem) = prune_term result in
                    (p1win + p1win',p2win + p2win', M.unionWith (+) multi rem)
          prune_term map = M.foldrWithKey prune1 (0,0,M.empty) map
          prune1 state order (p1win,p2win,map) =
              case won state of
                Nothing -> (p1win,p2win,M.insert state order map)
                Just False -> (p1win + order,p2win,map)
                Just True  -> (p1win,p2win + order,map)

step_until_the_end :: Integer -> Integer -> (Integer,Integer)
step_until_the_end p1start p2start =
    let init_map = M.singleton (p1start,0,p2start,0) 1 in
        stepcue 0 0 init_map False
    where stepcue p1w p2w ms p =
            let (p',(p1w',p2w',ms')) = superstep p1w p2w ms p in
                if M.null ms' then (p1w',p2w')
                               else stepcue p1w' p2w' ms' p'

main :: IO ()
main = do
    (sp1:sp2:_) <- getArgs
    let (p1win,p2win) = step_until_the_end (read sp1) (read sp2) in do
        putStrLn $ "Player 1 wins in " ++ (show p1win) ++ " universes"
        putStrLn $ "Plyaer 2 wins in " ++ (show p2win) ++ " universes"


