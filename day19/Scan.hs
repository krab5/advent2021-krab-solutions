{-# LANGUAGE TupleSections #-}
module Scan where

import qualified Data.Set as S
import qualified Data.Vector as V
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Geom

diffs :: [Vec3] -> [(Vec3,Vec3,Vec3)]
diffs vs =
    map (\(x,y) -> (x, y, x `absdiff` y)) $ take2 vs
    where take2 [] = []
          take2 (v:vs) =
              (map (v,) vs) ++ (take2 vs)

similar :: Vec3 -> Vec3 -> [Transfo3]
similar d1 d2 = do
    x <- d2 `indices` (d1 `get` xx)
    y <- d2 `indices` (d1 `get` yy)
    z <- d2 `indices` (d1 `get` zz)
    guard (x /= y)
    guard (x /= z)
    guard (y /= z)
    return $ tran (x,y,z)

maxocc :: Eq b => [(a, b)] -> ([a], b)
maxocc l =
    maximumBy (compare `on` (length . fst)) $ foldl group [] l
    where group acc (x, b) = add acc x b
          add []           x b' = [([x], b')]
          add ((xs,b):rem) x b' = if b == b' then (x:xs,b):rem else (xs,b):(add rem x b')

find_transfo :: [((Vec3,Vec3,Vec3),(Vec3,Vec3,Vec3))] -> Transfo3 -> (Transfo3,Vec3)
find_transfo (((x1,x2,_),(y1,y2,_)):xs) t =
    let ((xx1,yy1),(xx2,yy2),(xx3,yy3)) = find_next (x1,x2) (y1,y2) xs in
        let dx12 = xx1 `diff` xx2
            dy12 = (yy1 /// t) `diff` (yy2 /// t) in
            let t' = concordant_directions t dx12 dy12 in
                (t', find_translation t' xx1 yy1)
    where find_next (x1,x2) (y1,y2) (((x3,x4,_),(y3,y4,_)):xs)
              | x1 == x3 = make_up x1 x2 x4 y1 y2 y3 y4
              | x1 == x4 = make_up x1 x2 x3 y1 y2 y3 y4
              | x2 == x3 = make_up x2 x1 x4 y1 y2 y3 y4
              | x2 == x4 = make_up x2 x1 x3 y1 y2 y3 y4
              | otherwise = find_next (x1,x2) (y1,y2) xs
          make_up x1 x2 x3 y1 y2 y3 y4
              | y1 == y3 = ((x1,y1),(x2,y2),(x3,y4))
              | y1 == y4 = ((x1,y1),(x2,y2),(x3,y3))
              | y2 == y3 = ((x1,y2),(x2,y1),(x3,y4))
              | y2 == y4 = ((x1,y2),(x2,y1),(x3,y3))
          concordant_directions t xx yy =
               foldl (flip_if_different_signum xx yy) t [1..3]
          flip_if_different_signum xx yy tacc i
              | signum (xx `get` i) == signum (yy `get` i) = tacc
              | otherwise = tacc `opp` i
          find_translation t x y =
              let y' = y /// t in
                  x `diff` y'


find_similar :: [(Vec3,Vec3,Vec3)] -> [(Vec3,Vec3,Vec3)] -> Maybe ([((Vec3,Vec3,Vec3),(Vec3,Vec3,Vec3))],Transfo3)
find_similar base new =
    case l of
      [] -> Nothing
      l  -> Just $ maxocc l
    where l = [ ((cbase,cnew),transfo) | cbase@(_,_,d) <- base
                                       , cnew@(_,_,d') <- new
                                       , transfo <- similar d d' ]

find_common_points :: Int -> [Vec3] -> [Vec3] -> Maybe (Transfo3,Vec3)
find_common_points thresh base new =
    case find_similar (diffs base) (diffs new) of
      Nothing -> Nothing
      Just (similar,_) | length similar < dthresh -> Nothing
      Just (similar,transfo) -> 
        let (transfo',trvec) = find_transfo similar transfo in
            let common = foldl (\acc ((x1,x2,_),_) -> S.insert x2 (S.insert x1 acc)) S.empty similar in
                Just (transfo', trvec)
    where dthresh = (thresh * (thresh - 1)) `div` 2


find_overlaps :: Int -> [[Vec3]] -> [((Int,Int),Transfo3,Vec3)]
find_overlaps thresh sections =
    next 0 sections
    where next _ [] = []
          next _ [x] = []
          next n1 (s1:xs) = (next1 n1 s1 (n1 + 1) xs) ++ (next (n1 + 1) xs)
          next1 _ _ _ [] = []
          next1 n1 s1 n2 (s2:xs) =
              let rem = next1 n1 s1 (n2 + 1) xs in
                case find_common_points thresh s1 s2 of
                    Nothing -> rem
                    Just (t,v) -> ((n1,n2),t,v):rem

reduce :: [((Int,Int),Transfo3,Vec3)] -> M.Map Int (Transfo3,Vec3)
reduce trans =
    reduce1 (M.singleton 0 (tran (1,2,3),vec 0 0 0)) 0
    where upper = maximum $ concat $ map (\((a,b),_,_) -> [a,b]) trans
          reduce1 acc n =
            let start = filter (\((n',_),_,_) -> n' == n) trans
                end   = filter (\((_,n'),_,_) -> n' == n) trans in
                case M.lookup n acc of
                  Nothing -> acc
                  Just (t,v) ->
                      let (acc',ns') = foldl (append_direct t v) (acc,S.empty) start in
                          let (acc'',ns'') = foldl (append_indirect t v) (acc',ns') end in
                              S.foldl reduce1 acc'' ns''
          -- add a new transformation; (t,v) is the transfo from 0 to n, (t',v') is the 
          -- transfo from n to n'
          append_direct t v (acc,ns) ((n,n'),t',v')
              | n' `M.member` acc = (acc,ns)
              | otherwise =
                  (M.insert n' (t' `comp` t,(v' /// t) `translate` v) acc, S.insert n' ns)
          -- add a new transformation; (t,v) is the transfo from 0 to n, (t',v') is the
          -- transfo from n' to n
          append_indirect t v (acc,ns) ((n',n),t',v')
              | n' `M.member` acc = (acc,ns)
              | otherwise =
                  (M.insert n' ((invert t') `comp` t, v `diff` (v' /// ((invert t') `comp` t))) acc, S.insert n' ns)

accumulate_positions :: M.Map Int (Transfo3,Vec3) ->  [[Vec3]] -> S.Set Vec3
accumulate_positions trans (sec:secs) =
    snd $ foldl append_section (1,S.fromList sec) secs
    where append_section (n,acc) sec =
              let (t,v) = fromJust $ M.lookup n trans in
                  (n + 1, foldl (add_one t v) acc sec)
          add_one t v acc vec = S.insert ((vec /// t) `translate` v) acc







