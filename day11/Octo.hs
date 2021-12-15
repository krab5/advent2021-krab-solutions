module Octo where

import Grid
import qualified Data.Set as S

increase :: Grid -> [(Int,Int)] -> (S.Set (Int,Int), Grid)
increase grid pos =
    foldl increase1 (S.empty,grid) pos
    where increase1 (high,grid) pos =
            let val = (1 + (grid `at` pos)) `mod` 10 in
                let grid' = grid_update grid pos val
                    high' = if val == 0 then S.insert pos high else high in
                    (high',grid')

simulate :: Grid -> (S.Set (Int,Int), Grid)
simulate grid =
    let (high,grid') = increase grid points in
        foldl explore (high,grid') (S.toList high)
    where points = grid_poss grid
          explore (high,grid) pos =
            let neighs = filter (not . (`S.member` high)) $ all_neighbors grid pos in
                let (highneigh,grid') = increase grid neighs in
                    foldl explore (high `S.union` highneigh,grid') (S.toList highneigh)

simulateN :: Integer -> Grid -> (Integer, Grid)
simulateN 0 grid = (0, grid)
simulateN n grid =
    let (high,grid') = simulate grid in
        let (sum,grid'') = simulateN (n - 1) grid' in
            (sum + (toInteger $ S.size high), grid'')

simulate_until :: (Grid -> Bool) -> Grid -> (Integer,Grid)
simulate_until p grid =
    simu1 0 grid
    where simu1 n grid
            | p grid = (n,grid)
            | otherwise = simu1 (n + 1) $ snd $ simulate grid





