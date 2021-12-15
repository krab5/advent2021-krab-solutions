module Grid where

import qualified Data.Vector as V
import qualified Data.Set as S

data Grid = Grid {
    _num_col :: Int,
    _content :: V.Vector Int
}

new_grid :: Int -> [Int] -> Grid
new_grid nc ct = Grid nc $ V.fromList ct

instance Show Grid where
  show grid =
      V.ifoldl (\acc i x -> acc ++ show x ++ (if (i + 1) `mod` nc == 0 then "\n" else "")) "" ct
      where nc = _num_col grid
            ct = _content grid

at :: Grid -> (Int,Int) -> Int
at grid (col,row) = 
    ct V.! (col + row * nc)
    where nc = _num_col grid
          ct = _content grid

ats :: Grid -> [(Int,Int)] -> [Int]
ats grid = map (grid `at`)

maxrow :: Grid -> Int
maxrow grid =
    (ln `div` nc) - 1
    where nc = _num_col grid
          ln = V.length $ _content grid

maxcol :: Grid -> Int
maxcol grid =
    (_num_col grid) - 1

neighbors :: Grid -> (Int,Int) -> [(Int,Int)]
neighbors grid (col,row) =
    filter inbound $ [(col - 1, row),(col + 1, row),(col, row - 1),(col, row + 1)]
    where inbound (c,r) = c >= 0 && r >= 0 && c <= maxcol grid && r <= maxrow grid

foldneighbors :: (a -> (Int,Int) -> Int -> [((Int,Int),Int)] -> a) -> a -> Grid -> a
foldneighbors next z grid =
    foldl next0 z points
    where points = [(c,r) | c <- [0..maxcol grid], r <- [0..maxrow grid]]
          next0 acc pos =
              let neigh = map (\coord -> (coord,grid `at` coord)) $ neighbors grid pos
                  value = grid `at` pos in
                  next acc pos value neigh



