module Grid where

import Data.Char (ord)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (concat,find)
import Text.Printf

data Grid = Grid {
    _num_col :: Int,
    _content :: V.Vector Int
}

new_grid :: Int -> [Int] -> Grid
new_grid nc ct = Grid nc $ V.fromList ct

parse_grid :: String -> Grid
parse_grid [] = new_grid 0 []
parse_grid s =
    let ls@(l:_) = lines s in
        new_grid (length ls) (map parseDigit $ concat ls)
    where parseDigit c = ord c - ord '0'

instance Show Grid where
  show grid =
      V.ifoldl (\acc i x -> acc ++ show x ++ (if (i + 1) `mod` nc == 0 then "\n" else "")) "" ct
      where nc = _num_col grid
            ct = _content grid

id_ :: Grid -> (Int,Int) -> Int
id_ grid (col,row) =
    col + row * (_num_col grid)

at :: Grid -> (Int,Int) -> Int
at grid p = 
    (_content grid) V.! (id_ grid p)

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

straight_neighbors :: Grid -> (Int,Int) -> [(Int,Int)]
straight_neighbors grid (col,row) =
    filter inbound $ [(col - 1, row),(col + 1, row),(col, row - 1),(col, row + 1)]
    where inbound (c,r) = c >= 0 && r >= 0 && c <= maxcol grid && r <= maxrow grid

all_neighbors :: Grid -> (Int,Int) -> [(Int,Int)]
all_neighbors grid (col,row) =
    filter inbound $ [(col - 1, row + 1),(col, row + 1),(col + 1, row + 1),(col - 1, row),(col + 1, row),(col - 1, row - 1),(col, row - 1),(col + 1, row - 1)]
    where inbound (c,r) = c >= 0 && r >= 0 && c <= maxcol grid && r <= maxrow grid

grid_poss :: Grid -> [(Int,Int)]
grid_poss grid =
    [(c,r) | c <- [0..nc], r <- [0..nr]]
    where nr = maxrow grid
          nc = maxcol grid

grid_update :: Grid -> (Int,Int) -> Int -> Grid
grid_update grid pos val =
    grid { _content = (_content grid) V.// [(id_ grid pos,val)] }

grid_map :: (Int -> Int) -> Grid -> Grid
grid_map f grid =
    grid { _content = V.map f $ _content grid }

grid_mapl :: (Int -> Int) -> Grid -> [(Int,Int)] -> Grid
grid_mapl f grid poss =
    let ids = map (id_ grid) poss in
        let vals = map f $ map ((_content grid) V.!) ids in
            grid { _content = (_content grid) V.// (zip ids vals) }

grid_elem :: Int -> Grid -> [(Int,Int)]
grid_elem n grid =
    V.toList $ V.map coord $ V.elemIndices n $ _content grid
    where coord n = let (r,c) = n `divMod` (_num_col grid) in (c,r)

grid_all :: (Int -> Bool) -> Grid -> Bool
grid_all p = V.all p . _content

foldneighbors :: (Grid -> (Int,Int) -> [(Int,Int)]) -> (a -> (Int,Int) -> Int -> [((Int,Int),Int)] -> a) -> a -> Grid -> a
foldneighbors neighbors next z grid =
    foldl next0 z points
    where points = grid_poss grid 
          next0 acc pos =
              let neigh = map (\coord -> (coord,grid `at` coord)) $ neighbors grid pos
                  value = grid `at` pos in
                  next acc pos value neigh




