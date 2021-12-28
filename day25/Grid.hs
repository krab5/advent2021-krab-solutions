module Grid where

import Data.Char (ord)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (concat,find)
import Text.Printf

{-
A neat implementation of grids (= matrices) based on vectors, stored in rows
(i.e. a row is always contiguous in memory) I have been using throughout
the challenges.
-}

data Grid a = Grid {
    _num_col :: Int,
    _content :: V.Vector a
} deriving Eq

new_grid :: Int -> [a] -> Grid a
new_grid nc ct = Grid nc $ V.fromList ct

init_grid :: (Int,Int) -> a -> Grid a
init_grid (nc,nr) x = Grid nc $ V.generate (nc * nr) (\_ -> x)

parse_grid :: (Char -> a) -> String -> Grid a
parse_grid _ [] = new_grid 0 []
parse_grid parse1 s =
    let ls@(l:_) = lines s in
        new_grid (length l) (map parse1 $ concat ls)

parse_grid_digits :: String -> Grid Integer
parse_grid_digits = parse_grid (toInteger . (\x -> ord x - ord '0'))

instance Show a => Show (Grid a) where
  show grid =
      V.ifoldl (\acc i x -> acc ++ show x ++ (if (i + 1) `mod` nc == 0 then "\n" else "")) "" ct
      where nc = _num_col grid
            ct = _content grid

show_grid :: (a -> Char) -> Grid a -> String
show_grid showc grid =
    V.ifoldl (\acc i x -> acc ++ [showc x] ++ (if (i + 1) `mod` nc == 0 then "\n" else "")) "" ct
    where nc = _num_col grid
          ct = _content grid

id_ :: Grid a -> (Int,Int) -> Int
id_ grid (col,row) =
    col + row * (_num_col grid)

at :: Grid a -> (Int,Int) -> a
at grid p = 
    (_content grid) V.! (id_ grid p)

ats :: Grid a -> [(Int,Int)] -> [a]
ats grid = map (grid `at`)

maxrow :: Grid a -> Int
maxrow grid =
    (ln `div` nc) - 1
    where nc = _num_col grid
          ln = V.length $ _content grid

maxcol :: Grid a -> Int
maxcol grid =
    (_num_col grid) - 1

bottom_right :: Grid a -> (Int,Int)
bottom_right grid = (maxcol grid,maxrow grid)

top_left :: Grid a -> (Int,Int)
top_left _ = (0,0)

straight_neighbors :: Grid a -> (Int,Int) -> [(Int,Int)]
straight_neighbors grid (col,row) =
    filter inbound $ [(col - 1, row),(col + 1, row),(col, row - 1),(col, row + 1)]
    where inbound (c,r) = c >= 0 && r >= 0 && c <= maxcol grid && r <= maxrow grid

all_neighbors :: Grid a -> (Int,Int) -> [(Int,Int)]
all_neighbors grid (col,row) =
    filter inbound $ [(col - 1, row + 1),(col, row + 1),(col + 1, row + 1),(col - 1, row),(col + 1, row),(col - 1, row - 1),(col, row - 1),(col + 1, row - 1)]
    where inbound (c,r) = c >= 0 && r >= 0 && c <= maxcol grid && r <= maxrow grid

downward_wrapping :: Grid a -> (Int,Int) -> [(Int,Int)]
downward_wrapping grid (col,row) =
    [(col, (row + 1) `mod` (1 + maxrow grid))]

upward_wrapping :: Grid a -> (Int,Int) -> [(Int,Int)]
upward_wrapping grid (col,0) = [(col,maxrow grid)]
upward_wrapping grid (col,row) = [(col,row - 1)]

rightward_wrapping :: Grid a -> (Int,Int) -> [(Int,Int)]
rightward_wrapping grid (col,row) =
    [((col + 1) `mod` (1 + maxcol grid), row)]

leftward_wrapping :: Grid a -> (Int,Int) -> [(Int,Int)]
leftward_wrapping grid (0,row) = [(maxcol grid,row)]
leftward_wrapping grid (col,row) = [(col - 1,row)]

grid_poss :: Grid a -> [(Int,Int)]
grid_poss grid =
    [(c,r) | c <- [0..nc], r <- [0..nr]]
    where nr = maxrow grid
          nc = maxcol grid

grid_update :: Grid a -> (Int,Int) -> a -> Grid a
grid_update grid pos val =
    grid { _content = (_content grid) V.// [(id_ grid pos,val)] }

grid_updates :: Grid a -> [((Int,Int),a)] -> Grid a
grid_updates grid vals =
    grid { _content = (_content grid) V.// (map (\(p,v) -> (id_ grid p,v)) vals) }

grid_map :: (a -> b) -> Grid a -> Grid b
grid_map f grid =
    grid { _content = V.map f $ _content grid }

grid_mapl :: (a -> a) -> Grid a -> [(Int,Int)] -> Grid a
grid_mapl f grid poss =
    let ids = map (id_ grid) poss in
        let vals = map f $ map ((_content grid) V.!) ids in
            grid { _content = (_content grid) V.// (zip ids vals) }

grid_elem :: Eq a => a -> Grid a -> [(Int,Int)]
grid_elem n grid =
    V.toList $ V.map coord $ V.elemIndices n $ _content grid
    where coord n = let (r,c) = n `divMod` (_num_col grid) in (c,r)

grid_all :: (a -> Bool) -> Grid a -> Bool
grid_all p = V.all p . _content

foldneighbors :: (Grid b -> (Int,Int) -> [(Int,Int)]) -> (a -> (Int,Int) -> b -> [((Int,Int),b)] -> a) -> a -> Grid b -> a
foldneighbors neighbors next z grid =
    foldl next0 z points
    where points = grid_poss grid 
          next0 acc pos =
              let neigh = map (\coord -> (coord,grid `at` coord)) $ neighbors grid pos
                  value = grid `at` pos in
                  next acc pos value neigh

mapneighbors :: (Grid a -> (Int,Int) -> [(Int,Int)]) -> ((Int,Int) -> [((Int,Int),a)] -> a -> b) -> Grid a -> Grid b
mapneighbors neighbors f grid =
    Grid { _content = V.generate (nc * nr) f', _num_col = nc }
    where nr = 1 + maxrow grid
          nc = 1 + maxcol grid
          f' i =
              let (r,c) = i `divMod` nc in
                  let neighsval = map (\coord -> (coord,grid `at` coord)) $ neighbors grid (c,r)
                      val = grid `at` (c,r) in
                      f (c,r) neighsval val

count_if :: (a -> Bool) -> Grid a -> Integer
count_if p grid =
    V.foldl count 0 (_content grid)
    where count acc x
            | p x = acc + 1
            | otherwise = acc





