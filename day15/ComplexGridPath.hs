module ComplexGridPath (complex_grid_shortest) where

import qualified Data.Set as S
import Grid
import Path

{-
A more advanced system of grid, where the value at a given point is calculated
from an initial grid and its position in term of block.

This allows to effectively handle a large grid where elements are calculated
and not stored.
-}

data CGrid a = CGrid {
    repeat_c :: Int, -- number of times the grid is repeated rightward
    repeat_r :: Int, -- number of times the grid is repeated downward
    content :: Grid a, -- original grid
    get_element :: (Int,Int) -> a -> a -- Function giving the "true" value from the original value and the block coordinates
}

instance Space CGrid where
  {-
     Any position is divided into a position within a block, plus the position
     of the block itself. For instance, in a complex grid made of an original
     of 4x4 repeated 2 times rightward and downward, coordinate (7,2) is
     associated to block coordinate (1,0) and to coordinate (3,2) within this
     block. The element for the given position is the composition of the element
     at the local position in the original grid with the coordinate of the
     block, through the get_element function.
  -}
  cgrid @ (c,r) =
      let (block_col, local_col) = c `divMod` nc
          (block_row, local_row) = r `divMod` nr in
          get_element cgrid (block_col,block_row) (grid `at` (local_col,local_row))
      where grid = content cgrid
            nc = 1 + maxcol grid
            nr = 1 + maxrow grid
  {-
     Positions and neighbors are just copy-paste of the grid; I did not have
     time to abstract that so this is basically the same algorithm but adapted
     to the data structure
  -}
  positions cgrid = 
      S.fromList $ [(c,r) | c <- [0..nc], r <- [0..nr]]
      where grid = content cgrid
            rc = repeat_c cgrid
            rr = repeat_r cgrid
            nc = (1 + maxcol grid) * rc - 1
            nr = (1 + maxrow grid) * rr - 1
  neighbors cgrid (col,row) =
      S.fromList $ filter inbound $ [(col - 1, row),(col + 1, row),(col, row - 1),(col, row + 1)]
      where inbound (c,r) = c >= 0 && r >= 0 && c <= nc && r <= nr
            grid = content cgrid
            rc = repeat_c cgrid
            rr = repeat_r cgrid
            nc = (1 + maxcol grid) * rc - 1
            nr = (1 + maxrow grid) * rr - 1

{-
Function used to calculate the value in the grid at a given point. Basically,
the value is given by adding the block coordinate, and keeping the result
between 1 and 9.
-}
increase_block :: (Int,Int) -> Int -> Int
increase_block (br,bc) x =
    1 + ((x + br + bc - 1) `mod` 9)

{-
Wrapper function to find the distance of the shortest path in the complex grid.
The function takes care of creating said complex grid.
-}
complex_grid_shortest :: Grid Int -> Int -> Int -> Maybe Int
complex_grid_shortest grid repeatc repeatr =
    let cgrid = CGrid { repeat_c = repeatc, repeat_r = repeatr, content = grid, get_element = increase_block } in
        shortest (+) cgrid (0,0) 0 (nc,nr)
    where nc = (1 + maxcol grid) * repeatc - 1
          nr = (1 + maxrow grid) * repeatr - 1


