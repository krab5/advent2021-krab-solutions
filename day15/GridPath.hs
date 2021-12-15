module GridPath (simple_grid_shortest) where

import qualified Data.Set as S
import Grid
import Path

{-
A problem configuration for simple grids, basically using the functions
provided by the Grid module.
-}

instance Space Grid where
  (@) = at
  positions = S.fromList . grid_poss
  neighbors grid = S.fromList . straight_neighbors grid

simple_grid_shortest :: Grid Integer -> Maybe Integer
simple_grid_shortest grid =
    shortest (+) grid (0,0) 0 (maxcol grid, maxrow grid)


