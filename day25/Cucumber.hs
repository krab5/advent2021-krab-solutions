module Cucumber where

import Grid

data Cucumber = Empty | South | East deriving (Eq,Ord,Enum)

instance Show Cucumber where
  show Empty = "."
  show South = "v"
  show East = ">"

parse_cucumbers :: String -> Grid Cucumber
parse_cucumbers =
    parse_grid parse_cucumber
    where parse_cucumber 'v' = South
          parse_cucumber '>' = East
          parse_cucumber '.' = Empty

show_cucumbers :: Grid Cucumber -> String
show_cucumbers =
    show_grid show_cucumber
    where show_cucumber Empty = '.'
          show_cucumber East  = '>'
          show_cucumber South = 'v'

step :: Grid Cucumber -> Grid Cucumber
step grid =
    mapneighbors updown progress_south $ mapneighbors leftright progress_east grid
    where leftright g = (++) <$> leftward_wrapping g <*> rightward_wrapping g
          updown    g = (++) <$> upward_wrapping g <*> downward_wrapping g
          progress_east _ [(_,lv),(_,rv)] v
              | v == East && rv == Empty = Empty
              | v == Empty && lv == East = East
              | otherwise = v
          progress_south _ [(_,uv),(_,dv)] v
              | v == South && dv == Empty = Empty
              | v == Empty && uv == South = South
              | otherwise = v

step_until_no_progress :: Grid Cucumber -> (Integer,Grid Cucumber)
step_until_no_progress start =
    next 0 start
    where next n grid =
            let grid' = step grid in
                if grid == grid' 
                    then (n, grid)
                    else next (n + 1) grid'


