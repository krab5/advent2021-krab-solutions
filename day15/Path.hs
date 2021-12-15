{-# LANGUAGE TupleSections #-}
module Path where

import qualified Data.Set as S
import qualified Data.Map as M

{-
A quite standard, polymorphic implementation of Dijkstra's algorithm for
shortest path.

Note that this algorithm is adapted to work on a grid; formally, the grid
encodes a graph where the weight on every attached edge is equal to the value
in the grid.
-}


{-
I define a typeclass that represent grids in general, with the possibility to 
access an element at a given position, return the list of browsable positions, 
and the neighbors of the given point.

This allows to use a normal grid (as defined in `Grid`) or a grid made of
blocks, where the block's content is calculated on-the-fly (for part 2).
-}
infix 5 @

class Space g where
  (@) :: g a -> (Int,Int) -> a
  positions :: g a -> S.Set (Int,Int)
  neighbors :: g a -> (Int,Int) -> S.Set (Int,Int)

{-
We need to store the distances; we could have used a grid but using a map is
kind of the same in term of performance, and is easier to handle.

The distance map only contains the distance of the nodes to consider; if a key
is not in it, then its associated distance is infinity.
-}
type Distances a = M.Map (Int,Int) a

{-
Visit one node and update the distance of the surrounding unvisited nodes.
Update is given using a cost function, that combines the current distance of the
node (currentDist) and the value in the node.

The function keeps track of the visited nodes and the unvisited ones. The set
of unvisited nodes allow to optimise the browsing, since we never have to find
the next node to visit on the whole grid, but only in the nodes we just updated.

Figuratively, it is like unvisited always contain the "front" of the brosing.
-}
step :: (Ord b, Space g)
     => (b -> a -> b)
     -> g a -> Distances b
     -> S.Set (Int,Int) -> S.Set (Int,Int)
     -> (Int,Int) -> b 
     -> (Distances b, S.Set (Int,Int), S.Set (Int,Int))
step cost grid distances visited unvisited current currentDist =
    let (distances',unvisited') = S.foldl update1 (distances,S.delete current unvisited) $ neighs
        visited' = S.insert current visited in
        (distances', visited', unvisited')
    where neighs = (neighbors grid current) `S.difference` visited
          update1 (dist,unvi) pos = (updateDist dist pos, S.insert pos unvi)
          updateDist dist pos =
              let newDist = cost currentDist (grid @ pos) in
                  case M.lookup pos dist of
                    Just oldDist | oldDist < newDist -> dist
                    _ -> M.insert pos newDist dist -- oldDist >= newDist or pos not in dist

{-
Find the node with minimal distance among the unvisited nodes. Keeping track of
unvisited nodes allows this function to be quite efficient in general.
-}
find_lowest :: (Ord b, Space g) => g a -> Distances b -> S.Set (Int,Int) -> Maybe ((Int,Int),b)
find_lowest grid distances unvisited =
    S.foldl next Nothing unvisited
    where next Nothing pos = (pos,) <$> (M.lookup pos distances)
          next acc@(Just (minpos,minval)) pos =
              case M.lookup pos distances of
                Just dist | dist < minval -> Just (pos,dist)
                _ -> acc

{-
The main browsing function, that complete the remainder of the path from current
to target. This function makes progress with step, then use find_lowset to find
the next node to visit, and loops by calling itself back.

The browsing is finished whenever the target is marked as unvisited, as it means
it is reachable from the current path (and since it is mandatory no need to find
which node is the closes).
-}
next :: (Ord b, Space g)
     => (b -> a -> b)
     -> (Int,Int)
     -> g a -> Distances b
     -> S.Set (Int,Int) -> S.Set (Int,Int)
     -> (Int,Int) -> b
     -> Distances b
next cost target grid distances visited unvisited current currentDist =
    let (distances', visited', unvisited') = step cost grid distances visited unvisited current currentDist in
        if target `S.member` unvisited'
            then distances'
            else 
                case find_lowest grid distances' unvisited' of
                  Nothing -> distances'
                  Just (pos,val) ->
                      next cost target grid distances' visited' unvisited' pos val

{-
Wrapper function for the algorithm, finds the shortest path in a grid from
source to target, with given initial distance for source and following the given
cost function.

The function returns a Maybe because Map.lookup does, but hopefully this should
always be a Just...

Note that the funciton only returns the shortest distance, NOT the path (we 
would need a bit more of functions to do that, but it is not required by the
challenge).
-}
shortest :: (Ord b, Space g) => (b -> a -> b) -> g a -> (Int,Int) -> b -> (Int,Int) -> Maybe b
shortest cost grid source init target =
    let dist' = next cost target grid (M.singleton source init) S.empty S.empty source init in
        M.lookup target dist'





