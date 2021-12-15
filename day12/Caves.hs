module Caves where

import Data.Char
import Data.List
import qualified Data.Set as S

data Cave = Cave { cavename :: String } deriving (Eq,Ord)

instance Show Cave where
  show = cavename

is_big :: Cave -> Bool
is_big = any isUpper . cavename

is_start :: Cave -> Bool
is_start = (== "start") . cavename

is_end :: Cave -> Bool
is_end = (== "end") . cavename

data CaveSystem =
    CaveSystem {
        tunnels :: S.Set (Cave,Cave)
    }

instance Show CaveSystem where
  show cs =
      show $ map (\(c1,c2) -> show c1 ++ "--" ++ show c2) $ S.toList $ tunnels cs

empty_cave :: CaveSystem
empty_cave = CaveSystem { tunnels = S.empty }

caves :: CaveSystem -> S.Set Cave
caves cs =
    S.foldl (\acc (x,y) -> S.insert x $ S.insert y acc) S.empty $ tunnels cs

small_caves :: CaveSystem -> S.Set Cave
small_caves =
    S.filter (\c -> not (is_start c) && not (is_end c) && not (is_big c)) . caves

add_tunnel :: CaveSystem -> (Cave,Cave) -> CaveSystem
add_tunnel cs (c1,c2) =
    cs { tunnels = S.insert (norm c1 c2) (tunnels cs) }
    where norm x y
              | x < y = (x,y)
              | otherwise = (y,x)

parse_tunnel :: String -> (Cave,Cave)
parse_tunnel str =
    let (left,_:right) = span (/= '-') str in
        (Cave left, Cave right)

parse_cave_system :: String -> CaveSystem
parse_cave_system str =
    foldl (\acc s -> add_tunnel acc $ parse_tunnel s) empty_cave $ lines str

connected :: CaveSystem -> Cave -> S.Set Cave
connected cs c =
    S.foldl conn_then_add S.empty $ tunnels cs
    where conn_then_add acc (x,y)
            | x == c = S.insert y acc
            | y == c = S.insert x acc
            | otherwise = acc

explore :: CaveSystem -> S.Set Cave -> [Cave] -> Cave -> [[Cave]]
explore cs visited path start
    | is_end start = [path]
    | otherwise =
        let cons = S.toList $ (connected cs start) S.\\ visited in
            concat $ map explore_next cons
        where explore_next next =
                let visited' = if is_big next then visited else S.insert next visited in
                    explore cs visited' (next:path) next

explore_with_small :: CaveSystem -> Cave -> Integer -> S.Set Cave -> [Cave] -> Cave -> S.Set [Cave]
explore_with_small cs small numvisit visited path start
    | is_end start = S.singleton path
    | otherwise =
        let cons = (connected cs start) S.\\ visited in
            S.unions $ S.map explore_next cons
        where explore_next next =
                let (numvisit',visited') = next_visited next in
                    explore_with_small cs small numvisit' visited' (next:path) next
              next_visited next =
                  if is_big next then
                      (numvisit,visited)
                  else if next == small then
                      if numvisit > 0 then
                          (numvisit,S.insert small visited)
                      else
                          (numvisit+1,visited)
                  else (numvisit,S.insert next visited)

explore_all :: CaveSystem -> [[Cave]]
explore_all cs =
    let start = Cave "start" in
        map reverse $ filter (\(x:_) -> is_end x) $ explore cs (S.singleton start) [start] start 

explore_all_with_small :: CaveSystem -> [[Cave]]
explore_all_with_small cs =
    S.toList $ S.unions $ S.map explore1 smalls
    where smalls = small_caves cs
          start = Cave "start"
          explore1 small =
              S.map reverse $ S.filter (\(x:_) -> is_end x) $ explore_with_small cs small 0 (S.singleton start) [start] start


