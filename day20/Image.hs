module Image where

import qualified Data.Vector as V
import Grid

type BitVect = V.Vector Bool
type BitGrid = Grid Bool

parse_bit :: Char -> Bool
parse_bit '.' = False
parse_bit '#' = True

show_bit :: Bool -> Char
show_bit True = '#'
show_bit False = '.'

parse_map :: String -> BitVect
parse_map =
    V.fromList . map parse_bit

show_map :: BitVect -> String
show_map =
    V.foldr (\x acc -> (show_bit x):acc) []

parse_image :: String -> BitGrid
parse_image = parse_grid parse_bit

show_image :: BitGrid -> String
show_image = show_grid show_bit

at' :: Bool -> BitGrid -> (Int,Int) -> Bool
at' infvalue grid p@(c,r)
    | c < 0 || c > maxcol grid || r < 0 || r > maxrow grid = infvalue
    | otherwise = grid `at` p

neighs :: (Int,Int) -> [(Int,Int)]
neighs (c,r) =
    [(c - 1, r - 1),(c, r - 1),(c + 1, r - 1)
    ,(c - 1, r    ),(c, r    ),(c + 1, r    )
    ,(c - 1, r + 1),(c, r + 1),(c + 1, r + 1)]

calc :: [Bool] -> Int
calc = foldl (\acc x -> 2*acc + (if x then 1 else 0)) 0

get_id :: Bool -> BitGrid -> (Int,Int) -> Int
get_id infvalue grid (c,r) =
    calc $ map (at' infvalue grid) $ neighs (c,r)

enhance :: BitVect -> Bool -> BitGrid -> (Bool,BitGrid)
enhance map infvalue grid =
    ( map V.! calc (take 9 $ repeat infvalue)
    , Grid { _num_col = mcol + 1, _content = V.generate ((mrow + 1)*(mcol + 1)) gen1 })
    where mrow = (maxrow grid) + 2
          mcol = (maxcol grid) + 2
          gen1 n =
              let (r,c) = n `quotRem` (mcol + 1) in
                  map V.! (get_id infvalue grid (c - 1, r - 1))

enhanceN :: BitVect -> Integer -> Bool -> BitGrid -> (Bool,BitGrid)
enhanceN _ 0 iv g = (iv,g)
enhanceN m n iv g =
    let (iv', g') = enhance m iv g in
      enhanceN m (n - 1) iv' g'




