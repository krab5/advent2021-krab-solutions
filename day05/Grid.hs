module Grid where

import VentLine
import Data.List (intercalate)
import qualified Data.Vector as V

data Grid = Grid {
    _num_rows :: Int,
    _num_cols :: Int,
    _content :: V.Vector Integer
}

init_grid :: (Int,Int) -> Grid
init_grid (nc,nr) =
    Grid nr nc $ V.replicate (nr * nc) 0

instance Show Grid where
  show (Grid nr nc ct) =
      intercalate "\n" $ map printLine [0..nr - 1]
      where printLine ln =
                foldl (\acc n -> acc ++ printCell n) "" $ map (cart_id nc) $ [ (i,ln) | i <- [0..nc - 1] ]
            printCell n =
                let cc = ct V.! n in
                    if cc <= 0 then "."
                               else show cc

cart_id :: Int -> (Int,Int) -> Int
cart_id nc (col,line) = line * nc + col

id_cart :: Int -> Int -> (Int,Int)
id_cart nc n = let (l,c) = n `quotRem` nc in (c,l)

updateGrid :: (Int -> Integer -> Integer) -> [(Int,Int)] -> Grid -> Grid
updateGrid updater indices grid@(Grid nr nc ct) =
    let up = map (\i -> (i, updater i (ct V.! i))) $ map (cart_id nc) indices in
        grid { _content = ct V.// up }

applyVent :: Grid -> VentLine -> Grid
applyVent grid v =
    updateGrid (\_ -> (+1)) (segment v) grid

applyVents :: Grid -> [VentLine] -> Grid
applyVents = foldl applyVent

countDangerous :: Grid -> Integer
countDangerous (Grid _ _ ct) =
    V.foldl (\acc n -> if n > 1 then acc + 1 else acc) 0 ct

