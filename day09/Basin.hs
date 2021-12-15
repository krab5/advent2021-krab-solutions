module Basin where

import Grid
import Data.List (concat,find)
import qualified Data.Set as S
import Text.Printf

wells :: Grid -> [((Int,Int),Int)]
wells grid =
    foldneighbors next [] grid
    where next acc pos val neighs =
            if all (> val) $ map snd neighs then acc ++ [(pos,val)] else acc


basin :: Grid -> (Int,Int) -> (Int,S.Set (Int,Int))
basin grid pos =
    let b = explore (S.singleton pos) pos (grid `at` pos) in
        (S.size b, b)
    where explore acc pos val =
              let neighs = filter (\(p,v) -> v < 9 && not (p `elem` acc)) $ map (\co -> (co,grid `at` co)) $ neighbors grid pos in
                  foldl (\acc (p,v) -> explore (S.insert p acc) p v) acc neighs

hyper_pretty :: Grid -> [S.Set (Int,Int)] -> String
hyper_pretty grid basins =
    "<html>\n" ++
    "<head>\n<style>\n" ++ style ++ "</style>\n</head>\n<body>\n" ++
    "<table>\n" ++ (concat $ map showLine [0..mr]) ++
    "</table>\n</body>\n</html>\n"
    where mc = maxcol grid
          mr = maxrow grid
          nbasins = zip [1..] basins
          which_basin p = fst <$> (find (\(_,basin) -> p `S.member` basin) nbasins)
          showCell p = "  <td" ++ showBasin p ++ ">" ++ (show $ grid `at` p) ++ "</td>\n"
          showBasin p =
              case which_basin p of
                Nothing -> ""
                Just i -> " class=\"c" ++ show i ++ "\""
          showLine row =
              " <tr>\n" ++ (concat $ map showCell $ zip [0..mc] $ repeat row) ++ " </tr>\n"
          style =
            "td {\n  width: 20px;\n  text-align: center;\n}\n\n" ++
                (concat $ map style1 nbasins)
          style1 :: (Integer,S.Set (Int,Int)) -> String
          style1 (n,_) =
              ".c" ++ (show n) ++ " {\n  background-color: " ++ color n ++ ";\n}\n\n"
          color n =
              let r = 100 + (n * 133) `mod` 155
                  g = 100 + (50 + n * 77) `mod` 155
                  b = 100 + (20 + n * 91) `mod` 155
                  in printf "#%02x%02x%02x" r g b



