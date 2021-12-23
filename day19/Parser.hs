module Parser where

import Data.Char
import Geom

parse_vec :: String -> Vec3
parse_vec str =
    let (first,_:rem) = span (/= ',') str in
        let (second,_:third) = span (/= ',') rem in
            vec (read first) (read second) (read third)

parse_section :: [String] -> [Vec3]
parse_section = (map parse_vec) . tail

cut_sections :: [String] -> [[String]]
cut_sections [] = []
cut_sections ([]:xs) = cut_sections xs
cut_sections l =
    let (sec,rem) = span (not . null) l in
        sec:(cut_sections rem)

parse :: String -> [[Vec3]]
parse =
    map parse_section . cut_sections . lines

parse_file :: String -> IO [[Vec3]]
parse_file fn = readFile fn >>= return . parse




