module Main where

import System.Environment
import Data.Char
import qualified Data.Vector as V
import Grid
import Image

parse_file :: String -> (BitVect,BitGrid)
parse_file content =
    let (vec,rem) = span (not . isSpace) content in
        let img = dropWhile isSpace rem in
            (parse_map vec, parse_image img)

main :: IO ()
main = do
    (filename:snum:_) <- getArgs
    (vec,img) <- parse_file <$> readFile filename
    num <- return $ (read snum :: Integer)
    putStrLn $ "Enhance vector (size=" ++ (show $ V.length vec) ++ "):"
    putStrLn $ show_map vec
    putStrLn "Start image:"
    putStrLn $ show_image img
    putStrLn $ "(surrounded by " ++ (show $ show_bit False) ++ ")"
    putStrLn ""
    let (inf,result) = enhanceN vec num False img in do
        putStrLn $ "Enhance process after " ++ (show num) ++ " steps:"
        putStrLn $ show_image result
        putStrLn $ "(surrounded by " ++ (show $ show_bit inf) ++ ")"
        putStrLn $ "Number of #: " ++ (show $ count_if id result)



