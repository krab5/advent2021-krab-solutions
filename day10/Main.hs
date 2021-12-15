module Main where

import System.Environment
import Control.Monad
import Data.List (insert)

openning :: Char -> Bool
openning c = c `elem` "([{<"

closing :: Char -> Bool
closing c = c `elem` ")]}>"

closed_by :: Char -> Char -> Bool
closed_by '(' ')' = True
closed_by '[' ']' = True
closed_by '{' '}' = True
closed_by '<' '>' = True
closed_by _   _   = False

closing_char :: Char -> Char
closing_char '(' = ')' 
closing_char '[' = ']' 
closing_char '{' = '}' 
closing_char '<' = '>' 
closing_char x = x

corruption :: [Char] -> [Char]
corruption s =
    next [] [] s
    where next errs _ [] = errs
          next errs [] (x:xs)
            | openning x = next errs [x] xs
          next errs openned@(o:os) (x:xs)
            | openning x = next errs (x:openned) xs
            | closing x && o `closed_by` x = next errs os xs
            | closing x = next (x:errs) os xs
            | otherwise = next errs openned xs

complete :: [Char] -> [Char]
complete s =
    next [] s
    where next openned [] = foldr (\x acc -> (closing_char x):acc) "" openned
          next [] (x:xs) | openning x = next [x] xs
          next openned@(o:os) (x:xs)
            | openning x = next (x:openned) xs
            | closing x && o `closed_by` x = next os xs
            | otherwise = next openned xs

score :: [Char] -> Integer
score =
    sum . map score1
    where score1 ')' = 3
          score1 ']' = 57
          score1 '}' = 1197
          score1 '>' = 25137

score2 :: [Char] -> Integer
score2 =
    foldl (\acc x -> acc * 5 + score1 x) 0
    where score1 ')' = 1
          score1 ']' = 2
          score1 '}' = 3
          score1 '>' = 4

main :: IO ()
main = do
    (filename:_) <- getArgs
    ls <- lines <$> readFile filename
    (scorr,scomp) <- foldM step init ls
    putStrLn $ "Score for corruption: " ++ (show scorr)
    putStrLn $ "Score for completion: " ++ (show $ med scomp)
    where init = (0,[])
          step (scorr, scomp) str = do
            putStr $ "  " ++ str ++ " => "
            let errs = corruption str in
                if null errs
                    then stepComp scomp str  >>= (\scomp' -> return (scorr,scomp'))
                    else stepCorr scorr errs >>= (\scorr' -> return (scorr',scomp))
          stepComp scomp str =
              let comp = complete str in
                  let sc = score2 comp in do
                      putStrLn $ "missing " ++ comp ++ " |score=" ++ show sc ++ "|"
                      return $ insert sc scomp
          stepCorr scorr errs = 
              let sc = score errs in do
                putStrLn $ "errors " ++ errs ++ " |score=" ++ show sc ++ "|"
                return $ scorr + sc
          med l = l !! ((length l) `div` 2)



