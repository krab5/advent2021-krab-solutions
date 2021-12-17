{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import Data.Maybe
import Control.Monad
import Text.Printf

{-
xn vx0 x0 n = if n > vx0 then xn vx0 x0 vx0 else x0 + n*vx0 - ((n*(n-1)) `div` 2)
yn vy0 y0 n = y0 + n*vy0 - ((n*(n-1)) `div` 2)
-}

xn :: Integer -> Integer -> [Integer]
xn 0  x = repeat x
xn vx x = x:(xn (vx - 1) (x + vx))

yn :: Integer -> Integer -> [Integer]
yn vy y = y:(yn (vy - 1) (y + vy))

test :: (Integer,Integer) -> ((Integer,Integer),(Integer,Integer)) -> (Integer,Integer) -> Maybe (Integer,Integer)
test (x0,y0) ((xmin,xmax),(ymin,ymax)) (vx0,vy0) =
    search (xn vx0 x0) (yn vy0 y0)
    where search (x:xs) (y:ys)
            | x >= xmin && x <= xmax && y >= ymin && y <= ymax = Just (x,y)
            | y < ymin || x > xmax = Nothing
            | otherwise = search xs ys

findall :: (Integer,Integer) -> ((Integer,Integer),(Integer,Integer)) -> [((Integer,Integer),(Integer,Integer))]
findall (x0,y0) ((xmin,xmax),(ymin,ymax)) =
    concat $ map get_for_x [minvx..maxvx]
    where get_for_x vx = catMaybes $ map (\vy -> ((vx,vy),) <$> test (x0,y0) ((xmin,xmax),(ymin,ymax)) (vx,vy)) [minvy..maxvy]
          minvy = - maxvy
          maxvy = (abs ymin)
          minvx = 0
          maxvx = xmax + 1

process :: [String] -> IO ()
process [sxmin,sxmax,symin,symax] =
    let xmin = read sxmin
        xmax = read sxmax
        ymin = read symin
        ymax = read symax 
        (x0,y0) = (0,0) in
        let result = findall (x0,y0) ((xmin,xmax),(ymin,ymax)) in do
            forM_ result $ \((vx,vy),(tx,ty)) -> do
                printf "Velocity (%d,%d) hits target @ (%d,%d), with max y=%d\n" vx vy tx ty (yn vy y0 !! (abs $ fromInteger vy))
            putStrLn $ "Number of valid velocities: " ++ (show $ length result)

main :: IO ()
main = do
    ls <- getArgs
    if length ls < 4 then putStrLn $ "Usage: Main <xmin> <xmax> <ymin> <ymax>" else process (take 4 ls)

