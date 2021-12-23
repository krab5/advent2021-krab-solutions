{-# LANGUAGE TupleSections #-}
module Geom where

import Data.Function
import Data.List
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Maybe

type Vec3 = V.Vector Integer

vec :: Integer -> Integer -> Integer -> Vec3
vec x y z = V.fromList [x, y, z]

xx :: Int
xx = 1

yy :: Int
yy = 2

zz :: Int
zz = 3

get :: Num a => V.Vector a -> Int -> a
get v a 
    | a < 0 = - (get v (-a))
    | otherwise = v ! (a - 1)

set :: V.Vector a -> Int -> a -> V.Vector a
set v i x = v V.// [(i - 1,x)]

opp :: Num a => V.Vector a -> Int -> V.Vector a
opp v i = set v i (get v (-i))

diff :: Vec3 -> Vec3 -> Vec3
diff =
    V.zipWith (-)

absdiff :: Vec3 -> Vec3 -> Vec3
absdiff =
    V.zipWith ad
    where ad x y
            | x > y = x - y
            | otherwise = y - x

translate :: Vec3 -> Vec3 -> Vec3
translate =
    V.zipWith (+)

uminus :: Vec3 -> Vec3
uminus = V.map (\x -> -x)

vmap :: (Int -> a -> b) -> V.Vector a -> V.Vector b
vmap f =
    V.imap f' 
    where f' i x = f (i + 1) x

type Transfo3 = V.Vector Int

tran :: (Int, Int, Int) -> Transfo3
tran (x,y,z) = V.fromList [x, y, z]

app_transfo :: Vec3 -> Transfo3 -> Vec3
app_transfo v t =
    V.map (v `get`) t

comp :: Transfo3 -> Transfo3 -> Transfo3
comp t1 t2 =
    V.map (t1 `get`) t2

invert :: Transfo3 -> Transfo3
invert t =
    V.fromList $ map (find_id 0) [1..3]
    where find_id n x
              | t ! n == x = (n + 1)
              | t ! n == (- x) = - (n + 1)
              | otherwise = find_id (n + 1) x

infixr 1 ///

(///) = app_transfo

indices :: Vec3 -> Integer -> [Int]
indices v a =
    map (+ 1) $ V.toList $ V.findIndices (== a) v

manhattan :: Vec3 -> Vec3 -> Integer
manhattan v1 v2 =
    V.sum $ v1 `absdiff` v2

max_manhattan :: [Vec3] -> (Vec3,Vec3,Integer)
max_manhattan l =
    maximumBy (compare `on` thrd) $ map (\(x1,x2) -> (x1, x2, manhattan x1 x2)) $ gen l
    where gen [] = []
          gen (x:xs) = map (x,) xs ++ gen xs
          thrd (_,_,x) = x




