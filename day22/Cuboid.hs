module Cuboid where

import qualified Data.Set as S
import Text.Printf
import Data.Char

data Cuboid a = Cuboid {
    xmin :: a, xmax :: a,
    ymin :: a, ymax :: a,
    zmin :: a, zmax :: a
} deriving (Eq,Ord)
type Cuboids a = S.Set (Cuboid a)

empty_cuboids :: Cuboids a
empty_cuboids = S.empty

data Axis = X | Y | Z | MX | MY | MZ deriving (Eq,Ord,Enum,Show)

instance Show a => Show (Cuboid a) where
  show (Cuboid xmin xmax ymin ymax zmin zmax) =
      printf "[%s,%s]×[%s,%s]×[%s,%s]" (show xmin) (show xmax) (show ymin) (show ymax) (show zmin) (show zmax)

cuboid :: Ord a => (a,a) -> (a,a) -> (a,a) -> Cuboid a
cuboid (x1,x2) (y1,y2) (z1,z2) =
    Cuboid (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2) (min z1 z2) (max z1 z2)

cube :: a -> a -> a -> Cuboid a
cube x y z = Cuboid x x y y z z

parse_int_cuboid :: String -> Cuboid Integer
parse_int_cuboid str =
    let (first,rem) = span (/= ',') str in
        let (second,rem') = span (/= ',') $ dropWhile (not . isAlpha) rem in
            let third = dropWhile (not . isAlpha) rem' in
                cuboid (parse1 first) (parse1 second) (parse1 third)
    where parse1 (c:'=':rem) =
            let (lhs,rem') = span (/= '.') rem in
                let rhs = dropWhile (== '.') rem' in
                    (read lhs, read rhs)

subcub :: Ord a => Cuboid a -> Cuboid a -> Bool
subcub (Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) (Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2) =
    xmin1 >= xmin2 && xmax1 <= xmax2 && ymin1 >= ymin2 && ymax1 <= ymax2 && zmin1 >= zmin2 && zmax1 <= zmax2

subcubstrict :: Ord a => Cuboid a -> Cuboid a -> Bool
subcubstrict (Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) (Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2) =
    xmin1 > xmin2 && xmax1 < xmax2 && ymin1 > ymin2 && ymax1 < ymax2 && zmin1 > zmin2 && zmax1 < zmax2

disjoint :: Ord a => Cuboid a -> Cuboid a -> Bool
disjoint (Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) (Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2) =
    xmax1 < xmin2 || xmax2 < xmin1 || ymax1 < ymin2 || ymax2 < ymin1 || zmax1 < zmin2 || zmax2 < zmin1

intersect :: Ord a => Cuboid a -> Cuboid a -> Cuboids a
intersect c@(Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) c'@(Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2)
    | disjoint c c' = S.empty
    | otherwise =
        S.singleton (Cuboid (max xmin1 xmin2) (min xmax1 xmax2)
                            (max ymin1 ymin2) (min ymax1 ymax2)
                            (max zmin1 zmin2) (min zmax1 zmax2))

intersects :: Ord a => Cuboids a -> Cuboid a -> Cuboids a
intersects cs c =
    S.foldl (\acc c' -> S.union acc (c `intersect` c')) S.empty cs

slice :: (Ord a,Num a) => Axis -> a -> Cuboid a -> [Cuboid a]
slice X x c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | x >= xmax || x < xmin = [c]
    | otherwise =
        [Cuboid xmin  x    ymin ymax zmin zmax,
         Cuboid (x+1) xmax ymin ymax zmin zmax]
slice Y y c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | y >= ymax || y < ymin = [c]
    | otherwise =
        [Cuboid xmin xmax ymin  y    zmin zmax,
         Cuboid xmin xmax (y+1) ymax zmin zmax]
slice Z z c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | z >= zmax || z < zmin = [c]
    | otherwise =
        [Cuboid xmin xmax ymin ymax zmin  z,
         Cuboid xmin xmax ymin ymax (z+1) zmax]
slice MX x c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | x > xmax || x <= xmin = [c]
    | otherwise =
        [Cuboid x    xmax  ymin ymax zmin zmax,
         Cuboid xmin (x-1) ymin ymax zmin zmax]
slice MY y c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | y > ymax || y <= ymin = [c]
    | otherwise =
        [Cuboid xmin xmax y    ymax  zmin zmax,
         Cuboid xmin xmax ymin (y-1) zmin zmax]
slice MZ z c@(Cuboid xmin xmax ymin ymax zmin zmax)
    | z > zmax || z <= zmin = [c]
    | otherwise =
        [Cuboid xmin xmax ymin ymax z     zmax,
         Cuboid xmin xmax ymin ymax zmin  (z-1)]

infixl 4 `andthen`

andthen :: Ord a => (Cuboids a, Cuboid a) -> (Cuboid a -> [Cuboid a]) -> (Cuboids a, Cuboid a)
andthen (acc,c) f =
    case f c of
      [c'] -> (acc,c')
      [c',s] -> (S.insert s acc, c')

tie :: Ord a => Cuboid a -> (Cuboids a, Cuboid a)
tie c = (S.empty, c)

difference :: (Ord a,Num a) => Cuboid a -> Cuboid a -> Cuboids a
difference c@(Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) c'@(Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2)
    | disjoint c c' = S.singleton c
    | otherwise =
        fst $ tie c
        `andthen` (slice X  xmax2)
        `andthen` (slice MX xmin2) 
        `andthen` (slice Y  ymax2) 
        `andthen` (slice MY ymin2) 
        `andthen` (slice Z  zmax2) 
        `andthen` (slice MZ zmin2) 

differences :: (Ord a,Num a) => Cuboids a -> Cuboid a -> Cuboids a
differences cs c' =
    S.foldl (\acc c -> S.union acc (c `difference` c')) S.empty cs

union :: (Ord a, Num a) => Cuboid a -> Cuboid a -> Cuboids a
union c1@(Cuboid xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) c2@(Cuboid xmin2 xmax2 ymin2 ymax2 zmin2 zmax2)
    | c1 `subcub` c2 = S.singleton c2
    | c2 `subcub` c1 = S.singleton c1
    | xmin1 == xmin2 && xmax1 == xmax2 && ymin1 == ymin2 && ymax1 == ymax2 && (zmin1 == zmax2 || zmin2 == zmax1) = 
        S.singleton $ Cuboid xmin1 xmax1 ymin1 ymax1 (min zmin1 zmin2) (max zmax1 zmax2)
    | xmin1 == xmin2 && xmax1 == xmax2 && zmin1 == zmin2 && zmax1 == zmax2 && (ymin1 == ymax2 || ymin2 == ymax1) = 
        S.singleton $ Cuboid xmin1 xmax1 (min ymin1 ymin2) (max ymax1 ymax2) zmin1 zmax1
    | zmin1 == zmin2 && zmax1 == zmax2 && ymin1 == ymin2 && ymax1 == ymax2 && (xmin1 == xmax2 || xmin2 == xmax1) = 
        S.singleton $ Cuboid (min xmin1 xmin2) (max xmax1 xmax2) ymin1 ymax1 zmin1 zmax1
    | otherwise =
        case S.lookupMin $ c1 `intersect` c2 of
          Nothing -> S.fromList [c1,c2]
          Just i ->
              let c1' = c1 `difference` i
                  c2' = c2 `difference` i in
                  if S.size c1' <= S.size c2'
                      then S.insert c2 c1'
                      else S.insert c1 c2'

unions :: (Ord a, Num a) => Cuboids a -> Cuboid a -> Cuboids a
unions cs c =
    let (noninter,inter) = S.partition (disjoint c) cs in
        S.union noninter (union1 c inter)
    where union1 c cs 
              | S.null cs = S.singleton c
              | (c',cs') <- S.deleteFindMin cs =
                  let us = c `union` c' in
                      S.foldl union2 S.empty $ S.map (unions cs') us
          union2 acc cs =
              S.foldl unions acc cs

coherent :: (Ord a, Num a) => Cuboids a -> Bool
coherent cs =
    coh $ S.toList cs
    where coh [] = True
          coh (x:xs) =
              all (coh1 x) xs && coh xs
          coh1 x y = disjoint x y

discrete_size :: (Num a) => Cuboid a -> a
discrete_size (Cuboid xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

discrete_sizes :: (Num a) => Cuboids a -> a
discrete_sizes =
    S.foldl (\acc c -> acc + discrete_size c) 0




