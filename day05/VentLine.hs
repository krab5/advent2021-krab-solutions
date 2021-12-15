module VentLine where

import Data.Char

data VentLine = VentLine (Int,Int) (Int,Int)

instance Show VentLine where
  show (VentLine (x1,y1) (x2,y2)) =
      show x1 ++ "," ++ show y1 ++ " -> " ++ show x2 ++ "," ++ show y2

isHorizontal :: VentLine -> Bool
isHorizontal (VentLine (_,y1) (_,y2)) = y1 == y2

isVertical :: VentLine -> Bool
isVertical (VentLine (x1,_) (x2,_)) = x1 == x2

parseVent :: String -> VentLine
parseVent str =
    let (lhs,rem) = span (not . isSpace) str in
        let rhs = dropWhile (not . isDigit) rem in
            VentLine (parsePair lhs) (parsePair rhs)
    where parsePair str =
              let (left,_:right) = span (/= ',') str in
                  (read left, read right)

parseAllVents :: [String] -> [VentLine]
parseAllVents = map parseVent

straightLines :: [VentLine] -> [VentLine]
straightLines = filter ((||) <$> isHorizontal <*> isVertical)

startx :: VentLine -> Int
startx (VentLine (x1,_) (x2,_)) = min x1 x2

endx :: VentLine -> Int
endx (VentLine (x1,_) (x2,_)) = max x1 x2

starty :: VentLine -> Int
starty (VentLine (_,y1) (_,y2)) = min y1 y2

endy :: VentLine -> Int
endy (VentLine (_,y1) (_,y2)) = max y1 y2

segment :: VentLine -> [(Int,Int)]
segment (VentLine (x1,y1) (x2,y2)) =
    next (dir x1 x2) (dir y1 y2) (x1,y1)
    where dir x1 x2
            | x1 == x2 = 0
            | x1 < x2  = 1
            | x1 > x2  = -1
          next xdir ydir (x,y) 
            | (x,y) == (x2,y2) = [(x,y)] 
            | otherwise = (x,y):(next xdir ydir (x + xdir, y + ydir))

