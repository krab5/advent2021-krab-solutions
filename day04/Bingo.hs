module Bingo where

import Data.List (intercalate,minimumBy,maximumBy)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Data.Set as S
import Text.Printf

data BingoBoard = BingoBoard {
    _content :: V.Vector Integer,
    _marking :: S.Set Int
}

instance Show BingoBoard where
  show (BingoBoard content marking) =
      intercalate "\n" $ map (intercalate " ") $ cut $ map showNum $ V.toList $ V.indexed content
      where showNum (n,e)
                | n `S.member` marking = printf "% 3v\x35e" e
                | otherwise = printf "% 3v" e
            cut [] = []
            cut l =
                let (first,second) = splitAt 5 l in
                    first:(cut second)

cart_id :: (Int,Int) -> Int
cart_id (line,col) = line * 5 + col

id_cart :: Int -> (Int,Int)
id_cart n = n `quotRem` 5

is_marked :: (Int,Int) -> BingoBoard -> Bool
is_marked coord bb = S.member (cart_id coord) (_marking bb)

all_marked :: [(Int,Int)] -> BingoBoard -> Bool
all_marked l bb = all (`is_marked` bb) l

get_unmarked :: BingoBoard -> [Integer]
get_unmarked (BingoBoard content marking) =
    map snd $ V.toList $ V.filter (\(n,e) -> not $ n `S.member` marking) $ V.indexed content

column :: Int -> [(Int,Int)]
column c = [ (i,c) | i <- [0..4] ]

line :: Int -> [(Int,Int)]
line l = [ (l,j) | j <- [0..4] ]

all_columns :: [[(Int,Int)]]
all_columns = map column [0..4]

all_lines :: [[(Int,Int)]]
all_lines = map line [0..4]

all_config :: [[(Int,Int)]]
all_config = all_columns ++ all_lines

won :: BingoBoard -> Bool
won bb =
    any (`all_marked` bb) all_config

mark :: Integer -> BingoBoard -> BingoBoard
mark n bb =
    case V.elemIndex n $ _content bb of
      Nothing -> bb
      Just id -> bb { _marking = S.insert id $ _marking bb }

mark_until_won :: [Integer] -> BingoBoard -> Maybe (Int,Integer,BingoBoard)
mark_until_won l bb =
    next l (head l) 0 bb
    where next l x n bb
            | won bb = Just (n,x,bb)
            | [] <- l = Nothing
            | (x:xs) <- l = next xs x (n + 1) (mark x bb)

init_bingo :: [Integer] -> BingoBoard
init_bingo l =
    BingoBoard {
        _content = V.fromList l,
        _marking = S.empty
    }

compete :: [Integer] -> [BingoBoard] -> ((Int,Integer,BingoBoard),(Int,Integer,BingoBoard))
compete numbers boards =
    (best,worst)
    where marked = catMaybes $ map (mark_until_won numbers) boards
          best = minimumBy (\(x,_,_) (y,_,_) -> compare x y) marked
          worst= maximumBy (\(x,_,_) (y,_,_) -> compare x y) marked

score :: Integer -> BingoBoard -> Integer
score lastNum bb = lastNum * (sum $ get_unmarked bb)


