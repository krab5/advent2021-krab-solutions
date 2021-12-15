module Parser where

import Bingo
import Text.Read (readMaybe)
import Data.Char (isSpace)

parseNumList :: Char -> String -> [Integer]
parseNumList c [] = []
parseNumList c l
  | c `elem` l =
      let (n,r) = span (/= c) l in
          let lr = parseNumList c $ dropWhile (== c) r in
              case readMaybe n of
                Nothing -> lr
                Just nn -> nn:lr
  | otherwise =
      case readMaybe l of
        Nothing -> []
        Just nn -> [nn]

parse_bingo_board :: [String] -> BingoBoard
parse_bingo_board = 
    init_bingo . concat . map (parseNumList ' ')

data Config = Config {
    called_numbers :: [Integer],
    bingo_boards :: [BingoBoard]
} deriving Show

parse_config :: [String] -> Config
parse_config (nums:_:boards) =
    Config {
        called_numbers = numbers,
        bingo_boards = nextBoard boards
    }
    where numbers = parseNumList ',' nums
          nextBoard [] = []
          nextBoard ([]:boards) = nextBoard boards
          nextBoard boards = 
              let (thisBoard,remBoards) = splitAt 5 boards in
                  (parse_bingo_board thisBoard):(nextBoard remBoards)



