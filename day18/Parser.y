{
module Parser where

import Data.Char (isSpace, isDigit)
import Snailfish

}

%name snailfish
%tokentype { Token }
%error { parseError }
%token  '['  { LeftBra   }
        ']'  { RightBra  }
        ','  { Comma     }
        num  { Number $$ }

%%

Snail: '[' Snail1 ',' Snail1 ']'    { Node $2 $4 }

Snail1: Snail                       { $1 }
      | num                         { Leaf $1 }

{

data Token = LeftBra | RightBra | Comma | Number Integer deriving Show

parseError :: [Token] -> a
parseError []  = error "Error in expression"
parseError tks = error $ "Error near " ++ (show $ last tks)

lexer :: String -> [Token]
lexer [] = []
lexer ('[':xs) = LeftBra : lexer xs
lexer (']':xs) = RightBra : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer l@(x:xs)
    | isDigit x =
      let (d,rem) = span isDigit l in
        (Number $ read d) : lexer rem
    | isSpace x = lexer xs
    | otherwise = error $ "Unexpected character '" ++ show x ++ "'"

parse_snailfish :: String -> Snailfish
parse_snailfish = snailfish . lexer
}



