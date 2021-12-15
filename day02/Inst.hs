module Inst where

import Control.Monad.State

data Inst =
      Forward Integer
    | Down Integer
    | Up Integer

instance Show Inst where
  show (Forward i) = '→':(show i)
  show (Down i) = '↓':(show i)
  show (Up i) = '↑':(show i)

parse :: String -> Inst
parse str =
    let (kw, _:val) = span (/= ' ') str in
        guess kw $ read val
    where guess "forward" = Forward
          guess "down" = Down
          guess "up" = Up

parseAll :: [String] -> [Inst]
parseAll = map parse . filter (not . null)

data Pos = Pos {
        depth :: Integer,
        abscissa :: Integer
    }

instance Show Pos where
  show (Pos d x) = "(z↓=" ++ (show d) ++ ",x⃗=" ++ (show x) ++ ",π=" ++ (show $ d * x) ++ ")"

exec :: Pos -> Inst -> Pos
exec (Pos d x) inst =
    case inst of
      Forward i -> Pos d (x + i)
      Down i -> Pos (d + i) x
      Up   i -> Pos (d - i) x

execAll :: Pos -> [Inst] -> Pos
execAll =
    foldl exec


data Pos2 = Pos2 {
        depth' :: Integer,
        abscissa' :: Integer,
        aim' :: Integer
    }

instance Show Pos2 where
  show (Pos2 d x a) = "(z↓=" ++ (show d) ++ ",x⃗=" ++ (show x) ++ ",α⃕=" ++ (show a) ++ ",π=" ++ (show $ d * x) ++ ")"

exec2 :: Pos2 -> Inst -> Pos2
exec2 p@(Pos2 d x a) inst =
    case inst of
      Forward i -> p { depth' = d + a * i, abscissa' = x + i }
      Down i -> p { aim' = a + i }
      Up i -> p { aim' = a - i }

execAll2 :: Pos2 -> [Inst] -> Pos2
execAll2 = foldl exec2



