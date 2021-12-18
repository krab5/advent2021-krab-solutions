module Snailfish where

data Snailfish = Leaf Integer | Node Snailfish Snailfish

instance Show Snailfish where
  show (Leaf n) = show n
  show (Node l r) = "[" ++ (show l) ++ "," ++ (show r) ++ "]"

explode :: Snailfish -> Maybe Snailfish
explode t =
    (\(_,_,x) -> x) <$> explode1 0 t
    where explode1 _ t@(Leaf _) = Nothing
          explode1 4   (Node (Leaf nl) (Leaf nr)) =
              Just (Just nl, Just nr, Leaf 0)
          explode1 n   (Node l r) =
              case explode1 (n + 1) l of
                Just x -> up_left x r
                Nothing ->
                    case explode1 (n + 1) r of
                      Just x -> up_right x l
                      Nothing -> Nothing
          up_left (nl, nr, t) r =
              let (nr', r') = propagate_right nr r in
                  Just (nl, nr', Node t r')
          up_right (nl, nr, t) l =
              let (nl', l') = propagate_left nl l in
                  Just (nl', nr, Node l' t)
          propagate_right Nothing r = (Nothing, r)
          propagate_right (Just nr) (Leaf nr') = (Nothing, Leaf $ nr + nr')
          propagate_right (Just nr) (Node l r) = 
              let (nr',l') = propagate_right (Just nr) l in
                  (nr',Node l' r)
          propagate_left Nothing l = (Nothing, l)
          propagate_left (Just nl) (Leaf nl') = (Nothing, Leaf $ nl + nl')
          propagate_left (Just nl) (Node l r) =
              let (nl',r') = propagate_left (Just nl) r in
                  (nl',Node l r')

split :: Snailfish -> Maybe Snailfish
split (Leaf n)
    | n >= 10 = let d = n `div` 2 in Just $ Node (Leaf d) (Leaf $ n - d)
    | otherwise = Nothing
split (Node l r) =
    case split l of
      Just l' -> Just $ Node l' r
      Nothing -> 
          case split r of
            Just r' -> Just $ Node l r'
            Nothing -> Nothing

reduce :: Snailfish -> Snailfish
reduce sn =
    case explode sn of
      Just sn' -> reduce sn'
      Nothing ->
          case split sn of
            Just sn' -> reduce sn'
            Nothing -> sn

add :: Snailfish -> Snailfish -> Snailfish
add x y = reduce $ Node x y

ssum :: [Snailfish] -> Snailfish
ssum = foldl1 add

magnitude :: Snailfish -> Integer
magnitude (Leaf n) = n
magnitude (Node l r) =
    3 * (magnitude l) + 2 * (magnitude r)




