module Game where

import Control.Monad.State

class Generator g where
  next :: g -> (Integer,g)

data GameState g = GameState {
        gen :: g,
        current_player :: Bool,
        player1_score  :: Integer,
        player1_pos    :: Integer,
        player2_score  :: Integer,
        player2_pos    :: Integer,
        number_turns   :: Integer
    } deriving Show

game_init :: Generator g => g -> Integer -> Integer -> GameState g
game_init g p1 p2 = GameState g (not False) 0 p1 0 p2 0

type GameT g m = StateT (GameState g) m

add_score :: Monad m => Integer -> GameT g m Integer
add_score n =
    state $ \gs ->
        if current_player gs
            then let score = n + player2_score gs in (score,gs { player2_score = score })
            else let score = n + player1_score gs in (score,gs { player1_score = score })

add_mod :: Integer -> Integer -> Integer
add_mod start fwd =
    1 + (start + fwd - 1) `mod` 10

move_forward :: Monad m => Integer -> GameT g m Integer
move_forward n =
    state $ move
    where move gs
            | current_player gs =
                let r = add_mod (player2_pos gs) n in
                    (r, gs { player2_pos = r })
            | otherwise =
                let r = add_mod (player1_pos gs) n in
                    (r, gs { player1_pos = r })

change_player :: Monad m => GameT g m ()
change_player =
    modify $ \gs -> gs { current_player = not $ current_player gs, number_turns = 1 + number_turns gs }

get_number :: (Monad m, Generator g) => GameT g m Integer
get_number =
    state $ \gs -> let (n,g) = next $ gen gs in (n, gs { gen = g })

play_turn :: (Monad m, Generator g) => GameT g m Integer
play_turn = do
    change_player
    n <- get_number
    p <- move_forward n
    score <- add_score p
    return score

play_until_score_above :: (Monad m, Generator g) => Integer -> GameT g m ()
play_until_score_above thresh = do
    score <- play_turn
    if score >= thresh
        then return ()
        else play_until_score_above thresh

doPlay :: (Monad m, Generator g) =>  g -> Integer -> Integer -> Integer -> m (GameState g)
doPlay gen p1 p2 thresh =
    execStateT (play_until_score_above thresh) (game_init gen p1 p2)



