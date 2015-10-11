module Game where

import           Data.List

data Card = Card { power :: Int
                 , health  :: Int
                 , cost  :: Int } deriving(Show, Eq)

data Player = Player { hand   :: [Card]
                     , public :: [Card]
                     , deck :: [Card]
                     , mana   :: Int
                     , hp :: Int } deriving(Show)

data Board = Board Player Player

playCard :: Player -> Card -> Player
playCard player card = player { public = card : public player
                              , hand = delete card (hand player)
                              , mana = mana player - cost card }

minionAttack :: Card -> Card -> (Card, Card)
minionAttack attacker target = (Card (power attacker) (health attacker - power target) (cost attacker)
                               ,Card (power target) (health target - power attacker) (cost target))
-- should the public hand be on the board instead of the player?

attack :: Board -> Card -> Card -> Board
attack (Board player1 player2) attacker target = let (a, t) = minionAttack attacker target in
    Board (updatePublicCards player1 attacker a) (updatePublicCards player2 target t)
    where
      updatePublicCards player original new
                  | health new > 0 = player { public = replace original new (public player) }
                  | otherwise = player { public = delete original (public player) }

attackPlayer :: Board -> Card -> Player -> Board
attackPlayer (Board player1 player2) attacker target = Board
                                                          player1
                                                          (removeHp player2 (power attacker))

replace :: Card -> Card -> [Card] -> [Card]
replace search new = map (\x -> if x == search then new else x)

removeHp :: Player -> Int -> Player
removeHp player x = player { hp = hp player - x }
