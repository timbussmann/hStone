module Game where

import           Data.List

data Card = Card { power  :: Int
                 , health :: Int
                 , cost   :: Int } deriving(Show, Eq)

data Player = Player { name   :: String
                     , hand   :: [Card]
                     , public :: [Card]
                     , deck   :: [Card]
                     , mana   :: Int
                     , hp     :: Int } deriving(Show)

instance Eq Player where
  x == y = name x == name y

data Board = Board { activePlayer   :: Player
                   , inactivePlayer :: Player } deriving(Show, Eq)

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

endTurn :: Board -> Board
endTurn board = Board ((increaseMana . drawDeckCard) $ inactivePlayer board) (activePlayer board)

drawDeckCard :: Player -> Player
drawDeckCard player = let (x:xs) = deck player
                      in player { hand = x : hand player, deck = xs }

increaseMana :: Player -> Player
increaseMana player = player { mana = mana player + 1 }
