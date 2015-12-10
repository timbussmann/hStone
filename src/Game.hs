module Game where

import           Data.List

data Card = Card { power  :: Int
                 , health :: Int
                 , cost   :: Int } deriving(Show, Eq)

data Player = Player { name        :: String
                     , hand        :: [Card]
                     , public      :: [Card]
                     , deck        :: [Card]
                     , totalMana   :: Int
                     , currentMana :: Int
                     , hp          :: Int } deriving(Show, Eq)

data Board = Board { activePlayer   :: Player
                   , inactivePlayer :: Player } deriving(Show, Eq)

playCard :: Player -> Card -> Player
playCard player card = player { public = card : public player
                              , hand = delete card (hand player)
                              , currentMana = currentMana player - cost card }

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
endTurn board = Board ((refreshCurrentMana . increateTotalMana . drawDeckCard) $ inactivePlayer board) (activePlayer board)

drawDeckCard :: Player -> Player
drawDeckCard player = let (x:xs) = deck player
                      in player { hand = x : hand player, deck = xs }

increateTotalMana :: Player -> Player
increateTotalMana player = player { totalMana = totalMana player + 1 }

refreshCurrentMana :: Player -> Player
refreshCurrentMana player = player { currentMana = totalMana player }

evaluateWinner :: Board -> Maybe Player
evaluateWinner b
  | hp p2 <= 0 = Just p1
  | hp p1 <= 0 = Just p2
  | otherwise = Nothing
  where p1 = activePlayer b
        p2 = inactivePlayer b

action :: Board -> (Board -> Board) -> Either Player Board
action board action =
  let newBoard = action board
      winner = evaluateWinner newBoard
  in maybe (Right newBoard) Left winner
