module Game where

import           Data.List

class Unit a where
  damage :: a -> Int -> a
  power :: a -> Int

data Card = Card { cpower  :: Int
                 , health :: Int
                 , cost   :: Int } deriving(Show, Eq)

instance Unit Card where
 damage u x = u { health = health u - x }
 power = cpower

data Minion = Minion { mpower :: Int
                     , mhealth :: Int
                     , mactive :: Bool } deriving(Show, Eq)

instance Unit Minion where
  damage u x = u { mhealth = mhealth u - x }
  power = mpower

data Player = Player { name        :: String
                     , hand        :: [Card]
                     , public      :: [Minion]
                     , deck        :: [Card]
                     , hero        :: Card
                     , totalMana   :: Int
                     , currentMana :: Int } deriving(Show, Eq)

data Board = Board { activePlayer   :: Player
                   , inactivePlayer :: Player } deriving(Show, Eq)

boardAction :: Board -> (Board -> Board) -> (Board, Maybe Player)
boardAction board action = let b' = action board
                           in (b', evaluateWinner b')

playCard :: Card -> Board -> Board
playCard card board = let p = activePlayer board
                          p' = p { public = minionFromCard card : public p
                              , hand = delete card (hand p)
                              , currentMana = currentMana p - cost card }
                      in board { activePlayer = p'}

minionFromCard :: Card -> Minion
minionFromCard c = Minion (cpower c) (health c) False

minionAttack :: (Unit u) => u -> u -> (u, u)
minionAttack attacker target = ( damage attacker (power target)
                               , damage target (power attacker))

attack :: Minion -> Minion -> Board -> Board
attack attacker target (Board player1 player2) = let (a, t) = minionAttack attacker target in
    Board (updatePublicCards player1 attacker (a { mactive = False })) (updatePublicCards player2 target t)
    where
      updatePublicCards player original new
                  | mhealth new > 0 = player { public = replace original new (public player) }
                  | otherwise = player { public = delete original (public player) }

attackPlayer :: Board -> Minion -> Board
attackPlayer (Board player1 player2) attacker = Board
                                                  player1 { public = replace attacker (damage attacker ((cpower . hero) player2)) (public player1)}
                                                  (removeHp player2 (power attacker))

replace :: (Eq a) => a -> a -> [a] -> [a]
replace search new = map (\x -> if x == search then new else x)

removeHp :: Player -> Int -> Player
removeHp player x = let h = hero player
                        h' = h { health = health h - x}
                    in player { hero = h' }

endTurn :: Board -> Board
endTurn board = Board ((activateMinions . refreshCurrentMana . increaseTotalMana . drawDeckCard) $ inactivePlayer board) (activePlayer board)

drawDeckCard :: Player -> Player
drawDeckCard player = let (newDeck, newHand) = tryMove (deck player) (hand player)
                          player' = player { hand = newHand , deck = newDeck }
                      in if newHand == hand player then removeHp player' 4 else player'
                      where tryMove [] dest = ([], dest)
                            tryMove (x:xs) dest = (xs, x:dest)

increaseTotalMana :: Player -> Player
increaseTotalMana player = player { totalMana = totalMana player + 1 }

refreshCurrentMana :: Player -> Player
refreshCurrentMana player = player { currentMana = totalMana player }

activateMinions :: Player -> Player
activateMinions player = player { public = map (\c -> c { mactive = True }) (public player) }

evaluateWinner :: Board -> Maybe Player
evaluateWinner b
  | health (hero p2) <= 0 = Just p1
  | health (hero p1) <= 0 = Just p2
  | otherwise = Nothing
  where p1 = activePlayer b
        p2 = inactivePlayer b

action :: Board -> (Board -> Board) -> Either Player Board
action board action =
  let newBoard = action board
      winner = evaluateWinner newBoard
  in maybe (Right newBoard) Left winner
