module Game where

import           Data.List

data Card2 = MinionCard Minion | SpellCard Spell deriving(Show, Eq)

data Minion = Minion { mname :: String
                      , mpower :: Int
                      , mhealth :: Int
                      , mcost :: Int
                      , mactive :: Bool } deriving(Show, Eq)

data Hero = Hero { heroPower :: Int
                 , heroHealth :: Int } deriving(Show, Eq)

data Spell = NonTargetSpell { spellName :: String
                            , spellCost :: Int }
           | TargetSpell { spellName :: String
                         , spellCost :: Int } deriving(Show, Eq)


data Player = Player { name        :: String
                     , hand        :: [Card2]
                     , public      :: [Minion]
                     , deck        :: [Card2]
                     , hero        :: Hero
                     , totalMana   :: Int
                     , currentMana :: Int } deriving(Show, Eq)

data Board = Board { activePlayer   :: Player
                   , inactivePlayer :: Player } deriving(Show, Eq)

boardAction :: Board -> (Board -> Board) -> (Board, Maybe Player)
boardAction board action = let b' = removeDeadMinions (action board)
                           in (b', evaluateWinner b')

removeDeadMinions :: Board -> Board
removeDeadMinions board = let ap = activePlayer board
                              iap = inactivePlayer board
                              activePlayerCards = public ap
                              inactivePlayerCards = public iap
                          in board {
                            activePlayer = ap { public = removeDead activePlayerCards },
                            inactivePlayer = iap { public = removeDead inactivePlayerCards }
                          }
                          where removeDead = filter (\m -> mhealth m <= 0)

playCard :: Card2 -> Board -> Board
playCard (c@(MinionCard minion)) board =    let p = activePlayer board
                                                p' = p { public = minion { mactive = False }  : public p
                                                , hand = delete c (hand p)
                                                , currentMana = currentMana p - mcost minion }
                                            in board { activePlayer = p'}

minionFromCard :: Card2 -> Minion
minionFromCard (MinionCard minion) = minion { mactive = False }

minionAttack :: Minion -> Minion -> (Minion, Minion)
minionAttack attacker target = ( damage attacker (mpower target)
                               , damage target (mpower attacker))

attack :: Minion -> Minion -> Board -> Board
attack attacker target (Board player1 player2) = let (a, t) = minionAttack attacker target in
    Board (updatePublicCards player1 attacker (a { mactive = False })) (updatePublicCards player2 target t)
    where
      updatePublicCards player original new = player { public = replace original new (public player) }

attackPlayer :: Board -> Minion -> Board
attackPlayer (Board player1 player2) attacker = Board
                                                  player1 { public = replace attacker (damage attacker ((heroPower . hero) player2)) (public player1)}
                                                  (removeHp player2 (mpower attacker))

replace :: (Eq a) => a -> a -> [a] -> [a]
replace search new = map (\x -> if x == search then new else x)

removeHp :: Player -> Int -> Player
removeHp player x = let h = hero player
                        h' = h { heroHealth = heroHealth h - x}
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
  | heroHealth (hero p2) <= 0 = Just p1
  | heroHealth (hero p1) <= 0 = Just p2
  | otherwise = Nothing
  where p1 = activePlayer b
        p2 = inactivePlayer b


damage target d = target { mhealth = mhealth target - d }

action :: Board -> (Board -> Board) -> Either Player Board
action board action =
  let newBoard = action board
      winner = evaluateWinner newBoard
  in maybe (Right newBoard) Left winner
