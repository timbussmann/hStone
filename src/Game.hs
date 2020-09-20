
--TODO: use states to generate unique players, cards, etc
--TODO: taunt
--TODO: save/load game
--TODO: bug: when having same spelltarget minion multiple times (or both players) it applies to both
        -- > make cards unique
        -- > same with attacker

-- Card -> (CardInfo, Effect)
-- don't expose boardAction, expose specific operations using boardAction internally?
-- TODO: check available mana when playing cards
   
module Game where

import           Data.List

data UserInteraction = 
  None Board | 
  SelectSingleTarget [Minion] (Minion -> Board)

data Effect = 
  CreateMinion Minion | 
  TargetSpell (Board -> [Minion]) (Minion -> Minion) |
  Spell (Board -> Board) |
  NoEffect

instance Show Effect where
  show _ = "todo"
instance Eq Effect where
 x == y = False

data Card = Card { 
  cname :: String,
  ccost :: Mana, 
  ceffect :: Effect 
  } deriving(Show)
instance Eq Card where
  x == y = cname x == cname y

data Minion = Minion {  
  mname :: String,
  mpower :: Power,
  mhealth :: Health,
  mactive :: Bool 
  } deriving(Show, Eq)

data Player = Player { name        :: String
                     , hand        :: [Card]
                     , public      :: [Minion]
                     , deck        :: [Card]
                     , hero        :: Minion
                     , totalMana   :: Mana
                     , currentMana :: Mana } deriving(Show, Eq)

data Board = Board { activePlayer   :: Player
                   , inactivePlayer :: Player } deriving(Show, Eq)

type Mana = Int
type Power = Int
type Health = Int


boardAction :: Board -> (Board -> Board) -> (Board, Maybe Player)
boardAction board action = let b' = removeDeadMinions (action board)
                           in (b', evaluateWinner b')
  where
    removeDeadMinions :: Board -> Board
    removeDeadMinions board = let ap = activePlayer board
                                  iap = inactivePlayer board
                                  activePlayerCards = public ap
                                  inactivePlayerCards = public iap
                              in board {
                                activePlayer = ap { public = removeDead activePlayerCards },
                                inactivePlayer = iap { public = removeDead inactivePlayerCards }
                              }
                              where removeDead = filter (\m -> mhealth m > 0)
    evaluateWinner :: Board -> Maybe Player
    evaluateWinner b
      | mhealth (hero p2) <= 0 = Just p1
      | mhealth (hero p1) <= 0 = Just p2
      | otherwise = Nothing
      where p1 = activePlayer b
            p2 = inactivePlayer b


playCard :: Int -> Board -> UserInteraction
playCard cardIndex board = do
    let card = hand (activePlayer board) !! cardIndex
    let b = (removeFromHand card . removeSpellCost (ccost card)) board
    handleEffect b (ceffect card)
  where handleEffect board (CreateMinion minion) =  
          let p = activePlayer board
              p' = p { public = minion { mactive = False } : public p }
          in None (board { activePlayer = p' })

        handleEffect board (TargetSpell selector application) = 
          let validTargets = selector board
          in SelectSingleTarget validTargets (\target -> if target `elem` validTargets
                                                         then let target' = application target
                                                              in board { 
                                                                  activePlayer = (activePlayer board) { public = replace target target' (public . activePlayer $ board) },
                                                                  inactivePlayer = (inactivePlayer board) { public = replace target target' (public . inactivePlayer $ board)}
                                                                  }
                                                          else error "Invalid spell target")
        handleEffect board (Spell application) = None (application board)
        handleEffect board NoEffect = None board

        removeSpellCost mana board = let p = activePlayer board in board { activePlayer = p { currentMana = currentMana p - mana}}
        -- TODO should use indexing instead of card because of potential ambiguity on duplicate cards
        removeFromHand card board = let p = activePlayer board in board { activePlayer = p { hand = delete card (hand p)}}


attack :: Int -> Int -> Board -> Board
attack attackerIndex targetIndex (Board activePlayer enemyPlayer) =
    let target = public enemyPlayer !! targetIndex
        attacker = public activePlayer !! attackerIndex
        (a, t) = minionAttack attacker target
    in Board (updatePublicCards activePlayer attackerIndex (a { mactive = False})) (updatePublicCards enemyPlayer targetIndex t)

updatePublicCards :: Player -> Int -> Minion -> Player
updatePublicCards player index new = player { public = replaceAt (public player) index new  }

minionAttack :: Minion -> Minion -> (Minion, Minion)
minionAttack attacker target = ( damage attacker (mpower target)
                                , damage target (mpower attacker))
    where damage target d = target { mhealth = mhealth target - d }

attackHero :: Int -> Board -> Board
attackHero attackerIndex (Board activePlayer enemyPlayer) =
    let attacker = public activePlayer !! attackerIndex
        (a, t) = minionAttack attacker (hero enemyPlayer)
    in Board (updatePublicCards activePlayer attackerIndex (a { mactive = False})) (enemyPlayer { hero = t })


endTurn :: Board -> Board
endTurn board = Board ((activateMinions . refreshCurrentMana . increaseTotalMana . drawDeckCard) $ inactivePlayer board) (activePlayer board)
  where
    activateMinions player = player { public = map (\c -> c { mactive = True }) (public player) }
    refreshCurrentMana player = player { currentMana = totalMana player }
    increaseTotalMana player = player { totalMana = totalMana player + 1 }
    drawDeckCard player = let (newDeck, newHand) = tryMove (deck player) (hand player)
                              player' = player { hand = newHand , deck = newDeck }
                          in if newHand == hand player then removeHp player' 4 else player'
                          where tryMove [] dest = ([], dest)
                                tryMove (x:xs) dest = (xs, x:dest)


replace :: (Eq a) => a -> a -> [a] -> [a]
replace search new = map (\x -> if x == search then new else x)

replaceAt :: [a] -> Int -> a -> [a]
replaceAt original index new =
    let (before, after) = splitAt index original
    in before ++ [new] ++ tail after

removeHp :: Player -> Int -> Player
removeHp player x = let h = hero player
                        h' = h { mhealth = mhealth h - x}
                    in player { hero = h' }

