module Game where

import           Data.List

data Card = MinionCard Minion | AlliedSpell AlliedTargetSpell deriving(Show, Eq)

data Minion = Minion { mname :: String
                      , mpower :: Power
                      , mhealth :: Health
                      , mcost :: Mana
                      , mactive :: Bool } deriving(Show, Eq)

data Hero = Hero { heroPower :: Power
                 , heroHealth :: Health } deriving(Show, Eq)

data Target = MinionTarget Minion | HeroTarget Hero deriving(Show, Eq)

data AlliedTargetSpell = AlliedTargetSpell  { spellName :: String
                                            , spellCost :: Mana
                                            , spellEffect :: Minion -> Minion
                                            , validTargets :: Board -> [Minion]}
instance Eq AlliedTargetSpell where
  x == y = spellName x == spellName y
instance Show AlliedTargetSpell where
  show = spellName

data Player = Player { name        :: String
                     , hand        :: [Card]
                     , public      :: [Minion]
                     , deck        :: [Card]
                     , hero        :: Hero
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

playSpell :: Board -> AlliedTargetSpell -> ([Minion], Minion -> Board)
playSpell board spell =  let targets = validTargets spell board
                             player = activePlayer board
                          in (targets, \t ->
                            if t `elem` targets
                              then board {
                                      activePlayer = player {
                                        currentMana = currentMana player - spellCost spell,
                                        public = replace t (spellEffect spell t) (public  player),
                                        hand = delete (AlliedSpell spell) (hand player)
                                      }}
                              else  error "Invalid spell target")

playMinion :: Minion -> Board -> Board
playMinion minion board = let p = activePlayer board
                              p' = p { public = minion { mactive = False }  : public p
                              , hand = delete (MinionCard minion) (hand p)
                              , currentMana = currentMana p - mcost minion }
                          in board { activePlayer = p'}

minionFromCard :: Card -> Minion
minionFromCard (MinionCard minion) = minion { mactive = False }

minionAttack :: Minion -> Minion -> (Minion, Minion)
minionAttack attacker target = ( damage attacker (mpower target)
                               , damage target (mpower attacker))

attack :: Minion -> Board -> ([Target], Target -> Board)
attack attacker (Board activePlayer enemyPlayer) =
  let heroTarget = HeroTarget (hero enemyPlayer)
      minionTargets = map MinionTarget (public enemyPlayer)
  in (heroTarget : minionTargets, targetSelected)
  where
    targetSelected (MinionTarget target) =
      if target `elem` public enemyPlayer
      then
        let (a, t) = minionAttack attacker target
        in Board (updatePublicCards activePlayer attacker (a { mactive = False})) (updatePublicCards enemyPlayer target t)
      else error "Invalid target"
    targetSelected (HeroTarget target) =
      if target == hero enemyPlayer
      then
        let (a, t) = minionHeroAttack attacker target
        in Board (updatePublicCards activePlayer attacker (a { mactive = False})) (updateHero enemyPlayer t)
      else error "Invalid target"
    updatePublicCards player original new = player { public = replace original new (public player) }
    updateHero player new = player { hero = new }

minionHeroAttack :: Minion -> Hero -> (Minion, Hero)
minionHeroAttack minion hero = ( damage minion (heroPower hero)
                               , hero { heroHealth = heroHealth hero - mpower minion})

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
