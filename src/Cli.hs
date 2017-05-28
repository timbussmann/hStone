module Cli where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.List.Split
import           Game
import           Cards
import           System.IO
import           Text.Printf
import           Data.Maybe
import           Data.Char

handPrefix = 'h'
playerPublicPrefix = 'p'
enemyPublicPrefix = 'e'

start :: IO ()
start = do
  putStrLn "Welcome to hStone"
  putStrLn "enter your command. Type \"help\" to list available commands."
  --Cli.repeat (handleInput >=> (\(c, _) -> return c)) (\b -> getLine >>= (\i -> handle b i)
  --loop (\b -> getLine >>= (\i -> handle b (splitOn " " i))) Nothing
  loop (\b -> do
    input <- getLine
    let commands = splitOn " " input
    if head commands == "exit"
      then return (False, b)
      else do
        b' <- handle b commands
        checkForWinner b')
    Nothing

loop :: (Maybe Board -> IO (Bool, Maybe Board)) -> Maybe Board -> IO ()
loop action board = do
  result <- action board
  when (fst result) $ loop action (snd result)

checkForWinner :: Maybe (Board, Maybe Player) -> IO (Bool, Maybe Board)
checkForWinner x@(Just (b, Just p)) = do
  announceWinner p
  return (False, Just b)
  where
    announceWinner w = putStrLn $ printf "Player %s wins!" (name w)
checkForWinner x = return (True, maybe Nothing (Just . fst) x)

-- todo use: playCard instead of current put implementation
handleBoard :: [String] -> State Board (Maybe Player)
handleBoard ("end":_) = do
  winner <- boardAction endTurn
  board <- get
  -- need to use type BoardState = StateT Board IO. Binds state to IO and allows lifting to IO.
  -- doesn't make to use StateT x IO in game itself, as it leaks IO into the game logic.
  liftIO (putStrLn $ printf "%s's turn!" (name $ activePlayer board))
  return winner
handleBoard ("put":cardId:_) = do
  b <- get
  let cardToPlay = getCardById b cardId
  liftIO $ putStrLn $ printf "player %s plays card %s" (name (activePlayer b)) (show cardToPlay)
  boardAction (handleUserInteraction . playCard cardToPlay)
handleBoard ("attack":attackerId:_) = do
  b <- get
  let attacker = getMinionById b attackerId
  let attackAction = attack attacker b
  liftIO $ putStrLn "select target:"
  liftIO $ forM_ (fst attackAction) printTarget
  targetIndex <- liftIO $ getChar >>= \c -> return (digitToInt c)
  boardAction (\board -> snd (attack attacker board) (fst attackAction !! targetIndex))
handleBoard _ = return Nothing -- fallback

handle :: Maybe Board -> [String] -> IO (Maybe (Board, Maybe Player))
handle b ("":_) = return (b >>= \x -> Just (x, Nothing))
handle b ("help":_) = do
  putStrLn "exit = leave the game"
  putStrLn "board = shows the current board"
  putStrLn "start = start a new game"
  putStrLn "end = end the current turn"
  putStrLn "put {cardId} = puts the selected card on the public board"
  putStrLn "attack {attackerId} = attacks the target with the given attacker"
  return (b >>= \x -> Just(x, Nothing))
handle Nothing ("start":_) = do
  let board = endTurn createNewBoard
  putStrLn "Started new game!"
  return $ Just (board, Nothing)
handle Nothing _ = do
  putStrLn "No board found. Please start a new game."
  return Nothing
handle (Just board) ("board":_) = do
  printBoard board
  return $ Just (board, Nothing)
handle b (c:_) = do
  putStrLn $ printf "unknown command \"%s\". Type \"help\" to list available commands." c
  return (b >>= (\x -> Just (x, Nothing)))

printBoard :: Board -> IO ()
printBoard b = do
  let p1 = activePlayer b
  let p2 = inactivePlayer b
  putStrLn $ printf "You (%s): %d HP, %d/%d Mana" (name p1) (heroHealth (hero p1)) (currentMana p1) (totalMana p1)
  putStrLn "Hand:"
  printCards (hand p1) handPrefix
  putStrLn "Public:"
  printMinions (public p1) playerPublicPrefix
  putStrLn $ printf "Enemy (%s): %d HP, %d/%d Mana" (name p2) (heroHealth (hero p2)) (currentMana p2) (totalMana p2)
  putStrLn "Public:"
  printMinions (public p2) enemyPublicPrefix

printTarget :: Target -> IO ()
printTarget (MinionTarget minion) = putStrLn $ printf "%s %d HP %d AP" (mname minion) (mhealth minion) (mpower minion)
printTarget (HeroTarget hero) = putStrLn $ printf "Hero %d HP %d AP" (heroHealth hero) (heroPower hero)

printCards :: [Card] -> Char -> IO ()
printCards [] _ = putStrLn "-"
printCards cards prefix = foldM_
  (\i minion -> putStrLn "todo")
  ()
  cards
  --(\i (MinionCard minion) -> putStrLn (printf "[%c%d] %s %d HP %d AP" prefix i (mname minion) (mhealth minion) (mpower minion)) >>= \_ -> return (i + 1))

printMinions :: [Minion] -> Char -> IO ()
printMinions [] _ = putStrLn "-"
printMinions minions prefix = foldM_
  (\i c -> putStrLn (printf "[%c%d] %d HP %d AP" prefix i (mhealth c) (mpower c)) >>= \_ -> return (i + 1))
  (1 :: Int)
  minions

getCardById :: Board -> String -> Card
getCardById b identifier = let (prefix, index) = split identifier
                           in getCards prefix !! (index - 1)
                           where split (x:xs) = (x, read xs :: Int)
                                 getCards x
                                  | x == handPrefix = hand $ activePlayer b

getMinionById :: Board -> String -> Minion
getMinionById b identifier = let (prefix, index) = split identifier
                           in getCards prefix !! (index - 1)
                           where split (x:xs) = (x, read xs :: Int)
                                 getCards x
                                  | x == playerPublicPrefix = public $ activePlayer b
                                  | x == enemyPublicPrefix = public $ inactivePlayer b

handleUserInteraction :: UserInteraction -> Board
handleUserInteraction (None board) = board
--todo implement target spells (will require a switch to IO)

createNewBoard :: Board
createNewBoard =
  let
  {
    player1 = Player { name = "player1"
                      , hand = []
                      , public = []
                      , deck = reverse cards
                    , totalMana = 0
                    , currentMana= 0
                    , hero = Hero 0 5 };
    player2 = Player { name = "player2"
                      , hand = []
                      , public = []
                      , deck = reverse cards
                    , totalMana = 0
                    , currentMana= 0
                    , hero = Hero 0 5 };
  }
  in Board player1 player2
