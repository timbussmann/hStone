module Cli where

import           Control.Monad
import           Control.Monad.State
import           Data.List.Split
import           Game
import           System.IO
import           Text.Printf
import           Data.Maybe

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
    b' <- handle b (splitOn " " input)
    checkForWinner b')
    Nothing


loop :: (Maybe Board -> IO (Bool, Maybe Board)) -> Maybe Board -> IO ()
loop action board = do
  result <- action board
  when (fst result) $ loop action (snd result)

checkForWinner :: (Bool, Maybe Board) -> IO (Bool, Maybe Board)
checkForWinner x@(True, Just b) =
  let winner = evaluateWinner b in
  if isJust winner
    then announceWinner winner >>= \_ -> return (False, Just b)
    else return x
  where
    announceWinner (Just w) = putStrLn $ printf "Player %s wins!" (name w)
checkForWinner x = return x

handle :: Maybe Board -> [String] -> IO (Bool, Maybe Board)
handle b ("":_) = return (True, b)
handle b ("help":_) = do
  putStrLn "exit = leave the game"
  putStrLn "board = shows the current board"
  putStrLn "start = start a new game"
  putStrLn "end = end the current turn"
  putStrLn "put {cardId} = puts the selected card on the public board"
  putStrLn "attack {attackerId} {targetId} = attacks the target with the given attacker"
  return (True, b)
handle b ("exit":_) = do
  putStrLn "Goodbye!"
  return (False, b)
handle Nothing ("start":_) = do
  let board = endTurn createNewBoard
  putStrLn "Started new game!"
  return (True, Just board)
handle Nothing _ = do
  putStrLn "No board found. Please start a new game."
  return (True, Nothing)
handle (Just board) ("board":_) = do
  printBoard board
  return (True, Just board)
handle (Just b) ("end":_) = do
  let b' = endTurn b
  putStrLn $ printf "%s's turn!" (name $ activePlayer b')
  return (True, Just b')
handle (Just b) ("put":cardId:_) = do
  let cardToPlay = getCardById b cardId
  putStrLn $ printf "player %s plays card %s" (name (activePlayer b)) (show cardToPlay)
  let b' = playCard b cardToPlay
  return (True, Just b')
handle (Just b) ("attack":attackerId:targetId:_) = do
  let attacker = getCardById b attackerId
  let target = getCardById b targetId
  let b' = attack b attacker target
  return (True, Just b')
handle b (c:_) = do
  putStrLn $ printf "unknown command \"%s\". Type \"help\" to list available commands." c
  return (True, b)

printBoard :: Board -> IO ()
printBoard b = do
  let p1 = activePlayer b
  let p2 = inactivePlayer b
  putStrLn $ printf "You (%s): %d HP, %d/%d Mana" (name p1) (health (hero p1)) (currentMana p1) (totalMana p1)
  putStrLn "Hand:"
  printCards (hand p1) handPrefix
  putStrLn "Public:"
  printCards (public p1) playerPublicPrefix
  putStrLn $ printf "Enemy (%s): %d HP, %d/%d Mana" (name p2) (health (hero p2)) (currentMana p2) (totalMana p2)
  putStrLn "Public:"
  printCards (public p2) enemyPublicPrefix

printCards :: [Card] -> Char -> IO ()
printCards [] _ = putStrLn "-"
printCards cards prefix = foldM_
  (\i c -> putStrLn (showCard c prefix i) >>= \_ -> return (i + 1))
  1
  cards

showCard :: Card -> Char -> Int -> String
showCard c prefix i = printf "[%c%d] %d HP %d AP" prefix i (health c) (power c)

getCardById :: Board -> String -> Card
getCardById b identifier = let (prefix, index) = split identifier
                           in getCards prefix !! (index - 1)
                           where split (x:xs) = (x, read xs :: Int)
                                 getCards x
                                  | x == handPrefix = hand $ activePlayer b
                                  | x == playerPublicPrefix = public $ activePlayer b
                                  | x == enemyPublicPrefix = public $ inactivePlayer b

createNewBoard :: Board
createNewBoard =
  let
  {
    player1 = Player { name = "player1"
                      , hand = []
                      , public = []
                      , deck = [ Card 1 1 1
                               , Card 2 2 2
                               , Card 3 3 3
                      ]
                    , totalMana = 0
                    , currentMana= 0
                    , hero = Card 0 5 0 };
    player2 = Player { name = "player2"
                      , hand = []
                      , public = []
                      , deck = [ Card 1 1 1
                               , Card 2 2 2
                               , Card 3 3 3
                      ]
                    , totalMana = 0
                    , currentMana= 0
                    , hero = Card 0 5 0 };
  }
  in Board player1 player2
