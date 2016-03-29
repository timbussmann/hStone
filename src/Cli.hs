module Cli where

import Game
import System.IO
import Control.Monad
import Text.Printf
import Data.List.Split

start = do
  putStrLn "Welcome to hStone"
  let player1 = Player { name = "player1"
                      , hand = []
                      , public = []
                      , deck = [ Card 1 1 1
                               , Card 2 2 2
                               , Card 3 3 3
                      ]
                    , totalMana = 0
                    , currentMana= 0
                    , hp = 0}
  let player2 = Player { name = "player2"
                      , hand = []
                      , public = []
                      , deck = [ Card 1 1 1
                               , Card 2 2 2
                               , Card 3 3 3
                      ]
                    , totalMana = 0
                    , currentMana= 0
                    , hp = 0}
  let board = Board player1 player2

  putStrLn "enter your command. Type \"help\" to list available commands."

  let handleInput = handle Nothing

  --Cli.repeat (handleInput >=> (\(c, _) -> return c)) (\b -> getLine >>= (\i -> handle b i)
  loop (\b -> getLine >>= (\i -> handle b (splitOn " " i))) Nothing

  putStrLn "Goodbye!"
  return ()

loop :: (Maybe Board -> IO (Bool, Maybe Board)) -> Maybe Board -> IO ()
loop action board = do
  result <- action board
  when (fst result) $ loop action (snd result)

handle :: Maybe Board -> [String] -> IO (Bool, Maybe Board)
handle b ("help":_) = do
  putStrLn "exit = leave the game"
  putStrLn "board = shows the current board"
  putStrLn "start = start a new game"
  putStrLn "end = end the current turn"
  putStrLn "attack {attackerId} {targetId} = attacks the target with the given attacker"
  return (True, b)
handle b ("exit":_) =
  return (False, b)
handle Nothing ("start":_) = do
  let board = endTurn createNewBoard
  putStrLn "Started new game!"
  return (True, Just board)
handle Nothing _ = do
  putStrLn "No board found. Please start a new game."
  return (True, Nothing)
handle (Just board) ("board":_) = do
  print board
  return (True, Just board)
handle (Just b) ("end":_) =
  return (True, Just (endTurn b))
handle (Just b) ("attack":_)=
  return (True, Just b)
handle b (c:_) = do
  putStrLn $ printf "unknown command \"%s\". Type \"help\" to list available commands." c
  return (True, b)

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
                    , hp = 0};
    player2 = Player { name = "player2"
                      , hand = []
                      , public = []
                      , deck = [ Card 1 1 1
                               , Card 2 2 2
                               , Card 3 3 3
                      ]
                    , totalMana = 0
                    , currentMana= 0
                    , hp = 0}
  }
  in Board player1 player2
