module TestUtils where

import           Game

createPlayer :: Player
createPlayer = Player
                { name = "player"
                , hand = []
                , public = []
                , deck = []
                , currentMana = 0
                , totalMana = 0
                , hero = Hero 0 30 };

createBoard :: Board
createBoard = Board
              { activePlayer = createPlayer
              , inactivePlayer = createPlayer }
