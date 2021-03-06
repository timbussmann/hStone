module GameSpec where

import           Control.Exception (evaluate)
import           Game
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils

main = hspec spec

spec :: Spec
spec = do
  describe "playing a minion card" $ do
    let minion = Minion "" 1 3 True
    let card = Card "minion" 5 (CreateMinion minion)
    let player1 = createPlayer { hand = [card]
                               , public = []
                               , deck = []
                               , currentMana = 10
                               , totalMana = 10 }
    let board = Board player1 createPlayer

    let rBoard = fst (boardAction board (playMinion 0))
    let rPlayer = activePlayer rBoard

    it "places the card on the active player's board" $ do
      length (public rPlayer) `shouldBe` 1
      let m = (head . public) rPlayer
      mpower m `shouldBe` mpower minion
      mhealth m `shouldBe` mhealth minion

    it "reduces the mana pool by the card's cost" $
      currentMana rPlayer `shouldBe` 10 - ccost card

    it "disables the card" $
      (mactive . head . public) rPlayer `shouldBe` False

    it "removes the card from the hand" $
      hand rPlayer `shouldBe` []

  describe "ending a turn" $ do
    let player1 = createPlayer { name = "p1"
                         , hand = []
                         , public = []
                         , deck = []
                         , currentMana = 0
                         , totalMana = 0 }
    let player2 = createPlayer { name = "p2"
                         , hand = []
                         , public = [ Minion "m1" 1 1 False, Minion "m2" 2 2 False ]
                         , deck = [Card "c1" 1 (CreateMinion (Minion "1" 1 1 False)), Card "c2" 2 (CreateMinion (Minion "2" 2 2 False))]
                         , currentMana = 2
                         , totalMana = 4 }

    let board = Board { activePlayer = player1, inactivePlayer = player2 }

    let result = endTurn board

    it "disables active player" $
      inactivePlayer result `shouldBe` player1

    it "activates inactive player" $
      name (activePlayer result) `shouldBe` name player2

    it "adds one mana to active player" $
      (totalMana . activePlayer) result `shouldBe` totalMana player2 + 1

    it "restores active player's mana" $
      (currentMana . activePlayer) result `shouldBe` (totalMana . activePlayer) result

    it "active player draws card from deck" $ do
      (head . hand . activePlayer) result `shouldBe` (head . deck) player2
      (head . deck) player2 `shouldSatisfy` flip notElem (deck $ activePlayer result)

    it "activates active player's minions" $
      public (activePlayer result) `shouldSatisfy` all mactive

  describe "when player has no more cards in deck" $ do
    let board = Board createPlayer createPlayer

    let result = endTurn board
    let result' = endTurn result
    let result'' = endTurn result'

    it "damages the player when his turn begins" $ do
      (mhealth . hero . activePlayer) result `shouldBe` (mhealth . hero . inactivePlayer) board - 4
      -- no damage on other player's turn
      (mhealth . hero . inactivePlayer) result' `shouldBe` (mhealth . hero . activePlayer) result
      -- players turn again
      (mhealth . hero . activePlayer) result'' `shouldBe` (mhealth . hero . inactivePlayer) board - 8

    it "doubles damage on every turn"
      pending -- requires an additional state therefore I keep it simple for now

    it "player receives no new cards" $ do
      (hand . activePlayer) result `shouldBe` []
      (hand . activePlayer) result'' `shouldBe` []

  describe "evaluating a winner" $ do
    describe "when both players still have healthpoints" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hero = Minion "hero" 0 1 False }
                    , inactivePlayer = createPlayer { name = "p2", hero = Minion "hero" 0 30 False }}

      let (board', winner) = boardAction board id

      it "continues the game" $
        winner `shouldBe` Nothing

    describe "when active player has no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hero = Minion "hero" 0 0 False }
                    , inactivePlayer = createPlayer { name = "p2", hero = Minion "hero" 0 30 False }}

      let (board', winner) = boardAction board id

      it "inactive player wins the game" $
        winner `shouldBe` Just (inactivePlayer board)

    describe "when inactive player has no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hero = Minion "hero" 0 1 False }
                    , inactivePlayer = createPlayer { name = "p2", hero = Minion "hero" 0 (-1) False }}

      let (board', winner) = boardAction board id

      it "active player wins the game" $
        winner `shouldBe` Just (activePlayer board)

    describe "when both players have no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hero = Minion "hero" 0 (-3) False }
                    , inactivePlayer = createPlayer { name = "p2", hero = Minion "hero" 0 (-5) False }}

      let (board', winner) = boardAction board id

      it "active player wins the game" $
        winner `shouldBe` Just (activePlayer board)

  describe "making an action" $ do
    describe "when game not finished" $ do
      let board = createBoard
      let modification b = b { activePlayer = (activePlayer b) { hero = Minion "hero" 0 42 False }}

      let (resultBoard, winner) = boardAction board modification

      it "returns the modified board" $
        resultBoard `shouldBe` modification board

      it "has no winner" $
        winner `shouldBe` Nothing

    describe "when game finished" $ do
      let board = createBoard
      let modification b = b { activePlayer = (activePlayer b){ hero = Minion "hero" 0 0 False } }

      let (resultBoard, winner) = boardAction board modification

      it "returns the winner" $
        winner `shouldBe` Just (inactivePlayer board)

playMinion :: Int -> Board -> Board
playMinion cardIndex board = handleMinionInteraction (playCard cardIndex board)
  where handleMinionInteraction (None board) = board
