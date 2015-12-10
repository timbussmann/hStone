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
    let card = Card 1 3 5
    let player = Player { name = ""
                        , hand = [card]
                        , public = []
                        , deck = []
                        , currentMana = 10
                        , totalMana = 10
                        , hp = 30 }

    let rPlayer = playCard player card

    it "places the card on the player's board" $
      public rPlayer `shouldBe` [card]

    it "reduces the mana pool by the card's cost" $
      currentMana rPlayer `shouldBe` 10 - cost card

    it "disables the card"
      pending

    it "removes the card from the hand" $
      hand rPlayer `shouldBe` []

  describe "ending a turn" $ do
    let player1 = Player { name = "p1"
                         , hand = []
                         , public = []
                         , deck = []
                         , currentMana = 0
                         , totalMana = 0
                         , hp = 0 }
    let player2 = Player { name = "p2"
                         , hand = []
                         , public = []
                         , deck = [ Card 1 1 1, Card 2 2 2 ]
                         , currentMana = 2
                         , totalMana = 4
                         , hp = 0 }

    let board = Board { activePlayer = player1, inactivePlayer = player2 }

    let result = endTurn board

    it "disables active player" $
      inactivePlayer result `shouldBe` player1

    it "activates inactive player" $
      activePlayer result `shouldBe` player2

    it "adds one mana to active player" $
      (totalMana . activePlayer) result `shouldBe` totalMana player2 + 1

    it "restores active player's mana" $
      (currentMana . activePlayer) result `shouldBe` (totalMana . activePlayer) result

    describe "active player draws card from deck" $ do
      it "adds top deck card into players hand" $
        (head . hand . activePlayer) result `shouldBe` (head . deck) player2

      it "removes top deck card from the deck" $
        (head . deck) player2 `shouldSatisfy` flip notElem (deck $ activePlayer result)

  describe "evaluating a winner" $ do
    describe "when both players still have healthpoints" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hp = 1}
                    , inactivePlayer = createPlayer { name = "p2", hp = 30 }}

      let result = evaluateWinner board

      it "continues the game" $
        result `shouldBe` Nothing

    describe "when active player has no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hp = 0}
                    , inactivePlayer = createPlayer { name = "p2", hp = 30 }}

      let result = evaluateWinner board

      it "inactive player wins the game" $
        result `shouldBe` Just (inactivePlayer board)

    describe "when inactive player has no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hp = 1}
                    , inactivePlayer = createPlayer { name = "p2", hp = -1 }}

      let result = evaluateWinner board

      it "active player wins the game" $
        result `shouldBe` Just (activePlayer board)

    describe "when both players have no more health" $ do
      let board = Board
                    { activePlayer = createPlayer { name = "p1", hp = -3}
                    , inactivePlayer = createPlayer { name = "p2", hp = -5 }}

      let result = evaluateWinner board

      it "active player wins the game" $
        result `shouldBe` Just (activePlayer board)


--TODO: use states to generate unique players, cards, etc
--TODO: gets damage when no cards left
-- note: sepc tests: how detailed, how many edge cases do you mind? e.g. draw card from deck: do you test that it doesn't draw all, that it keeps the existing hand card, and so on?
