module GameSpec where

import           Control.Exception (evaluate)
import           Game
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec = do
  describe "playing a minion card" $ do
    let card = Card 1 3 5
    let player = Player { name = ""
                        , hand = [card]
                        , public = []
                        , deck = []
                        , mana = 10
                        , hp = 30 }

    let rPlayer = playCard player card

    it "places the card on the player's board" $
      public rPlayer `shouldBe` [card]

    it "reduces the mana pool by the card's cost" $
      mana rPlayer `shouldBe` 10 - cost card

    it "disables the card"
      pending

    it "removes the card from the hand" $
      hand rPlayer `shouldBe` []

  describe "ending a turn" $ do
    let player1 = Player { name = "p1"
                         , hand = []
                         , public = []
                         , deck = []
                         , mana = 0
                         , hp = 0 }
    let player2 = Player { name = "p2"
                         , hand = []
                         , public = []
                         , deck = [ Card 1 1 1, Card 2 2 2 ]
                         , mana = 2
                         , hp = 0 }

    let board = Board { activePlayer = player1, inactivePlayer = player2 }

    let result = endTurn board

    it "disables active player" $
      inactivePlayer result `shouldBe` player1

    it "activates inactive player" $
      activePlayer result `shouldBe` player2

    it "adds one mana to active player" $
      (mana . activePlayer) result `shouldBe` mana player2 + 1

    it "restores active player's mana"
      pending

    describe "active player draws card from deck" $ do
      it "adds top deck card into players hand" $
        (head . hand . activePlayer) result `shouldBe` (head . deck) player2

      it "removes top deck card from the deck" $
        (head . deck) player2 `shouldSatisfy` flip notElem (deck $ activePlayer result)

--TODO: use states to generate unique players, cards, etc
--TODO: gets damage when no cards left
-- note: sepc tests: how detailed, how many edge cases do you mind? e.g. draw card from deck: do you test that it doesn't draw all, that it keeps the existing hand card, and so on?
