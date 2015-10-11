module GameSpec where

import           Control.Exception (evaluate)
import           Game
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec =
  describe "playing a minion card" $ do
    let card = Card 1 3 5
    let player = Player { hand = [card]
                        , public = []
                        , deck = []
                        , mana = 10
                        , hp = 30 }

    let rPlayer = playCard player card

    it "places the card on the player's board" $
      public rPlayer  `shouldBe` [card]

    it "reduces the mana pool by the card's cost" $
      mana rPlayer `shouldBe` 10 - cost card

    it "disables the card"
      pending

    it "removes the card from the hand" $
      hand rPlayer `shouldBe` []
