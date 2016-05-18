module AttackingSpec where

import Test.Hspec
import Game
import TestUtils

spec :: Spec
spec = do
  describe "attacking with a minion" $ do
    let attacker = Minion { mpower=2, mhealth=6, mactive=True }
    let target = Minion { mpower=1, mhealth=5, mactive=True }
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "reduces target's health by attacker's power" $
      (mhealth . head . public) player2 `shouldBe` mhealth target - power attacker

    it "reduces attacker's health by target's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - power target

    it "deactivates attacking minion" $
      (mactive . head . public) player1 `shouldBe` False

  describe "when attacked minion has no more health" $ do
    let attacker = Minion 10 10 True
    let target = Minion 2 2 False
    let otherCard = Minion 10 10 False
    let board = Board
                  (createPlayer { public = [attacker] })
                  (createPlayer { public = [target, otherCard] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacked minion from the owner's board" $
      public player2 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
      public player2 `shouldSatisfy` elem otherCard

  describe "when attacking minion has no more health" $ do
    let attacker = Minion 2 2 True
    let target = Minion 8 8 False
    let otherCard = Minion 10 10 True
    let board = Board
                    (createPlayer { public = [attacker, otherCard] })
                    (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacking minion from the owner's board" $
      public player1 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
     public player1 `shouldSatisfy` elem otherCard

  describe "when attacking enemy hero" $ do
    let attacker = Minion 5 5 True
    let targetPlayer = createPlayer { hero = Card 2 30 0 True }
    let board = Board
                    createPlayer { public = [attacker] }
                    targetPlayer

    let (Board player1 player2) = attackPlayer board attacker

    it "reduces attacked hero's health" $
      (health . hero) player2 `shouldBe` (health . hero) targetPlayer - power attacker

    it "reduces attacker's health by hero's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - (cpower . hero) targetPlayer
