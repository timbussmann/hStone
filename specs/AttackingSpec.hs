module AttackingSpec where

import Test.Hspec
import Game
import TestUtils

spec :: Spec
spec = do
  describe "attacking with a minion" $ do
    let attacker = Minion "attacker" 2 6 0 True
    let target = Minion "target" 1 5 0 True
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "reduces target's health by attacker's power" $
      (mhealth . head . public) player2 `shouldBe` mhealth target - mpower attacker

    it "reduces attacker's health by target's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - mpower target

    it "deactivates attacking minion" $
      (mactive . head . public) player1 `shouldBe` False

  describe "when attacked minion has no more health" $ do
    let attacker = Minion "attacker" 10 10 0 True
    let target = Minion "target" 2 2 0 False
    let otherCard = Minion "other" 10 10 0 False
    let board = Board
                  (createPlayer { public = [attacker] })
                  (createPlayer { public = [target, otherCard] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacked minion from the owner's board" $
      public player2 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
      public player2 `shouldSatisfy` elem otherCard

  describe "when attacking minion has no more health" $ do
    let attacker = Minion "attacker" 2 2 0 True
    let target = Minion "target" 8 8 0 False
    let otherCard = Minion "other" 10 10 0 True
    let board = Board
                    (createPlayer { public = [attacker, otherCard] })
                    (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacking minion from the owner's board" $
      public player1 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
     public player1 `shouldSatisfy` elem otherCard

  describe "when attacking enemy hero" $ do
    let attacker = Minion "attacker" 5 5 0 True
    let targetPlayer = createPlayer { hero = Hero 2 30 }
    let board = Board
                    createPlayer { public = [attacker] }
                    targetPlayer

    let (Board player1 player2) = attackPlayer board attacker

    it "reduces attacked hero's health" $
      (heroHealth . hero) player2 `shouldBe` (heroHealth . hero) targetPlayer - mpower attacker

    it "reduces attacker's health by hero's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - (heroPower . hero) targetPlayer
