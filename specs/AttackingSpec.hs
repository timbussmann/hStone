module AttackingSpec where

import Test.Hspec
import Game
import TestUtils

spec :: Spec
spec = do
  describe "attacking with a minion" $ do
    let attacker = Card { cpower=2, health=6, cost=0 }
    let target = Card { cpower=1, health=5, cost=0 }
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "reduces target's health by attacker's power" $
      (health . head . public) player2 `shouldBe` health target - power attacker

    it "reduces attacker's health by target's power" $
      (health . head . public) player1 `shouldBe` health attacker - power target

  describe "when attacked minion has no more health" $ do
    let attacker = Card { cpower=10, health=10, cost=0 }
    let target = Card { cpower=2, health=2, cost=0 }
    let otherCard = Card { cpower=10, health=10, cost=0 }
    let board = Board
                  (createPlayer { public = [attacker] })
                  (createPlayer { public = [target, otherCard] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacked minion from the owner's board" $
      public player2 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
      public player2 `shouldSatisfy` elem otherCard

  describe "when attacking minion has no more health" $ do
    let attacker = Card { cpower=2, health=2, cost=0 }
    let target = Card { cpower=8, health=8, cost=0 }
    let otherCard = Card { cpower=10, health=10, cost=0 }
    let board = Board
                    (createPlayer { public = [attacker, otherCard] })
                    (createPlayer { public = [target] })

    let (Board player1 player2) = attack attacker target board

    it "removes attacking minion from the owner's board" $
      public player1 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
     public player1 `shouldSatisfy` elem otherCard

  describe "when attacking enemy hero" $ do
    let attacker = Card { cpower=5, health=5, cost=0 }
    let targetPlayer = createPlayer { hero = Card 2 30 0 }
    let board = Board
                    createPlayer { public = [attacker] }
                    targetPlayer

    let (Board player1 player2) = attackPlayer board attacker

    it "reduces attacked hero's health" $
      (health . hero) player2 `shouldBe` (health . hero) targetPlayer - power attacker

    it "reduces attacker's health by hero's power" $
      (health . head . public) player1 `shouldBe` health attacker - (cpower . hero) targetPlayer
