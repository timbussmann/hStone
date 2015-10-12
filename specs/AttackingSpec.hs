module AttackingSpec where

import Test.Hspec
import Game

spec :: Spec
spec = do
  describe "attacking with a minion" $ do
    let attacker = Card { power=2, health=6, cost=0 }
    let target = Card { power=1, health=5, cost=0 }
    let board = Board
                  (createPlayer [attacker])
                  (createPlayer [target])

    let (Board player1 player2) = attack board attacker target

    it "reduces target's health by attacker's power" $
      (health . head . public) player2 `shouldBe` health target - power attacker

    it "reduces attacker's health by target's power" $
      (health . head . public) player1 `shouldBe` health attacker - power target

  describe "when attacked minion has no more health" $ do
    let attacker = Card { power=10, health=10, cost=0 }
    let target = Card { power=2, health=2, cost=0 }
    let otherCard = Card { power=10, health=10, cost=0 }
    let board = Board
                  (createPlayer [attacker])
                  (createPlayer [target, otherCard])

    let (Board player1 player2) = attack board attacker target

    it "removes attacked minion from the owner's board" $
      public player2 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
      public player2 `shouldSatisfy` elem otherCard

  describe "when attacking minion has no more health" $ do
    let attacker = Card { power=2, health=2, cost=0 }
    let target = Card { power=8, health=8, cost=0 }
    let otherCard = Card { power=10, health=10, cost=0 }
    let board = Board
                    (createPlayer [attacker, otherCard])
                    (createPlayer [target])

    let (Board player1 player2) = attack board attacker target

    it "removes attacking minion from the owner's board" $
      public player1 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
     public player1 `shouldSatisfy` elem otherCard

  describe "when attacking enemy player" $ do
    let attacker = Card { power=5, health=5, cost=0 }
    let targetPlayer = createPlayer []
    let board = Board
                    targetPlayer
                    (createPlayer [])

    let (Board player1 player2) = attackPlayer board attacker targetPlayer

    it "reduces attacked players health" $
      hp player2 `shouldBe` hp targetPlayer - power attacker


createPlayer :: [Card] -> Player
createPlayer publicCards = Player { name = "player"
                                  , hand = []
                                  , public = publicCards
                                  , deck = []
                                  , mana = 0
                                  , hp = 30 }
