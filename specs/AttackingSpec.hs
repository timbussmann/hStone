module AttackingSpec where

import Test.Hspec
import Game
import TestUtils
import Control.Exception (evaluate)
import Data.List

spec :: Spec
spec = do
  describe "attacking a minion" $ do
    let attacker = Minion "attacker" 2 6 True
    let target = Minion "target" 1 5 True
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = [target] })

    let (Board player1 player2) = attack 0 0 board

    it "reduces target's health by attacker's power" $
      (mhealth . head . public) player2 `shouldBe` mhealth target - mpower attacker

    it "reduces attacker's health by target's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - mpower target

    it "deactivates attacking minion" $
      (mactive . head . public) player1 `shouldBe` False

  describe "when attacked minion has no more health" $ do
    let attacker = Minion "attacker" 10 10 True
    let target = Minion "target" 2 2 False
    let otherCard = Minion "other" 10 10 False
    let board = Board
                  (createPlayer { public = [attacker] })
                  (createPlayer { public = [target, otherCard] })

    let (Board player1 player2) = attack 0 0 board

    it "removes attacked minion from the owner's board" $
      public player2 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
      public player2 `shouldSatisfy` elem otherCard

  describe "when attacking minion has no more health" $ do
    let attacker = Minion "attacker" 2 2 True
    let target = Minion "target" 8 8 False
    let otherCard = Minion "other" 10 10 True
    let board = Board
                    (createPlayer { public = [attacker, otherCard] })
                    (createPlayer { public = [target] })

    let (Board player1 player2) = attack 0 0 board

    it "removes attacking minion from the owner's board" $
      public player1 `shouldSatisfy` notElem target

    it "keeps other minions on the owner's board" $
     public player1 `shouldSatisfy` elem otherCard

  describe "when attacking enemy hero" $ do
    let attacker = Minion "attacker" 5 5 True
    let targetPlayer = createPlayer { hero = Minion "hero" 2 30 False }
    let board = Board
                    createPlayer { public = [attacker] }
                    targetPlayer

    let (Board player1 player2) = attackHero 0 board

    it "reduces attacked hero's health" $
      (mhealth . hero) player2 `shouldBe` (mhealth . hero) targetPlayer - mpower attacker

    it "reduces attacker's health by hero's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - (mpower . hero) targetPlayer

  describe "when attacking one of multiple equal minions" $ do
    let attacker = Minion "attacker" 1 6 True
    let targets = [ Minion "minionA" 0 5 True
                  , Minion "minionA" 0 5 True
                  , Minion "minionA" 0 5 True ]
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = targets })

    let (Board player1 player2) = attack 0 1 board

    it "attacks selected minion" $
        mhealth (public player2 !! 1) `shouldBe` 4
    it "ignores other minions" $ do
        let m0 = mhealth (public player2 !! 0)
        let m2 = mhealth (public player2 !! 2)
        (m0, m2) `shouldBe` (5, 5)

  describe "Ally targeting spells" $ do
    describe "when playing spell on own minion" $ do
      let target = Minion "target minion" 1 1 True
      let expected = Minion "buffed minion" 12 12 True
      let spellCard = Card "spell" 3 (TargetSpell (public . activePlayer) (const expected))
      let board = Board
                    createPlayer { public = [target], hand = [spellCard]}
                    createPlayer

      let interaction = playCard 0 board
      let result = selectSpellTarget interaction target

      it "removes spell from the hand" $
        (hand . activePlayer) result `shouldSatisfy` notElem spellCard

      it "removes spell's mana cost from the player's mana pool" $
        (currentMana . activePlayer) result `shouldBe` (currentMana . activePlayer) board - 3

      it "casts spell effect on target" $ do
        (public . activePlayer) result `shouldSatisfy` elem expected
        (public . activePlayer) result `shouldSatisfy` notElem target

    describe "when selecting invalid spell target" $ do
      let target = Minion "target minion" 1 1 True
      let spellCard = Card "spell" 0 (TargetSpell (public . activePlayer) id)
      let board = Board
                    createPlayer { public = [Minion "other minion" 2 2 True], hand = [spellCard]}
                    createPlayer

      let interaction = playCard 0 board
      let result = selectSpellTarget interaction target

      it "throws an exception" $
        evaluate result `shouldThrow` errorCall "Invalid spell target"

selectSpellTarget :: UserInteraction -> Minion -> Board
selectSpellTarget (SelectSingleTarget targets selector) target = selector target