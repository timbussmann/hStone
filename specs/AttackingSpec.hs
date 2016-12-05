module AttackingSpec where

import Test.Hspec
import Game
import TestUtils
import Control.Exception (evaluate)
import Data.List

spec :: Spec
spec = do
  describe "attacking with a minion" $ do
    let attacker = Minion "attacker" 2 6 0 True
    let targets = [ Minion "target1" 1 5 0 True
                  , Minion "target2" 1 5 0 True
                  , Minion "target3" 1 5 0 True ]
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = targets })

    let action = attack attacker board

    it "lists enemy minions as targets" $
      fst action `shouldSatisfy` isInfixOf (map MinionTarget targets)

    it "lists enemy hero as target" $
      fst action `shouldSatisfy` elem (HeroTarget ((hero . inactivePlayer) board))

    describe "attacking an invalid target" $
      it "throws an exception" $
        evaluate (snd action (MinionTarget (Minion "invalid target" 1 1 0 True))) `shouldThrow` errorCall "Invalid target"

  describe "attacking a minion" $ do
    let attacker = Minion "attacker" 2 6 0 True
    let target = Minion "target" 1 5 0 True
    let board = Board
                  (createPlayer  { public = [attacker] })
                  (createPlayer { public = [target] })

    let (Board player1 player2) = snd (attack attacker board) (MinionTarget target)

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

    let (Board player1 player2) = snd (attack attacker board) (MinionTarget target)

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

    let (Board player1 player2) = snd (attack attacker board) (MinionTarget target)

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

    let (Board player1 player2) = snd (attack attacker board) (HeroTarget (hero targetPlayer))

    it "reduces attacked hero's health" $
      (heroHealth . hero) player2 `shouldBe` (heroHealth . hero) targetPlayer - mpower attacker

    it "reduces attacker's health by hero's power" $
      (mhealth . head . public) player1 `shouldBe` mhealth attacker - (heroPower . hero) targetPlayer

  describe "Ally targeting spells" $ do
    describe "when playing spell on own minion" $ do
      let target = Minion "target minion" 1 1 0 True
      let expected = Minion "buffed minion" 12 12 0 True
      let spell = AlliedTargetSpell "buff" 3 (const expected) (public . activePlayer)
      let spellCard = AlliedSpell spell
      let board = Board
                    createPlayer { public = [target], hand = [spellCard]}
                    createPlayer

      let targets = playSpell board spell
      let result = snd targets target

      it "only provides own minions as targets" $ do
        fst targets `shouldSatisfy` elem target
        length (fst targets) `shouldBe` 1

      it "removes spell from the hand" $
        (hand . activePlayer) result `shouldSatisfy` notElem spellCard

      it "removes spell's mana cost from the player's mana pool" $
        (currentMana . activePlayer) result `shouldBe` (currentMana . activePlayer) board - 3

      it "casts spell effect on target" $ do
        (public . activePlayer) result `shouldSatisfy` elem expected
        (public . activePlayer) result `shouldSatisfy` notElem target

    describe "when selecting invalid spell target" $ do
      let target = Minion "target minion" 1 1 0 True
      let spell = AlliedTargetSpell "buff" 3 id (public . activePlayer)
      let spellCard = AlliedSpell spell
      let board = Board
                    createPlayer { public = [Minion "other minion" 2 2 0 True], hand = [spellCard]}
                    createPlayer

      let result = snd (playSpell board spell) target

      it "throws an exception" $
        evaluate result `shouldThrow` errorCall "Invalid spell target"
