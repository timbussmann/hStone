module SpellSpec where

import           Game
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Cards
import           Data.List
import           Data.Maybe
import           Debug.Trace

spec :: Spec
spec = do
  describe "Divine Spirit" $ do
    let target = Minion "target minion" 1 2 True
    let spellCard = head spells
    let board = Board
                  createPlayer { public = [target], hand = [spellCard]}
                  createPlayer

    let (SelectSingleTarget targets selector) = playCard spellCard board
    let result = fst (boardAction board (\b -> selector $ head targets))

    it "should double target minions health" $
      (mhealth . head . public . activePlayer) result `shouldBe` mhealth target * 2

  describe "Shadow Word: Death" $ do
    let spellCard = fromJust $ find (\(NewCard name _ _) -> name == "Shadow Word: Death") spells
    let target4ap = Minion "4 ap" 4 10 True
    let target5ap = Minion "5 ap" 5 10 True
    let target6ap = Minion "6 ap" 6 10 True
    let board = Board
                  createPlayer { hand = [spellCard]}
                  createPlayer { public = [target4ap, target5ap, target6ap]}
    let (SelectSingleTarget targets selector) = playCard spellCard board
    
    it "should be applyable to enemy targets with more than 5 power" $ do
      length targets `shouldBe` 2
      (target5ap `elem` targets) `shouldBe` True
      (target6ap `elem` targets) `shouldBe` True
    it "should remove selected target" $ do
      let result = fst $ boardAction board (\b -> selector $ head targets)
      length (public $ inactivePlayer result) `shouldBe` 2 -- should remove targeted card
      mname (head $ public $ inactivePlayer result) `shouldBe` "4 ap"
      mname (last $ public $ inactivePlayer result) `shouldBe` "6 ap"