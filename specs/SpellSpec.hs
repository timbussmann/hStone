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
    let target = Minion "target minion" 1 2 0 True
    let spellCard = head spells
    let board = Board
                  createPlayer { public = [target], hand = [AlliedSpell spellCard]}
                  createPlayer

    let result = fst (boardAction board (\b -> let (targets, select) = playSpell b spellCard
                                               in select (head targets)))

    it "should double target minions health" $
      (mhealth . head . public . activePlayer) result `shouldBe` mhealth target * 2

  describe "Shadow Word: Death" $ do
    let spellCard = fromJust $ find (\s -> spellName s == "Shadow Word: Death") spells
    let target4ap = Minion "target with 4 ap" 4 10 0 True
    let target5ap = Minion "target with 5 ap" 5 10 0 True
    let target6ap = Minion "target with 6 ap" 6 10 0 True
    let board = Board
                  createPlayer { hand = [AlliedSpell spellCard]}
                  createPlayer { public = [target4ap, target5ap, target6ap]}
    
    
    it "should be applyable to enemy targets with more than 5 power" $ do
      let targets = fst $ playSpell board spellCard
      length targets `shouldBe` 2
      (target5ap `elem` targets) `shouldBe` True
      (target6ap `elem` targets) `shouldBe` True
    it "should set targets health to 0" $ do
      let result = fst $ boardAction board (\b -> let (targets, select) = playSpell b spellCard
                                                  in select (trace "test"target6ap))
      mhealth (head $ public $ activePlayer result) `shouldBe` 10
      mhealth (last $ public $ activePlayer result) `shouldBe` 0
