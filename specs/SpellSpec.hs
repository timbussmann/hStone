module SpellSpec where

import           Game
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Cards

spec :: Spec
spec =
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
