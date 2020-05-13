import Test.Hspec
import TestFixtures.Points
import TestFixtures.Resources

import Core.GameInit
import Core.Points
import Core.Resources

import Domain.GameState
import Domain.Resource

import Data.Map (fromList)

main = hspec $ do
    describe "GameState.initNeighbours" $
        it "with 3 players" $ 
            initNeighbours ["a", "b", "c"] `shouldBe` fromList [("a", ("c", "b")), ("b", ("a", "c")), ("c", ("b", "a"))]
    describe "GameInit.generateDeck" $ do
        context "with 3 players generates valid deck" $ validateDeck 3 $ runIO $ generateDeck 3
        context "with 4 players generates valid deck" $ validateDeck 4 $ runIO $ generateDeck 4
        context "with 5 players generates valid deck" $ validateDeck 5 $ runIO $ generateDeck 5
        context "with 6 players generates valid deck" $ validateDeck 6 $ runIO $ generateDeck 6
        context "with 7 players generates valid deck" $ validateDeck 7 $ runIO $ generateDeck 7
    describe "Points.calculatePoints" $
        it "with 3 players" $
            calculatePoints gameStateForPoints `shouldBe` pointsExpected
    describe "Resources.coverResourceCosts" coverResourceCostsTests


validateDeck playerNo deck = do
    (age1, age2, age3) <- deck
    it "age1" $ length age1 `shouldBe` (7 * playerNo)
    it "age2" $ length age2 `shouldBe` (7 * playerNo)
    it "age3" $ length age3 `shouldBe` (7 * playerNo)