import Test.Hspec
import TestFixtures.Points

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
    describe "Resources.satisfyResources" $ do
        context "one single resource produced" $ do
            it "1" $ satisfyResources []            []              `shouldBe` True
            it "2" $ satisfyResources []            [Single Wood 1] `shouldBe` True
            it "3" $ satisfyResources [Cost Wood 1] []              `shouldBe` False
            it "4" $ satisfyResources [Cost Wood 1] [Single Loom 1] `shouldBe` False
            it "5" $ satisfyResources [Cost Wood 1] [Single Wood 1] `shouldBe` True
            it "6" $ satisfyResources [Cost Wood 2] [Single Wood 1] `shouldBe` False
            it "7" $ satisfyResources [Cost Wood 1] [Single Wood 2] `shouldBe` True
            it "8" $ satisfyResources [Cost Wood 2] [Single Wood 2] `shouldBe` True
        context "multiple single resources produced" $ do
            it "1" $ satisfyResources [Cost Wood 1, Cost Loom 1] [Single Wood 1]                `shouldBe` False
            it "2" $ satisfyResources [Cost Wood 1, Cost Loom 1] [Single Wood 1, Single Loom 1] `shouldBe` True
            it "3" $ satisfyResources [Cost Wood 1, Cost Loom 3] [Single Wood 1, Single Loom 2] `shouldBe` False
            it "4" $ satisfyResources [Cost Wood 1, Cost Loom 1] [Single Wood 2, Single Loom 2] `shouldBe` True
            it "5" $ satisfyResources [Cost Loom 1, Cost Wood 1] [Single Wood 1, Single Loom 1] `shouldBe` True
        context "one 'any' resource produced" $ do
            it "1" $ satisfyResources []            [Any [Wood, Loom]] `shouldBe` True
            it "2" $ satisfyResources [Cost Wood 1] [Any [Wood, Loom]] `shouldBe` True
            it "3" $ satisfyResources [Cost Wood 2] [Any [Wood, Loom]] `shouldBe` False
            it "4" $ satisfyResources [Cost Loom 1] [Any [Wood, Loom]] `shouldBe` True
            it "5" $ satisfyResources [Cost Clay 1] [Any [Wood, Loom]] `shouldBe` False
        context "one 'any' and one single resource produced" $ do
            it "1" $ satisfyResources [Cost Wood 2]                [Single Clay 1, Any [Wood, Loom]] `shouldBe` False
            it "2" $ satisfyResources [Cost Clay 1]                [Single Clay 1, Any [Wood, Loom]] `shouldBe` True
            it "3" $ satisfyResources [Cost Wood 1, Cost Clay 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` True
            it "4" $ satisfyResources [Cost Loom 1, Cost Clay 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` True
            it "5" $ satisfyResources [Cost Wood 1, Cost Loom 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` False
            it "6" $ satisfyResources [Cost Stone 1]               [Single Clay 1, Any [Wood, Loom]] `shouldBe` False
            it "7" $ satisfyResources [Cost Wood 2]                [Single Wood 1, Any [Wood, Loom]] `shouldBe` True
            it "8" $ satisfyResources [Cost Clay 1]                [Single Wood 1, Any [Wood, Loom]] `shouldBe` False
            it "9" $ satisfyResources [Cost Wood 1, Cost Loom 1]   [Single Wood 1, Any [Wood, Loom]] `shouldBe` True
            it "10" $ satisfyResources [Cost Wood 1, Cost Stone 1] [Single Wood 1, Any [Wood, Loom]] `shouldBe` False
            it "11" $ satisfyResources [Cost Wood 2, Cost Loom 1]  [Single Wood 1, Any [Wood, Loom]] `shouldBe` False
            it "12" $ satisfyResources [Cost Loom 1, Cost Wood 1]  [Single Wood 1, Any [Wood, Loom]] `shouldBe` True
        context "one 'any' with 3 resources produced" $ do
            it "1" $ satisfyResources [Cost Wood 1] [Any [Wood, Loom, Clay]] `shouldBe` True
            it "2" $ satisfyResources [Cost Loom 1] [Any [Wood, Loom, Clay]] `shouldBe` True
            it "3" $ satisfyResources [Cost Clay 1] [Any [Wood, Loom, Clay]] `shouldBe` True
            it "4" $ satisfyResources [Cost Ore 1]  [Any [Wood, Loom, Clay]] `shouldBe` False
        context "multiple 'any' resources produced" $ do
            it "1" $ satisfyResources [Cost Wood 1]               [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` True
            it "2" $ satisfyResources [Cost Wood 2]               [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` False
            it "3" $ satisfyResources [Cost Wood 1, Cost Clay 1]  [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` True
            it "4" $ satisfyResources [Cost Ore 1, Cost Clay 1]   [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` False
            it "5" $ satisfyResources [Cost Stone 1]              [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` False
            it "6" $ satisfyResources [Cost Wood 1]               [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` True
            it "7" $ satisfyResources [Cost Wood 2]               [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` True
            it "8" $ satisfyResources [Cost Wood 1, Cost Ore 1]   [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` True
            it "9" $ satisfyResources [Cost Loom 1, Cost Ore 1]   [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` True
            it "10" $ satisfyResources [Cost Wood 2, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` False
            it "11" $ satisfyResources [Cost Wood 2, Cost Ore 1]  [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` False
            it "12" $ satisfyResources [Cost Wood 1, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` True
            it "13" $ satisfyResources [Cost Clay 1]              [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` False
            it "14" $ satisfyResources [Cost Wood 3]              [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` False
            it "15" $ satisfyResources [Cost Wood 1]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` True
            it "16" $ satisfyResources [Cost Loom 1]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` True
            it "17" $ satisfyResources [Cost Wood 2]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` True
            it "18" $ satisfyResources [Cost Loom 2]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` True
            it "19" $ satisfyResources [Cost Wood 1, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` True
            it "20" $ satisfyResources [Cost Ore 1]               [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` False
            it "21" $ satisfyResources [Cost Wood 3]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` False


validateDeck playerNo deck = do
    (age1, age2, age3) <- deck
    it "age1" $ length age1 `shouldBe` (7 * playerNo)
    it "age2" $ length age2 `shouldBe` (7 * playerNo)
    it "age3" $ length age3 `shouldBe` (7 * playerNo)