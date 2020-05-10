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
    describe "Resources.coverResourceCosts" $ do
        context "one single resource produced" $ do
            it "1" $ coverResourceCosts []            []              `shouldBe` Nothing
            it "2" $ coverResourceCosts []            [Single Wood 1] `shouldBe` Nothing
            it "3" $ coverResourceCosts [Cost Wood 1] []              `shouldBe` Just [[Cost Wood 1]]
            it "4" $ coverResourceCosts [Cost Wood 1] [Single Loom 1] `shouldBe` Just [[Cost Wood 1]]
            it "5" $ coverResourceCosts [Cost Wood 1] [Single Wood 1] `shouldBe` Nothing
            it "6" $ coverResourceCosts [Cost Wood 2] [Single Wood 1] `shouldBe` Just [[Cost Wood 1]]
            it "7" $ coverResourceCosts [Cost Wood 1] [Single Wood 2] `shouldBe` Nothing
            it "8" $ coverResourceCosts [Cost Wood 2] [Single Wood 2] `shouldBe` Nothing
        context "multiple single resources produced" $ do
            it "1" $ coverResourceCosts [Cost Wood 1, Cost Loom 1] [Single Wood 1]                `shouldBe` Just [[Cost Loom 1]]
            it "2" $ coverResourceCosts [Cost Wood 1, Cost Loom 1] [Single Wood 1, Single Loom 1] `shouldBe` Nothing
            it "3" $ coverResourceCosts [Cost Wood 1, Cost Loom 3] [Single Wood 1, Single Loom 2] `shouldBe` Just [[Cost Loom 1]]
            it "4" $ coverResourceCosts [Cost Wood 1, Cost Loom 1] [Single Wood 2, Single Loom 2] `shouldBe` Nothing
            it "5" $ coverResourceCosts [Cost Loom 1, Cost Wood 1] [Single Wood 1, Single Loom 1] `shouldBe` Nothing
        context "one 'any' resource produced" $ do
            it "1" $ coverResourceCosts []            [Any [Wood, Loom]] `shouldBe` Nothing
            it "2" $ coverResourceCosts [Cost Wood 1] [Any [Wood, Loom]] `shouldBe` Nothing
            it "3" $ coverResourceCosts [Cost Wood 2] [Any [Wood, Loom]] `shouldBe` Just [[Cost Wood 1]]
            it "4" $ coverResourceCosts [Cost Loom 1] [Any [Wood, Loom]] `shouldBe` Nothing
            it "5" $ coverResourceCosts [Cost Clay 1] [Any [Wood, Loom]] `shouldBe` Just [[Cost Clay 1]]
        context "one 'any' and one single resource produced" $ do
            it "1" $ coverResourceCosts [Cost Wood 2]                [Single Clay 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Wood 1]]
            it "2" $ coverResourceCosts [Cost Clay 1]                [Single Clay 1, Any [Wood, Loom]] `shouldBe` Nothing
            it "3" $ coverResourceCosts [Cost Wood 1, Cost Clay 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` Nothing
            it "4" $ coverResourceCosts [Cost Loom 1, Cost Clay 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` Nothing
            it "5" $ coverResourceCosts [Cost Wood 1, Cost Loom 1]   [Single Clay 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Loom 1],[Cost Wood 1]]
            it "6" $ coverResourceCosts [Cost Stone 1]               [Single Clay 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Stone 1]]
            it "7" $ coverResourceCosts [Cost Wood 2]                [Single Wood 1, Any [Wood, Loom]] `shouldBe` Nothing
            it "8" $ coverResourceCosts [Cost Clay 1]                [Single Wood 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Clay 1]]
            it "9" $ coverResourceCosts [Cost Wood 1, Cost Loom 1]   [Single Wood 1, Any [Wood, Loom]] `shouldBe` Nothing
            it "10" $ coverResourceCosts [Cost Wood 1, Cost Stone 1] [Single Wood 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Stone 1]]
            it "11" $ coverResourceCosts [Cost Wood 2, Cost Loom 1]  [Single Wood 1, Any [Wood, Loom]] `shouldBe` Just [[Cost Loom 1],[Cost Wood 1]]
            it "12" $ coverResourceCosts [Cost Loom 1, Cost Wood 1]  [Single Wood 1, Any [Wood, Loom]] `shouldBe` Nothing
        context "one 'any' with 3 resources produced" $ do
            it "1" $ coverResourceCosts [Cost Wood 1] [Any [Wood, Loom, Clay]] `shouldBe` Nothing
            it "2" $ coverResourceCosts [Cost Loom 1] [Any [Wood, Loom, Clay]] `shouldBe` Nothing
            it "3" $ coverResourceCosts [Cost Clay 1] [Any [Wood, Loom, Clay]] `shouldBe` Nothing
            it "4" $ coverResourceCosts [Cost Ore 1]  [Any [Wood, Loom, Clay]] `shouldBe` Just [[Cost Ore 1]]
        context "multiple 'any' resources produced" $ do
            it "1" $ coverResourceCosts [Cost Wood 1]               [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` Nothing
            it "2" $ coverResourceCosts [Cost Wood 2]               [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` Just [[Cost Wood 1]]
            it "3" $ coverResourceCosts [Cost Wood 1, Cost Clay 1]  [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` Nothing
            it "4" $ coverResourceCosts [Cost Ore 1, Cost Clay 1]   [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` Just [[Cost Clay 1],[Cost Ore 1]]
            it "5" $ coverResourceCosts [Cost Stone 1]              [Any [Wood, Loom], Any [Clay, Ore]] `shouldBe` Just [[Cost Stone 1]]
            it "6" $ coverResourceCosts [Cost Wood 1]               [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "7" $ coverResourceCosts [Cost Wood 2]               [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "8" $ coverResourceCosts [Cost Wood 1, Cost Ore 1]   [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "9" $ coverResourceCosts [Cost Loom 1, Cost Ore 1]   [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "10" $ coverResourceCosts [Cost Wood 2, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Just [[Cost Loom 1],[Cost Wood 1]]
            it "11" $ coverResourceCosts [Cost Wood 2, Cost Ore 1]  [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Just [[Cost Ore 1],[Cost Wood 1]]
            it "12" $ coverResourceCosts [Cost Wood 1, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "13" $ coverResourceCosts [Cost Clay 1]              [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Just [[Cost Clay 1]]
            it "14" $ coverResourceCosts [Cost Wood 3]              [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Just [[Cost Wood 1]]
            it "15" $ coverResourceCosts [Cost Loom 1, Cost Wood 1] [Any [Wood, Loom], Any [Wood, Ore]] `shouldBe` Nothing
            it "16" $ coverResourceCosts [Cost Wood 1]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Nothing
            it "17" $ coverResourceCosts [Cost Loom 1]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Nothing
            it "18" $ coverResourceCosts [Cost Wood 2]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Nothing
            it "19" $ coverResourceCosts [Cost Loom 2]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Nothing
            it "20" $ coverResourceCosts [Cost Wood 1, Cost Loom 1] [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Nothing
            it "21" $ coverResourceCosts [Cost Ore 1]               [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Just [[Cost Ore 1]]
            it "22" $ coverResourceCosts [Cost Wood 3]              [Any [Wood, Loom], Any [Wood, Loom]] `shouldBe` Just [[Cost Wood 1]]


validateDeck playerNo deck = do
    (age1, age2, age3) <- deck
    it "age1" $ length age1 `shouldBe` (7 * playerNo)
    it "age2" $ length age2 `shouldBe` (7 * playerNo)
    it "age3" $ length age3 `shouldBe` (7 * playerNo)