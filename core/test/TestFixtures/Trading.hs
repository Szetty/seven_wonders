module TestFixtures.Trading where
import TestFixtures.GameState
import Test.Hspec

import Domain.GameState
import Domain.Resource
import Domain.Player
import Core.Trading

import Data.Map(fromList)

testTrading = do
    describe "Trading.tryTrading" $ do
        context "nothing to trade" $ do
            let trade = tryTrading gameStateForTradingEmpty "b"
            it "1" $ trade []                                                         `shouldBe` []
            it "2" $ trade [[Cost Wood 1]]                                            `shouldBe` []
            it "3" $ trade [[Cost Wood 2]]                                            `shouldBe` []
            it "4" $ trade [[Cost Wood 1, Cost Loom 1]]                               `shouldBe` []
            it "5" $ trade [[Cost Wood 1], [Cost Loom 1]]                             `shouldBe` []
            it "6" $ trade [[Cost Wood 1, Cost Loom 2], [Cost Ore 3, Cost Papyrus 4]] `shouldBe` []
        context "trade without discount" $ do
            let trade = tryTrading gameStateForTrading0 "b"
            it "1" $ trade [[Cost Wood 1]]               `shouldBe` [[("c", Wood, 2)]]
            it "2" $ trade [[Cost Ore 1]]                `shouldBe` [[("a", Ore, 2)]]
            it "3" $ trade [[Cost Clay 1]]               `shouldBe` [[("a", Clay, 2)], [("c", Clay, 2)]]
            it "4" $ trade [[Cost Stone 1]]              `shouldBe` []
            it "5" $ trade [[Cost Wood 1], [Cost Ore 1]] `shouldBe` [[("c", Wood, 2)], [("a", Ore, 2)]]
            it "6" $ trade [[Cost Wood 2]]               `shouldBe` []
            it "7" $ trade [[Cost Ore 2]]                `shouldBe` []
            it "8" $ trade [[Cost Clay 2]]               `shouldBe` [[("a", Clay, 2), ("c", Clay, 2)]]
            it "9" $ trade [[Cost Wood 1, Cost Ore 1]]   `shouldBe` [[("a", Ore, 2), ("c", Wood, 2)]]
            it "10" $ trade [[Cost Clay 3]]              `shouldBe` []
        context "trade with one discount on all resources" $ do
            let trade = tryTrading gameStateForTrading1 "b"
            it "1" $ trade [[Cost Wood 1]]               `shouldBe` [[("c", Wood, 2)]]
            it "2" $ trade [[Cost Ore 1]]                `shouldBe` [[("a", Ore, 1)]]
            it "3" $ trade [[Cost Clay 1]]               `shouldBe` [[("a", Clay, 1)], [("c", Clay, 2)]]
            it "4" $ trade [[Cost Stone 1]]              `shouldBe` []
            it "5" $ trade [[Cost Wood 1], [Cost Ore 1]] `shouldBe` [[("c", Wood, 2)], [("a", Ore, 1)]]
            it "6" $ trade [[Cost Wood 2]]               `shouldBe` []
            it "7" $ trade [[Cost Ore 2]]                `shouldBe` []
            it "8" $ trade [[Cost Clay 2]]               `shouldBe` [[("a", Clay, 1), ("c", Clay, 2)]]
            it "9" $ trade [[Cost Wood 1, Cost Ore 1]]   `shouldBe` [[("a", Ore, 1), ("c", Wood, 2)]]
            it "10" $ trade [[Cost Clay 3]]              `shouldBe` []
        context "trade with both discounts on all resources" $ do
            let trade = tryTrading gameStateForTrading2 "b"
            it "1" $ trade [[Cost Wood 1]]               `shouldBe` [[("c", Wood, 1)]]
            it "2" $ trade [[Cost Ore 1]]                `shouldBe` [[("a", Ore, 1)]]
            it "3" $ trade [[Cost Clay 1]]               `shouldBe` [[("a", Clay, 1)], [("c", Clay, 1)]]
            it "4" $ trade [[Cost Stone 1]]              `shouldBe` []
            it "5" $ trade [[Cost Wood 1], [Cost Ore 1]] `shouldBe` [[("c", Wood, 1)], [("a", Ore, 1)]]
            it "6" $ trade [[Cost Wood 2]]               `shouldBe` []
            it "7" $ trade [[Cost Ore 2]]                `shouldBe` []
            it "8" $ trade [[Cost Clay 2]]               `shouldBe` [[("a", Clay, 1), ("c", Clay, 1)]]
            it "9" $ trade [[Cost Wood 1, Cost Ore 1]]   `shouldBe` [[("a", Ore, 1), ("c", Wood, 1)]]
            it "10" $ trade [[Cost Clay 3]]              `shouldBe` []
        context "trade with one discount on some resources" $ do
            let trade = tryTrading gameStateForTrading3 "b"
            it "1" $ trade [[Cost Wood 1]]               `shouldBe` [[("c", Wood, 2)]]
            it "2" $ trade [[Cost Ore 1]]                `shouldBe` [[("a", Ore, 2)]]
            it "3" $ trade [[Cost Stone 1]]              `shouldBe` []
            it "4" $ trade [[Cost Clay 1]]               `shouldBe` [[("a", Clay, 1)], [("c", Clay, 2)]]
            it "5" $ trade [[Cost Wood 1], [Cost Ore 1]] `shouldBe` [[("c", Wood, 2)], [("a", Ore, 2)]]
            it "6" $ trade [[Cost Wood 2]]               `shouldBe` []
            it "7" $ trade [[Cost Ore 2]]                `shouldBe` []
            it "8" $ trade [[Cost Clay 2]]               `shouldBe` [[("a", Clay, 1), ("c", Clay, 2)]]
            it "9" $ trade [[Cost Wood 1, Cost Ore 1]]   `shouldBe` [[("a", Ore, 2), ("c", Wood, 2)]]
            it "10" $ trade [[Cost Clay 3]]              `shouldBe` []
        context "trade without discount repeating resources" $ do
            let trade = tryTrading gameStateForTrading4 "b"
            it "1" $ trade [[Cost Clay 1]] `shouldBe` [[("a", Clay, 2)], [("c", Clay, 2)]]
            it "2" $ trade [[Cost Clay 2]] `shouldBe` [[("a", Clay, 2), ("c", Clay, 2)], [("c", Clay, 2), ("c", Clay, 2)]]
            it "3" $ trade [[Cost Clay 3]] `shouldBe` [[("a", Clay, 2), ("c", Clay, 2), ("c", Clay, 2)]]
    describe "Trading.diffResourceCosts" $ do
        it "1" $ []                        `diffResourceCosts` []            `shouldBe` []
        it "2" $ [Cost Wood 1]             `diffResourceCosts` [Cost Wood 1] `shouldBe` []
        it "3" $ [Cost Wood 1]             `diffResourceCosts` [Cost Loom 1] `shouldBe` [Cost Wood 1]
        it "4" $ [Cost Wood 1, Cost Ore 1] `diffResourceCosts` [Cost Wood 1] `shouldBe` [Cost Ore 1]
        it "5" $ [Cost Wood 1, Cost Ore 1] `diffResourceCosts` [Cost Ore 1]  `shouldBe` [Cost Wood 1]
        it "6" $ [Cost Wood 3]             `diffResourceCosts` [Cost Wood 2] `shouldBe` [Cost Wood 1]
        it "6" $ [Cost Wood 3]             `diffResourceCosts` [Cost Wood 4] `shouldBe` []

gameStateForTradingEmpty =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = []
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [defaultTradeAction]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = []
            })
        ]
    }

gameStateForTrading0 =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = [Single Ore 1, Single Clay 1]
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [defaultTradeAction]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = [Single Clay 1, Single Wood 1]
            })
        ]
    }

gameStateForTrading1 =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = [Single Ore 1, Single Clay 1]
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [
                    defaultTradeAction,
                    return tradeAction :: Action (Domain.Player.Name -> ResourceType -> Int)
                ]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = [Single Clay 1, Single Wood 1]
            })
        ]
    }
    where
        tradeAction :: Domain.Player.Name -> ResourceType -> Int
        tradeAction "a" = const 1
        tradeAction _  = const 2

gameStateForTrading2 =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = [Single Ore 1, Single Clay 1]
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [
                    defaultTradeAction,
                    return $ const $ const 1 :: Action (Domain.Player.Name -> ResourceType -> Int)
                ]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = [Single Clay 1, Single Wood 1]
            })
        ]
    }

gameStateForTrading3 =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = [Single Ore 1, Single Clay 1]
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [
                    defaultTradeAction,
                    return tradeAction :: Action (Domain.Player.Name -> ResourceType -> Int)
                ]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = [Single Clay 1, Single Wood 1]
            })
        ]
    }
    where
        tradeAction :: Domain.Player.Name -> ResourceType -> Int
        tradeAction "a" Clay = 1
        tradeAction _ _ = 2

gameStateForTrading4 =
    defaultGameState {
        neighbours = initNeighbours ["a", "b", "c"],
        playerStates = fromList [
            ("a", (defaultPlayerState "a") {
                resourcesProduced = [Single Ore 1, Single Clay 1]
            }),
            ("b", (defaultPlayerState "b") {
                tradeActions = [defaultTradeAction]
            }),
            ("c", (defaultPlayerState "c") {
                resourcesProduced = [Single Clay 2]
            })
        ]
    }