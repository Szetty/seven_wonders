import Test.Hspec

import Core.GameInit
import Core.Points

import Domain.GameState

import Data.Map (fromList, adjust)

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
            calculatePoints gameStateForPoints `shouldBe` fromList [
                ("a", fromList [(MilitaryP, -6), (TreasuryP, 0), (WonderP, 3), (CivilianP, 6), (ScientificP, 10), (CommercialP, 0), (GuildsP, 0)]),
                ("b", fromList [(MilitaryP, 13), (TreasuryP, 3), (WonderP, 0), (CivilianP, 0), (ScientificP, 94), (CommercialP, 0), (GuildsP, 0)]),
                ("c", fromList [(MilitaryP, 18), (TreasuryP, 1), (WonderP, 0), (CivilianP, 0), (ScientificP, 1), (CommercialP, 0), (GuildsP, 20)])
            ]


validateDeck playerNo deck = do
    (age1, age2, age3) <- deck
    it "age1" $ length age1 `shouldBe` (7 * playerNo)
    it "age2" $ length age2 `shouldBe` (7 * playerNo)
    it "age3" $ length age3 `shouldBe` (7 * playerNo)

gameStateForPoints =
    GameState{
        deck = ([], [], []),
        players = fromList [],
        playerStates = fromList [
            ("a", PlayerState {
                militarySymbols = 0,
                resourceActions = [],
                tradeActions = [],
                constructFreeAction = [],
                constructLastStructureAction = defaultConstructLastStructureAction,
                copyGuildAction = defaultCopyGuildAction,
                builtStructures = fromList [],
                coins = 0,
                battleTokens = [-1, -1, -1, -1, -1, -1],
                pointActions = [
                    return (adjust (3+) WonderP),
                    return (adjust (6+) CivilianP)
                ],
                scientificSymbols = fromList [
                    (Compass, 1),
                    (Gears, 1),
                    (Tablet, 1)
                ],
                scientificActions = []
            }),
            ("b", PlayerState {
                militarySymbols = 0,
                resourceActions = [],
                tradeActions = [],
                constructFreeAction = [],
                constructLastStructureAction = defaultConstructLastStructureAction,
                copyGuildAction = defaultCopyGuildAction,
                builtStructures = fromList [],
                coins = 9,
                battleTokens = [0, 1, -1, 3, 5, 5],
                pointActions = [],
                scientificSymbols = fromList [
                    (Compass, 4),
                    (Gears, 4),
                    (Tablet, 4)
                ],
                scientificActions = [
                    return (adjust (1+) Compass),
                    return (adjust (1+) Gears)
                ]
            }),
            ("c", PlayerState {
                militarySymbols = 0,
                resourceActions = [],
                tradeActions = [],
                constructFreeAction = [],
                constructLastStructureAction = defaultConstructLastStructureAction,
                copyGuildAction = defaultCopyGuildAction,
                builtStructures = fromList [],
                coins = 4,
                battleTokens = [1, 1, 3, 3, 5, 5],
                pointActions = [
                    return (adjust (10+) GuildsP),
                    return (adjust (10+) GuildsP)
                ],
                scientificSymbols = fromList [(Compass, 0), (Gears, 0), (Tablet, 0)],
                scientificActions = [
                    return (adjust (1+) Compass)
                ]
            })
        ],
        neighbours = initNeighbours ["a", "b", "c"],
        cardsDismissed = [],
        currentAgeCards = fromList []
    }