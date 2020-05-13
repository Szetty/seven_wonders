module TestFixtures.Points where

import Domain.GameState
import Domain.Structure
import Domain.Player

import Data.Map (Map, fromList, adjust)

gameStateForPoints =
    GameState{
        deck = ([], [], []),
        players = fromList [],
        playerStates = fromList [
            ("a", PlayerState {
                militarySymbols = 0,
                resourcesProduced = [],
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
                resourcesProduced = [],
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
                resourcesProduced = [],
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

pointsExpected :: Map Domain.Player.Name (Map PointCategory Int)
pointsExpected = 
    fromList [
        ("a", fromList [(MilitaryP, -6), (TreasuryP, 0), (WonderP, 3), (CivilianP, 6), (ScientificP, 10), (CommercialP, 0), (GuildsP, 0)]),
        ("b", fromList [(MilitaryP, 13), (TreasuryP, 3), (WonderP, 0), (CivilianP, 0), (ScientificP, 94), (CommercialP, 0), (GuildsP, 0)]),
        ("c", fromList [(MilitaryP, 18), (TreasuryP, 1), (WonderP, 0), (CivilianP, 0), (ScientificP, 1), (CommercialP, 0), (GuildsP, 20)])
    ]