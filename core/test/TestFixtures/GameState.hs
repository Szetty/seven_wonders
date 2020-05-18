module TestFixtures.GameState where

import Domain.GameState

import Data.Map(fromList)

defaultGameState =
    GameState{
        deck = ([], [], []),
        players = fromList [],
        playerStates = fromList [],
        neighbours = fromList [],
        cardsDismissed = [],
        currentAgeCards = fromList []
    }

defaultPlayerState =
    PlayerState {
        coins = 0,
        militarySymbols = 0,
        battleTokens = [],
        scientificSymbols = fromList [(Compass, 0), (Gears, 0), (Tablet, 0)],
        builtStructures = fromList [],
        resourcesProduced = [],
        pointActions = [],
        tradeActions = [defaultTradeAction],
        constructFreeAction = [],
        constructLastStructureAction = defaultConstructLastStructureAction,
        copyGuildAction = defaultCopyGuildAction,
        scientificActions = []
    }