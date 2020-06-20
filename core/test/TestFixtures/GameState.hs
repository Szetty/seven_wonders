module TestFixtures.GameState where

import Domain.GameState
import Domain.Resource

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

defaultPlayerState playerName =
    PlayerState {
        playerName = playerName,
        coins = 0,
        militarySymbols = 0,
        battleTokens = [],
        scientificSymbols = fromList [(Compass, 0), (Gears, 0), (Tablet, 0)],
        builtStructures = fromList [],
        resourcesProduced = [],
        pointActions = [],
        tradeActions = [defaultTradeAction],
        constructFreeAction = False,
        constructLastStructureAction = False,
        copyGuildAction = False,
        scientificActions = [],
        wonderStagesBuilt = 0,
        wonder = toWonderSide (head wonders) 0
    }