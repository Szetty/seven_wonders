module Core.Points where

import Domain.GameState
import Domain.Player
import Domain.Structure
import Domain.Resource

import Data.Map(Map, fromList, map, adjust)

calculatePoints :: GameState -> Map Domain.Player.Name (Map PointCategory Int)
calculatePoints gameState@GameState{playerStates = playerStates} =
    Data.Map.map (calculatePointsForPlayer gameState) playerStates

calculatePointsForPlayer :: GameState -> PlayerState -> Map PointCategory Int
calculatePointsForPlayer gameState PlayerState{
    coins = coins
,   battleTokens = battleTokens
,   pointActions = pointActions
,   scientificSymbols = scientificSymbols
,   scientificActions = scientificActions
} = (adjust (const $ calculateTreasuryPoints coins) TreasuryP
    . adjust (const $ sum battleTokens) MilitaryP 
    . combineFunctionActions gameState pointActions
    . adjust (const $ calculateScientificPoints $ combineFunctionActions gameState scientificActions scientificSymbols) ScientificP)
    initPointMap

initPointMap :: Map PointCategory Int
initPointMap = 
    fromList [(MilitaryP, 0), (TreasuryP, 0), (WonderP, 0), (CivilianP, 0), (ScientificP, 0), (CommercialP, 0), (GuildsP, 0)]