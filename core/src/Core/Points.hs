module Core.Points where

import Domain.GameState
import Domain.Player
import Domain.Structure

import Data.Map(Map, elems, fromList, map, adjust)

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
    where
        calculateTreasuryPoints = (`div` 3)
        calculateScientificPoints :: Map ScientificSymbol Int -> Int
        calculateScientificPoints scientificSymbols = identicalPoints scientificSymbols + differentSetPoints scientificSymbols
            where
                identicalPoints scientificSymbols = sum (mapper <$> elems scientificSymbols)
                differentSetPoints scientificSymbols = ((7*) . minimum) $ elems scientificSymbols
                mapper = round . (**2) . fromIntegral :: Int -> Int

initPointMap :: Map PointCategory Int
initPointMap = 
    fromList [(MilitaryP, 0), (TreasuryP, 0), (WonderP, 0), (CivilianP, 0), (ScientificP, 0), (CommercialP, 0), (GuildsP, 0)]

