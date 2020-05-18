module Core.Trading where

import Domain.Player
import Domain.GameState
import Domain.Resource
import Core.Resources

import Data.List (sort, nub, (\\))

type TradingOption = [(Domain.Player.Name, ResourceType, Coin)]
type TradeSetup = (Domain.Player.Name, [ResourceProduced], [ResourceType -> Int])

tryTrading :: GameState -> Domain.Player.Name -> [[ResourceCost]] -> [TradingOption]
tryTrading gs@GameState {playerStates = playerStates} playerName =
    computeTradingOptions (
        (neighbourWest, resourcesProducedWest, tradeFunctionsWest),
        (neighbourEast, resourcesProducedEast, tradeFunctionsEast)
    )
    where
        tradeFunctions :: [Domain.Player.Name -> ResourceType -> Int]
        tradeFunctions = extractActions gs $ tradeActions $ lookupPlayerState gs playerName
        (neighbourWest, neighbourEast) = lookupNeighbours gs playerName
        resourcesProducedWest = resourcesProduced $ lookupPlayerState gs neighbourWest
        resourcesProducedEast = resourcesProduced $ lookupPlayerState gs neighbourEast
        tradeFunctionsWest = (\tradeFunction -> tradeFunction neighbourWest) <$> tradeFunctions
        tradeFunctionsEast = (\tradeFunction -> tradeFunction neighbourEast) <$> tradeFunctions


computeTradingOptions :: (TradeSetup, TradeSetup) -> [[ResourceCost]] -> [TradingOption]
computeTradingOptions tradeSetups resourceCostCombinations =
    resourceCostCombinations >>= doComputeTradingOptions tradeSetups
    where
        doComputeTradingOptions :: (TradeSetup, TradeSetup) -> [ResourceCost] -> [TradingOption]
        doComputeTradingOptions (tradeSetupWest, tradeSetupEast) resourceCosts =
            let
                order1 = (tradeSetupWest, tradeSetupEast)
                order2 = (tradeSetupEast, tradeSetupWest)
            in
                nub $ applyTradeSetups order1 resourceCosts ++ applyTradeSetups order2 resourceCosts

applyTradeSetups :: (TradeSetup, TradeSetup) -> [ResourceCost] -> [TradingOption]
applyTradeSetups ((name1, resources1, trade1), (name2, resources2, trade2)) resourceCosts =
    case coverResourceCosts resourceCosts resources1 of
        Nothing -> 
            [computeTradeCost name1 trade1 resourceCosts]
        Just resourceCostCombinationsAfterTrade ->
            resourceCostCombinationsAfterTrade >>= applySecondTradeSetup
            where 
                applySecondTradeSetup remainingResourceCosts = 
                    let
                        currentCost = computeTradeCost name1 trade1 (resourceCosts `diffResourceCosts` remainingResourceCosts)
                    in
                        case coverResourceCosts remainingResourceCosts resources2 of
                            Nothing ->
                                [sort $ currentCost ++ computeTradeCost name2 trade2 remainingResourceCosts]
                            Just _ ->
                                []
        
computeTradeCost :: Domain.Player.Name -> [ResourceType -> Int] -> [ResourceCost] -> TradingOption
computeTradeCost name tradeFunctions resourceCosts =
    sort $ resourceCosts >>= computeResourceCost
    where
        computeResourceCost (Cost resourceType count) = replicate count (name, resourceType, tradeValue resourceType)
        tradeValue resourceType = minimum $ (\tradeFunction -> tradeFunction resourceType) <$> tradeFunctions

diffResourceCosts = diff []
    where
        diff accResourceCosts [] _ = accResourceCosts
        diff accResourceCosts resourceCosts [] = accResourceCosts ++ resourceCosts 
        diff accResourceCosts (resourceCost:resourceCosts) resourceCosts2 =
            diff (doDiff accResourceCosts resourceCost resourceCosts2) resourceCosts resourceCosts2
            where
                doDiff accResourceCosts cost1 [] = accResourceCosts ++ [cost1]
                doDiff accResourceCosts cost1@(Cost resourceType1 c1) (Cost resourceType2 c2 : resourceCosts)
                    | resourceType1 == resourceType2 && c1 <= c2 = accResourceCosts 
                    | resourceType1 == resourceType2 && c1 > c2 = accResourceCosts ++ [Cost resourceType1 (c1 - c2)]
                    | otherwise = doDiff accResourceCosts cost1 resourceCosts