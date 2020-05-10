module Core.Resources where

import Domain.Resource
import Data.List (permutations, nub)
import Data.Maybe (isNothing)

coverResourceCosts :: [ResourceCost] -> [ResourceProduced] -> Maybe [[ResourceCost]]
coverResourceCosts [] _ = Nothing
coverResourceCosts costs produced = do
    let singleProduced = filter isSingleResourceProduced produced
    let anyProduced = filter isAnyResourceProduced produced
    let costsRemaining = matchSingleProduced costs singleProduced []
    tryAllAnyProducedCombinations costsRemaining anyProduced
    
matchSingleProduced :: [ResourceCost] -> [ResourceProduced] -> [ResourceCost] -> [ResourceCost]
matchSingleProduced costs [] unmatchedCosts = unmatchedCosts ++ costs
matchSingleProduced [] _produced unmatchedCosts = unmatchedCosts
matchSingleProduced (c:cs) produced unmatchedCosts =
    case tryMatchCostWithSingle c produced [] of
        Left (remainingCost, remainingProduced) ->
            matchSingleProduced cs remainingProduced (unmatchedCosts ++ [remainingCost])
        Right remainingProduced ->
            matchSingleProduced cs remainingProduced unmatchedCosts

tryMatchCostWithSingle :: ResourceCost -> [ResourceProduced] -> [ResourceProduced] -> Either (ResourceCost, [ResourceProduced]) [ResourceProduced]
tryMatchCostWithSingle cost [] produced = Left (cost, produced)
tryMatchCostWithSingle cost@(Cost costType c) (produced@(Single producedType pc) : ps) producedResources
    | costType == producedType =
        if c <= pc then
            if c == pc then
                Right $ producedResources ++ ps
            else
                Right $ producedResources ++ (Single producedType (pc - c) : ps)
        else 
            tryMatchCostWithSingle (Cost costType (c - pc)) ps producedResources
    | otherwise =
        tryMatchCostWithSingle cost ps (producedResources ++ [produced])

tryAllAnyProducedCombinations :: [ResourceCost] -> [ResourceProduced] -> Maybe [[ResourceCost]]
tryAllAnyProducedCombinations [] _ = Nothing
tryAllAnyProducedCombinations costs [] = Just [costs]
tryAllAnyProducedCombinations costs produced =
    doTryAllAnyProducedCombinations combinations []
    where
        combinations = [(produced, costs) | produced <- permutations produced, costs <- permutations costs]
        doTryAllAnyProducedCombinations :: [([ResourceProduced], [ResourceCost])] -> [[ResourceCost]] -> Maybe [[ResourceCost]]
        doTryAllAnyProducedCombinations [] acc = Just $ nub acc
        doTryAllAnyProducedCombinations ((produced, costs):ps) acc =
            case matchAnyProduced costs produced of
                [] -> 
                    Nothing
                remainingCosts -> 
                    doTryAllAnyProducedCombinations ps (acc ++ [remainingCosts])

matchAnyProduced :: [ResourceCost] -> [ResourceProduced] -> [ResourceCost]
matchAnyProduced costs produced = 
    doMatchAnyProduced costs produced []
    where 
        doMatchAnyProduced costs [] unmatchedCosts = unmatchedCosts ++ costs
        doMatchAnyProduced [] _produced unmatchedCosts = unmatchedCosts
        doMatchAnyProduced (c : cs) produced unmatchedCosts =
            case tryMatchCostWithAny c produced [] of
                Left (remainingCost, remainingProduced) ->
                    doMatchAnyProduced cs remainingProduced (unmatchedCosts ++ [remainingCost])
                Right remainingProduced ->
                    doMatchAnyProduced cs remainingProduced unmatchedCosts
            

tryMatchCostWithAny :: ResourceCost -> [ResourceProduced] -> [ResourceProduced] -> Either (ResourceCost, [ResourceProduced]) [ResourceProduced]
tryMatchCostWithAny cost [] produced = Left (cost, produced)
tryMatchCostWithAny cost@(Cost costType c) (produced@(Any producedTypes) : ps) producedResources
    | costType `elem` producedTypes =
        if c == 1 then
            Right $ producedResources ++ ps
        else
            tryMatchCostWithAny (Cost costType (c - 1)) ps producedResources
    | otherwise =
        tryMatchCostWithAny cost ps (producedResources ++ [produced])