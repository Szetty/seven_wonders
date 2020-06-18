module Domain.Resource where

import Data.Map (Map, elems)

data ResourceType = Wood | Stone | Ore | Clay | Glass | Loom | Papyrus deriving (Enum, Show, Ord, Eq)
allResourceTypes = enumFrom (toEnum 0) :: [Domain.Resource.ResourceType]

data ResourceCost = Cost ResourceType Int deriving (Show, Ord, Eq)

data ResourceProduced = Single ResourceType Int | Any [ResourceType] deriving (Show, Ord, Eq)
isSingleResourceProduced (Single _ _) = True
isSingleResourceProduced _ = False
isAnyResourceProduced (Any _) = True
isAnyResourceProduced _ = False

type Coin = Int
calculateTreasuryPoints :: Int -> Int
calculateTreasuryPoints = (`div` 3)

data ScientificSymbol = Tablet | Compass | Gears deriving (Enum, Show, Ord, Eq)
calculateScientificPoints :: Map ScientificSymbol Int -> Int
calculateScientificPoints scientificSymbols = identicalPoints scientificSymbols + differentSetPoints scientificSymbols
    where
        identicalPoints scientificSymbols = sum (mapper <$> elems scientificSymbols)
        differentSetPoints scientificSymbols = ((7*) . minimum) $ elems scientificSymbols
        mapper = round . (**2) . fromIntegral :: Int -> Int