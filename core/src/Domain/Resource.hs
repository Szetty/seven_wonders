module Domain.Resource where

data ResourceType = Wood | Stone | Ore | Clay | Glass | Loom | Papyrus deriving (Enum, Show, Ord, Eq)
type Coin = Int

data ResourceCost = Cost ResourceType Int deriving (Show, Ord, Eq)

data ResourceProduced = Single ResourceType Int | Any [ResourceType] deriving (Show, Ord, Eq)
isSingleResourceProduced (Single _ _) = True
isSingleResourceProduced _ = False
isAnyResourceProduced (Any _) = True
isAnyResourceProduced _ = False