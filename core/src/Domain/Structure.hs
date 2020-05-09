module Domain.Structure where

import Domain.Resource

data Structure a = Structure {
    name :: Name,
    category :: Domain.Structure.Category,
    age :: Age,
    effects :: [a],
    dependency :: [Name],
    dependents :: [Name],
    cost :: (Coin, [ResourceCost]),
    playerThresholds :: [Threshold]
}

instance Show (Structure a) where
    show Structure {name = name} = show name

type Name = String
-- MG = Manufactured Good, RM = Raw Material
data Category = Civilian | Commercial | Guild | MG | Military | RM | Scientific deriving (Enum, Show, Ord, Eq)
data Age = I | II | III deriving (Enum, Show)
type Threshold = Int

isGuildStructure Structure {category = Guild} = True
isGuildStructure _ = False