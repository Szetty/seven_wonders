module Domain.Resource where

data ResourceType = Wood | Stone | Ore | Clay | Glass | Loom | Papyrus deriving (Enum, Show)
type Coin = Int

data ResourceCost = WoodC Int | StoneC Int | OreC Int | ClayC Int | GlassC Int | LoomC Int | PapyrusC Int deriving (Show)