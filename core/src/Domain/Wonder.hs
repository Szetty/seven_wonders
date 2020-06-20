module Domain.Wonder where

import Domain.Resource

data Wonder a = Wonder {
    name :: String,
    sideA :: WonderSide a,
    sideB :: WonderSide a
}

data WonderSide a = WonderSide {
    name :: String,
    resourceEffects :: [a],
    stages :: [WonderStage a]
}

instance Show (WonderSide a) where
    show WonderSide {name = name} = show name

data WonderStage a = WonderStage {
    cost :: [ResourceCost],
    effects :: [a]
}