module Domain.Wonder where

import Domain.Resource

data Wonder a = Wonder {
    name :: String,
    sideA :: WonderSide a,
    sideB :: WonderSide a
}

data WonderSide a = WonderSide {
    resourceEffects :: [a],
    stages :: [WonderStage a]
}

data WonderStage a = WonderStage {
    cost :: [ResourceCost],
    effects :: [a]
}