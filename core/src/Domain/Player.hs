module Domain.Player where

data Player = Player {
    name :: Name,
    sth :: Int
}

type Name = String