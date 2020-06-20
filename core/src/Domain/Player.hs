module Domain.Player where

newtype Player = Player {
    name :: Name
} deriving Show

type Name = String