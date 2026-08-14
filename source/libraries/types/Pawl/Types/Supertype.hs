module Pawl.Types.Supertype where

-- | CR 205.4a
data Supertype
  = -- | CR 205.4c
    Basic
  | -- | CR 205.4d, CR 205.4e
    Legendary
  | -- | CR 205.4h
    Ongoing
  | -- | CR 205.4g
    Snow
  | -- | CR 205.4f
    World
  deriving (Bounded, Enum, Eq, Ord, Show)
