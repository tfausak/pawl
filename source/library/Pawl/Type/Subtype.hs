module Pawl.Type.Subtype where

-- Grows: other land types, other creature types, …
data Subtype
  = Mountain
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  deriving (Eq, Ord, Show)
