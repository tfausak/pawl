module Pawl.Type.Subtype where

-- Grows: other land types, other creature types, …
data Subtype
  = Mountain
  | Swamp
  | Forest
  | Island
  | Plains
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  | Cat
  | Dinosaur
  | Beast
  | Rat
  | Elephant
  | Myr
  | Skeleton
  | Wall -- CR 205.3m (a creature type)
  | Wizard -- CR 205.3m (a creature type)
  deriving (Eq, Ord, Show)
