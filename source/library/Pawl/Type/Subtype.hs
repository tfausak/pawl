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
  | Shapeshifter -- CR 205.3m (a creature type; Clone's printed type)
  | Lhurgoyf -- CR 205.3m (a creature type; Tarmogoyf's printed type)
  | Arcane -- CR 205.3k (a spell type; Inner Calm, Outer Strength's)
  | Barbarian -- CR 205.3m (a creature type; Barbarian Outcast's)
  deriving (Eq, Ord, Show)
