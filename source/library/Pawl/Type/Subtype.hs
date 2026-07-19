{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Subtype where

import Language.Haskell.TH.Syntax (Lift)

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
  deriving (Eq, Lift, Ord, Show)
