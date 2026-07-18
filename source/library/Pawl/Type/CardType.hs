module Pawl.Type.CardType where

-- Grows: Sorcery, Artifact, Enchantment, Planeswalker, Battle, …
data CardType
  = Land
  | Creature
  | Instant
  deriving (Eq, Ord, Show)
