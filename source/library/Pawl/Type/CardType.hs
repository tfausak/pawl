module Pawl.Type.CardType where

-- Grows: Instant, Sorcery, Artifact, Enchantment, Planeswalker, Battle, …
data CardType
  = Land
  | Creature
  deriving (Eq, Ord, Show)
