module Pawl.Type.CardType where

-- Grows: Sorcery, Artifact, Planeswalker, Battle, …
data CardType
  = Land
  | Creature
  | Instant
  | -- CR 110.4: one of the six permanent types. Humility is the first
    -- enchantment printing (M3b).
    Enchantment
  deriving (Eq, Ord, Show)
