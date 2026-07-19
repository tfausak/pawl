module Pawl.Type.CardType where

-- Grows: Sorcery, Artifact, Planeswalker, Battle, …
data CardType
  = Land
  | Creature
  | Instant
  | -- CR 110.4: one of the six permanent types. Humility is the first
    -- enchantment printing (M3b).
    Enchantment
  | -- CR 301: an artifact, a permanent type. Mindslaver is the first (M3g).
    Artifact
  deriving (Eq, Ord, Show)
