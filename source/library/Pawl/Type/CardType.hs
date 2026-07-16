module Pawl.Type.CardType where

-- Grows: Creature, Instant, Sorcery, …
data CardType
  = Land
  deriving (Eq, Ord, Show)
