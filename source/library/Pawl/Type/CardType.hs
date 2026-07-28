module Pawl.Type.CardType where

-- CR 205.2a lists fifteen card types. The nine missing ones are Planeswalker
-- (#301), Battle (#302), Kindred (#303), and the six command-zone residents in
-- #131 -- Dungeon, Plane, Phenomenon, Vanguard, Scheme and Conspiracy.
data CardType
  = Land
  | Creature
  | Instant
  | -- CR 110.4: one of the six permanent types. Humility is the first
    -- enchantment printing (M3b).
    Enchantment
  | -- CR 301: an artifact, a permanent type. Mindslaver is the first (M3g).
    Artifact
  | -- CR 307: a sorcery, cast only at sorcery speed (not a permanent). Blaze is
    -- the first sorcery printing (M4a).
    Sorcery
  deriving (Eq, Ord, Show)
