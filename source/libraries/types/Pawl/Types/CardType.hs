module Pawl.Types.CardType where

-- | CR 205.2a
data CardType
  = -- | CR 301
    Artifact
  | -- | CR 310
    Battle
  | -- | CR 315
    Conspiracy
  | -- | CR 302
    Creature
  | -- | CR 309
    Dungeon
  | -- | CR 303
    Enchantment
  | -- | CR 304
    Instant
  | -- | CR 308
    Kindred
  | -- | CR 305
    Land
  | -- | CR 312
    Phenomenon
  | -- | CR 311
    Plane
  | -- | CR 306
    Planeswalker
  | -- | CR 314
    Scheme
  | -- | CR 307
    Sorcery
  | -- | CR 313
    Vanguard
  deriving (Eq, Ord, Show)
