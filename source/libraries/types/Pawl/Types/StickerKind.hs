module Pawl.Types.StickerKind where

-- | CR 123.1: the four kinds of sticker. A classification the closed half
-- cases on, CounterKind's standing; Ord is load-bearing (Set keys).
data StickerKind
  = -- | CR 123.6.
    Name
  | -- | CR 123.7.
    Ability
  | -- | CR 123.8.
    PowerToughness
  | -- | CR 123.9.
    Art
  deriving (Bounded, Enum, Eq, Ord, Show)
