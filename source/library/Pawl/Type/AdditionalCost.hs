module Pawl.Type.AdditionalCost where

-- A non-mana cost of an activated ability that names the source permanent.
-- CR 602.1a: the {T} symbol taps it; CR 701.21: Sacrifice moves it to its
-- owner's graveyard. Grows: pay life, discard, remove a counter, tap another
-- object, ....
data AdditionalCost
  = TapSelf
  | SacrificeSelf
  deriving (Eq, Ord, Show)
