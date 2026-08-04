-- | CR 709-722: the comprehensive rules' own enumeration of card layouts. One
-- constructor per layout that has actually landed; casing on this in the closed
-- half is the same act as casing on a Phase or a Keyword (design.md section 1),
-- since these are numbered sections of the rulebook rather than the identity of
-- an effect.
module Pawl.Types.Layout where

data Layout
  = -- | A card with exactly one face: every card printed without a second set
    -- of characteristics, which is the whole pool today.
    Normal
  deriving (Bounded, Enum, Eq, Ord, Show)
