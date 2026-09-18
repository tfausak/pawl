module Pawl.Types.Gift where

-- | CR 702.174a's [something]: what "Gift a [something]" promises, which CR
-- 702.174b turns into the second ability's effect.
--
-- A type of its own rather than a bare Pawl.Types.Effect on the keyword: rule
-- 702.174b writes the effect out in the rulebook from this word alone, so the
-- card prints the word and Pawl.Engine.Keyword mints the effect.
--
-- Not implemented: CR 702.174d's Food, CR 702.174f's tapped Fish, CR 702.174g's
-- extra turn, CR 702.174h's Treasure and CR 702.174i's Octopus (#3833).
data Gift
  = -- | CR 702.174e: "Gift a card" -- the chosen player draws a card.
    Card
  deriving (Bounded, Enum, Eq, Ord, Show)
