module Pawl.Types.Gift where

-- | CR 702.174a's [something]: what "Gift a [something]" promises, which CR
-- 702.174b turns into the second ability's effect.
--
-- A type of its own rather than a bare Pawl.Types.Effect on the keyword: rule
-- 702.174b writes the effect out in the rulebook from this word alone, so the
-- card prints the word and Pawl.Engine.Keyword mints the effect.
--
-- Not implemented: CR 702.174g's extra turn (#3833).
data Gift
  = -- | CR 702.174e: "Gift a card" -- the chosen player draws a card.
    Card
  | -- | CR 702.174d: "Gift a Food" -- the chosen player creates a Food token.
    Food
  | -- | CR 702.174f: "Gift a tapped Fish" -- the chosen player creates a tapped
    -- 1\/1 blue Fish creature token.
    TappedFish
  | -- | CR 702.174i: "Gift an Octopus" -- the chosen player creates an 8\/8 blue
    -- Octopus creature token.
    Octopus
  | -- | CR 702.174h: "Gift a Treasure" -- the chosen player creates a Treasure
    -- token.
    Treasure
  deriving (Bounded, Enum, Eq, Ord, Show)
