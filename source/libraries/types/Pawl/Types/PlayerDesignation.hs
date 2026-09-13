module Pawl.Types.PlayerDesignation where

-- | A designation A PLAYER can have for the rest of the game and nothing else
-- about it: CR 702.131c's city's blessing and CR 702.195b's enduring story. Both
-- rules word the mark identically -- "a designation that has no rules meaning
-- other than to act as a marker that other rules and effects can identify", and
-- "any number of players may have [it] at the same time" -- so they are one
-- payload rather than a field, a read atom and an engine module apiece
-- (Player.designations, Quantity.HasPlayerDesignation,
-- Pawl.Engine.PlayerDesignation).
--
-- Pawl.Types.Designation is the PERMANENT's version and stays separate: every
-- mark there lasts until the permanent leaves the battlefield, and the two rules
-- here say "for the rest of the game" instead, which no permanent outlives.
--
-- NOT GameState.monarch's shape either, though the monarch is also a player's
-- designation: CR 725.3 makes it unique, so it is one seat on the game. These two
-- are held by any number of players at once, so each player carries their own set
-- -- Player.designations, where CR 702.179b's speed rides for the same reason.
data PlayerDesignation
  = -- | CR 702.131c: the city's blessing, which CR 702.131a\/b's ascend sets.
    CitysBlessing
  | -- | CR 702.195b: an enduring story, which CR 702.195a's storied sets.
    EnduringStory
  deriving (Bounded, Enum, Eq, Ord, Show)
