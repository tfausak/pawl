module Pawl.Types.InitiativeTarget where

-- | Which player an Effect.TakeTheInitiative names. CR 726.1 leaves the naming
-- to the effect, so this enumerates the two ways the pool and the rulebook do
-- it.
--
-- Its own sum rather than a Pawl.Types.PlayerRef, for
-- Pawl.Types.MonarchTarget's reason: CR 726.2's ControllerOfSource has no
-- PlayerRef spelling, and PlayerRef.EachPlayer is meaningless for a designation
-- CR 726.3 gives to exactly one player at a time.
--
-- No InSlot arm, where MonarchTarget has one: Scryfall o:"takes the
-- initiative", 2026-09-03, returns only Undercity // The Initiative, whose
-- reminder face states CR 726.2's hand-off -- no printing names a targeted
-- player. Denethor, Stone Seer's "target player becomes the monarch" is what
-- would refute this. A card that did would add the arm, and
-- Pawl.Engine.Resolve's slot reader would follow MonarchTarget.InSlot's.
data InitiativeTarget
  = -- | "you take the initiative" (Aarakocra Sneak's entry): the resolving
    -- controller.
    TheController
  | -- | CR 726.2: "the controller of those creatures takes the initiative" (the
    -- combat-damage hand-off): the controller of the object bound as the
    -- ability's source.
    ControllerOfSource
  deriving (Eq, Ord, Show)
