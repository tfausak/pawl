module Pawl.Types.ControllerBecomesTarget where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.StackObjectKind as StackObjectKind

-- | CR 603.2 over CR 601.2c read from the TARGETED PLAYER's side: which stack
-- objects naming CR 109.5's "you" fire the ability. Dormant Gomazoa's "whenever
-- you become the target of a spell" against Amulet of Safekeeping's "whenever you
-- become the target of a spell or ability an opponent controls".
--
-- Two fields rather than two constructors, Pawl.Types.SpellCast's shape: both
-- printings watch the same rule 601.2c announcement and differ only in how far
-- they narrow it, so a second constructor would say the same thing twice at every
-- exhaustive reader of Pawl.Types.TriggerCondition.
data ControllerBecomesTarget = MkControllerBecomesTarget
  { -- | How the TARGETING object's controller (CR 405.4, recorded on
    -- Pawl.Types.BecameTarget) stands to CR 109.5's "you" -- Amulet of
    -- Safekeeping's "an opponent controls". AnyPlayer is Dormant Gomazoa, which
    -- narrows not at all.
    --
    -- The relation the sibling condition SelfBecomesTargeted carries for rule
    -- 702.21a, read off the same field of the same event.
    relation :: PlayerRelation.PlayerRelation,
    -- | Which of CR 601.2c's two announcement roads. Nothing is "a spell or
    -- ability" -- CR 112.1's spell and CR 113.3's ability alike, which is Amulet
    -- of Safekeeping's; Just Spell is Dormant Gomazoa's "a spell".
    --
    -- Read off the event rather than off the board because the matcher has no
    -- GameState: CR 602.2b and CR 603.3d route an ability through the same rule
    -- 601.2c, so a condition that did not ask would fire Gomazoa on Ravenous
    -- Rats' targeted discard.
    --
    -- Just Ability is representable and unprinted, admitted for the reason ward's
    -- You relation is: the field is stated over
    -- Pawl.Types.StackObjectKind, not over the two printings that read it.
    kind :: Maybe StackObjectKind.StackObjectKind
  }
  deriving (Eq, Ord, Show)
