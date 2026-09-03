module Pawl.Types.CopyTargets where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Where a copy put onto the stack by Pawl.Types.Effect's CopyStackObject arm
-- gets its targets: CR 707.10's three answers, which are three different acts
-- rather than three settings of one.
--
-- Not a Bool with a payload beside it (#2209): CR 707.10c hands the choice to a
-- player, CR 707.10d makes the effect enumerate one copy per candidate, and the
-- unmarked case asks nobody anything. Only one of the three can be true at once,
-- so the type says so.
data CopyTargets
  = -- | CR 707.10 alone: the copy keeps the decisions the original made,
    -- targets included.
    Copied
  | -- | CR 707.10c: "you may choose new targets for the copy" (Twincast).
    ChosenByController
  | -- | CR 707.10d: one copy per object this ref names that the original could
    -- target, every one of that copy's targets being that object (Zada, Hedron
    -- Grinder's "copy that spell for each other creature you control that the
    -- spell could target").
    --
    -- The ref names the card's own description of the candidates ("each other
    -- creature you control"); rule 707.10d's "could target" narrowing is the
    -- executor's and is not written here.
    --
    -- Not implemented: CR 707.10d's "each PLAYER ... it could target", which
    -- Radiate needs and an ObjectRef cannot name (#3140).
    ForEach ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)

-- | What a card copying with the original's targets writes, and the value the
-- codec elides.
defaultValue :: CopyTargets
defaultValue = Copied
