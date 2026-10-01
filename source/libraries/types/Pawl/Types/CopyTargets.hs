module Pawl.Types.CopyTargets where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Where a copy put onto the stack by Pawl.Types.Effect's CopyStackObject arm
-- gets its targets: four of CR 707.10's answers, which are different acts
-- rather than settings of one.
--
-- Not a Bool with a payload beside it, see #2209: CR 707.10c hands the choice to
-- a player, CR 707.10d makes the effect enumerate one copy per candidate, CR
-- 707.10e has it name one outright, and the unmarked case asks nobody anything.
-- At most one can hold at a time, so the type says so.
data CopyTargets
  = -- | CR 707.10 alone: the copy keeps the decisions the original made,
    -- targets included.
    Copied
  | -- | CR 707.10c: "you may choose new targets for the copy" (Twincast).
    ChosenByController
  | -- | CR 707.10d: one copy per player or object these refs name that the
    -- original could target, other than what it already targets (Zada, Hedron
    -- Grinder; Radiate).
    ForEach (NonEmpty.NonEmpty ObjectRef.ObjectRef)
  | -- | CR 707.10e: ONE copy, whose every target is the one player or object
    -- this ref names (Ivy, Gleeful Spellthief).
    Stated ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)

-- | What a card copying with the original's targets writes, and the value the
-- codec elides.
defaultValue :: CopyTargets
defaultValue = Copied
