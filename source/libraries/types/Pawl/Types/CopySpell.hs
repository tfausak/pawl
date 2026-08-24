module Pawl.Types.CopySpell where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's CopySpell arm: CR 707.10's "put a copy of
-- it onto the stack". Twincast's "copy target instant or sorcery spell" is
-- @ref = InSlot spell@, @newTargets = True@.
--
-- An ObjectRef rather than a bare SlotName, for the reason
-- Pawl.Types.CreateCopy's comment gives: every printed producer names a target
-- slot, but the field is the general one so a swept "copy each ..." needs no new
-- shape.
--
-- `newTargets` is CR 707.10c's OWN clause and not an inference from the copy: a
-- copy carries the original's targets (CR 707.10) unless the effect says the
-- controller may change them, and Twincast, Fork and Reverberate all print the
-- sentence separately from the copy instruction. False is the value that copies
-- CR 707.10 alone.
--
-- Not implemented: CR 707.10d's "for each player or object it could target" and
-- CR 707.10e's "a copy with a new specified target", each of which chooses the
-- copy's targets rather than offering the controller a change (#2209).
data CopySpell = MkCopySpell
  { ref :: ObjectRef.ObjectRef,
    newTargets :: Bool
  }
  deriving (Eq, Ord, Show)

-- | What a card copying with the original's targets writes, and the value the
-- codec elides.
defaultNewTargets :: Bool
defaultNewTargets = False
