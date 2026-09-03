module Pawl.Types.CopyStackObject where

import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's CopyStackObject arm: CR 707.10's "put a copy of
-- it onto the stack". Twincast's "copy target instant or sorcery spell" is
-- @ref = InSlot spell@, @targets = ChosenByController@; Lithoform Engine's "copy target
-- activated or triggered ability you control" is the same payload over a slot
-- whose pool is Pawl.Types.Pool's Abilities.
--
-- ONE opcode for all three of CR 707.10's nouns, and not three: the rule states
-- one act, and which noun the named object is is a CLASSIFICATION the executor
-- reads off the object's Source (Pawl.Engine.Resolve's copyOnStackOf), never a
-- choice the card makes.
--
-- An ObjectRef rather than a bare SlotName, for the reason
-- Pawl.Types.CreateCopy's comment gives: every printed producer names a target
-- slot, but the field is the general one so a swept "copy each ..." needs no new
-- shape.
--
-- `targets` is CR 707.10's own clause and not an inference from the copy: a
-- copy carries the original's targets (CR 707.10) unless the effect says
-- otherwise, and Twincast, Fork and Reverberate all print the sentence
-- separately from the copy instruction.
--
-- Not implemented: CR 707.10e's "copy ... and specify a new target for the
-- copy" (gap #3141).
data CopyStackObject = MkCopyStackObject
  { ref :: ObjectRef.ObjectRef,
    targets :: CopyTargets.CopyTargets
  }
  deriving (Eq, Ord, Show)
