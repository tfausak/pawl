module Pawl.Types.BecomeCopy where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's BecomeCopy arm: CR 707.4's change of a
-- permanent already on the battlefield into a copy of another object. Unstable
-- Shapeshifter's "this creature becomes a copy of that creature" is
-- @original = InSlot became@, @subject = EachMatching IsSource@.
--
-- TWO ObjectRefs, named rather than positional for Pawl.Types.RequireBlock's
-- reason: the sides are not interchangeable, and a card file that swapped them
-- would decode into a copy pointing the wrong way -- the entrant becoming a copy
-- of the Shapeshifter instead.
--
-- Each is an ObjectRef rather than a bare SlotName, for the reason CreateCopy's
-- comment gives: `original` is a slot on every producer in the pool, while
-- `subject` is "this permanent" and so an EachMatching over IsSource. Mirrorweave's
-- "each other creature becomes a copy of target nonlegendary creature" is the
-- swept shape the same field takes, and needs only a duration besides (#1753).
--
-- NO DURATION FIELD, and that is structural rather than an omission. This opcode
-- writes the copiable values themselves (CR 707.2 / 613.1a layer 1) by stamping
-- the subject's copy snapshot, which is what CR 707.3 requires -- "objects that
-- copy the object will use the new copiable values" -- and a snapshot has nowhere
-- to record an expiry. Not implemented: a copy effect the card gives a duration
-- ("until your next turn", Crystalline Resonance), which needs a layer-1
-- continuous effect beside the stamp (#1753).
data BecomeCopy = MkBecomeCopy
  { original :: ObjectRef.ObjectRef,
    subject :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
