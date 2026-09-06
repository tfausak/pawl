module Pawl.Types.BecomeCopy where

import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's BecomeCopy arm: an effect turning an
-- object that already exists into a copy of another one (CR 707.1), which for a
-- permanent is CR 707.4's change. Unstable Shapeshifter's "this creature becomes
-- a copy of that creature" is @original = InSlot became@,
-- @subject = EachMatching IsSource@.
--
-- NEITHER SIDE is confined to the battlefield: CR 707.1's object is a "spell,
-- permanent, or card", so Synthetic Mirror of the Fallen's subject is a card in
-- a graveyard and Pawl.CastSpec's "CR 707.2 a graveyard card that is a copy is
-- priced at the copy's mana cost" is what proves the copiable values follow it
-- there.
--
-- The copy does NOT follow the card onto the stack. CR 400.7 makes a cast card a
-- new object, which CR 611.2c's fixed set of affected objects does not reach, so
-- the spell announces and resolves its own printed text -- the reading a Clone
-- dying as Clone gets. Pawl.CastSpec's "CR 400.7 the copy cast from the graveyard
-- is a new object" proves it. Two independent roads hold it, which is why no
-- one-line mutation reddens that case: Pawl.Types.Object.newIncarnation clears
-- `bindings` on the move, and Pawl.Engine.Cast stamps the stack incarnation a
-- fresh binding map besides.
--
-- Not implemented: a SPELL on the stack becoming a copy, the third noun in CR
-- 707.1's list -- the one place CR 707.2's copiable rules text would have to
-- reach Pawl.Engine.Resolve.resolveSpellWith, which reads the printed face
-- (#3154).
--
-- TWO ObjectRefs, named rather than positional for Pawl.Types.RequireBlock's
-- reason: the sides are not interchangeable, and a card file that swapped them
-- would decode into a copy pointing the wrong way -- the entrant becoming a copy
-- of the Shapeshifter instead.
--
-- Each is an ObjectRef rather than a bare SlotName, for the reason CreateCopy's
-- comment gives: `original` is a slot on both producers in the pool, while
-- `subject` is Unstable Shapeshifter's "this permanent" and so an EachMatching
-- over IsSource, and the Mirror's targeted card and so a slot. Mirrorweave's
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
    subject :: ObjectRef.ObjectRef,
    -- | CR 707.9's "except ..." clause, empty for a copy effect that states none.
    -- The SAME list EntryRewrite.AsCopy carries, and applied by the same fold
    -- (Pawl.Engine.Replacement.applyCopyExceptions) into the snapshot this opcode
    -- stamps, which is what CR 707.9a asks for: the gained ability "becomes part
    -- of the copiable values for the copy", so a token copy of the Shapeshifter
    -- taken afterwards has it too (CR 707.2).
    exceptions :: [CopyException.CopyException]
  }
  deriving (Eq, Ord, Show)
