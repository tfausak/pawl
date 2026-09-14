module Pawl.Types.CopyStackObject where

import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

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
-- Pawl.Types.CreateCopy's comment gives: a swept "copy each ..." needs no new
-- shape, and CR 702.40a's storm copies its own spell, which no slot names.
--
-- `targets` is CR 707.10's own clause and not an inference from the copy: a
-- copy carries the original's targets (CR 707.10) unless the effect says
-- otherwise, and Twincast, Fork and Reverberate all print the sentence
-- separately from the copy instruction.
data CopyStackObject ability = MkCopyStackObject
  { ref :: ObjectRef.ObjectRef,
    targets :: CopyTargets.CopyTargets,
    -- | How many copies of each named object: CR 702.40a's "copy it for each
    -- other spell that was cast before it this turn"; a printed "copy" is one.
    quantity :: Quantity.Quantity,
    -- | CR 707.10: who puts the copy onto the stack, and so owns and controls it
    -- -- Meletis Charlatan's "the controller of target instant or sorcery spell
    -- copies it", where the seat is the one that controls a targeted slot's
    -- object (PlayerRef.ControllerOfBound).
    --
    -- A PlayerRef and not a PlayerId, since only a resolution knows a seat, and
    -- CR 109.5's "you" -- the resolving ability's own controller -- is the value
    -- the codec elides, which is what every other producer in the pool means.
    --
    -- PLURAL where the reference names several seats: one copy per named player,
    -- which is what makes PlayerRef's roster arms mean something here. Every
    -- producer in data\/cards names exactly one, and the elided default names the
    -- resolving controller alone.
    --
    -- The copy's controller is also CR 707.10c's chooser ("that player ... may
    -- choose new targets for that copy"), which is the half a board can see:
    -- Pawl.CopySpec's "CR 707.10 the Charlatan's copy is bob's, and bob chooses
    -- its new target" proves it.
    copier :: PlayerRef.PlayerRef,
    -- | CR 707.9's "except ..." clause, empty for a copy effect that states none.
    -- The SAME list EntryRewrite.AsCopy, Pawl.Types.BecomeCopy and
    -- Pawl.Types.CreateCopy carry, applied by the same fold
    -- (Pawl.Engine.Replacement.applyCopyExceptions) into the copiable snapshot
    -- the executor stamps on the copy -- Double Major's "except it isn't
    -- legendary if the spell is legendary", which CR 707.9f reads as "any other
    -- exceptions that effect includes" of this one effect.
    --
    -- A copy of an ABILITY takes none of them, and that is not an omission:
    -- every arm of CopyException writes a characteristic, and an ability on the
    -- stack has no card behind it to write one onto (CR 113.7a) -- which is why
    -- the executor stamps an ability copy no snapshot at all either.
    exceptions :: [CopyException.CopyException ability]
  }
  deriving (Eq, Ord, Show)

-- | What a card making one copy writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1

-- | CR 707.10's own answer when the effect names nobody else: the copy is put onto
-- the stack by the resolving ability's controller. The value the codec elides.
defaultCopier :: PlayerRef.PlayerRef
defaultCopier = PlayerRef.Relative PlayerRelation.You
