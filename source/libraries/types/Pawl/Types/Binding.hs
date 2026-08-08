module Pawl.Types.Binding where

import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Recipient as Recipient

-- | CR 601.2: the cast-time choices bound to one named slot of a spell or ability
-- on the stack. A record, not a sum: a field per binding kind, and a kind absent
-- for this slot is Nothing. The record shape is what lets two binding
-- environments be combined field-by-field (Pawl.Engine.Binding.mergeBinding)
-- without a tag to case on. No slot in this pool populates two fields at once --
-- each reserved name carries exactly one kind, and a target slot carries a
-- Recipient -- so the merge is total and order-independent by construction
-- rather than by luck. Grows a field per future binding (a for-each count).
--
-- A CHOICE AN EFFECT OFFERS does not belong here: CR 608.2d has the player
-- announce it while applying the effect, so it is asked and consumed inside
-- Pawl.Engine.Resolve without ever being stored. Magical Hack's two basic land
-- types are that shape.
data Binding = MkBinding
  { -- | CR 601.2c: the chosen target for this slot; re-validated at CR 608.2b.
    target :: Maybe Recipient.Recipient,
    -- | CR 601.2b: the value chosen for a variable in the cost (X). Read by
    -- Quantity.evaluate. Nothing for a slot with no amount.
    amount :: Maybe Natural.Natural,
    -- | CR 700.2 / 601.2b: the modes chosen for a modal spell, by index. A Seq and
    -- not a Set, because CR 700.2d's "You may choose the same mode more than once"
    -- makes one index appear several times; it is kept sorted ascending by the
    -- casting path, so ordering IS printed order (CR 608.2c) and resolution reads
    -- the modes pre-sorted with a repeated mode's occurrences adjacent.
    -- Stored only under the reserved Binding.chosenModes slot. Nothing elsewhere.
    modes :: Maybe (Seq.Seq ModeIndex.ModeIndex),
    -- | CR 707.2 / 707.5: the copiable-value snapshot a permanent copies AS IT
    -- ENTERS. Stored only under Pawl.Engine.Binding.copySource; the layer fold
    -- reads it as the layer-1 seed. Nothing for a non-copy object.
    copy :: Maybe ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 111.1: EVERY object a Create minted, for a card that refers back to
    -- all of them at once -- Thatcher Revolt's "those tokens". The plural of
    -- `target` above, and a separate field rather than a plural Recipient
    -- because a Create's slot is a definition, never a target (CR 115.10a), so
    -- nothing here is subject to CR 608.2b's illegal-target check. Nothing for
    -- every other slot.
    --
    -- No slot carries BOTH this and a target. mergeBinding would keep both, and
    -- Pawl.Engine.Engine.placeOne's per-field join is where they could meet, so
    -- the guarantee is a lint rather than a type: a card reaching it would have
    -- to declare a delayed ability's target spec under a name its own Create
    -- defines, which Pawl.CardSpec rejects. Pawl.Engine.Resolve.slotGroup records
    -- which way it would fail anyway.
    --
    -- A Seq and not a Set: mint order is the order the tokens entered, and it is
    -- not an ObjectId ordering. Effect.Sacrifice acts in it. The ObjectRef
    -- readers do not depend on it -- CR 611.2c's continuous effects freeze a SET
    -- and the one-shots are CR 608.2f-simultaneous batches -- but ordering the
    -- field arbitrarily would be inventing an order the game does not have.
    objects :: Maybe (Seq.Seq ObjectId.ObjectId)
  }
  deriving (Eq, Ord, Show)

-- | The empty binding: no choice of any kind. The unit for merging.
empty :: Binding
empty = MkBinding {target = Nothing, amount = Nothing, modes = Nothing, copy = Nothing, objects = Nothing}
