module Pawl.Types.TriggerEntry where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.3b: ONE ENTRY of the ordering choice, in which each player puts the
-- triggered abilities they control on the stack in any order they choose.
-- Pawl.Types.PendingTrigger is the engine's own record of a trigger that has
-- fired and is not yet on the stack; this is the part of it the player being
-- asked is shown.
--
-- Two fields and not four, and the two it drops are the point. `controller` is
-- the player the whole prompt is addressed to, so it is constant across the
-- batch. `bindings` is deliberately ABSENT: CR 603.6a fires one trigger per
-- entrant, so a Soul Warden watching two creatures enter contributes two entries
-- differing only in which creature each remembers -- and those really are
-- interchangeable, since either order gains the same life. Carrying the bindings
-- would make them distinct on the wire and raise a question with no answer. Such
-- a batch is elided entirely, by Pawl.Engine.Engine.orderInert -- which is what
-- makes the absence of `bindings` here safe rather than merely tidy: Aether
-- Flash's two entries are equal in the same way and are NOT interchangeable, and
-- it is that function, not this type, that tells them apart.
--
-- `source` is what the ability hangs on (CR 113.7), or CR 725.2's absence of one.
-- `ability` is the DISCRIMINATOR (#61): a source with two DISTINCT triggered
-- abilities keyed on one event puts two entries into a single choice -- Hero of
-- Bladehold's battle cry (CR 702.91a) beside its printed attack trigger, whose
-- order decides whether the tokens are pumped.
--
-- The ABILITY ITSELF rather than an ordinal index into the source's abilities,
-- for two reasons that both come down to what "the same ability" means:
--
--   * an index would make a card printing the same ability TWICE offer two
--     distinguishable entries, and they are not -- same condition, same payload,
--     same environment, so every permutation gives the same board. Value
--     equality answers that correctly and an ordinal cannot.
--   * there is no one list to index INTO. A trigger reaches the batch from a
--     live projection, from CR 608.2h last known information, from a printed
--     card in a graveyard, from rule 702's minting, or from the CR 603.7 delayed
--     store -- and the last is not an ability "of" the source at all.
--
-- Every inherent ability is Sourceless, so `source` alone would
-- leave the monarch's end-step draw, its crown steal and CR 702.179d's speed
-- increase identical; as values the three abilities differ.
data TriggerEntry = MkTriggerEntry
  { source :: TriggerSource.TriggerSource,
    ability :: TriggeredAbility.TriggeredAbility Card.Card
  }
  deriving (Eq, Ord, Show)
