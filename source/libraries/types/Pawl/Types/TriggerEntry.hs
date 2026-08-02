module Pawl.Types.TriggerEntry where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.3b: ONE ENTRY of the ordering choice -- "each player, in APNAP order,
-- puts each triggered ability they control ... on the stack in any order they
-- choose." Pawl.Types.PendingTrigger is the engine's own record of a trigger that
-- has fired and is not yet on the stack; this is the part of it the player being
-- asked is shown.
--
-- Two fields and not four, and the two it drops are the point. `controller` is
-- the player the whole prompt is addressed to, so it is constant across the
-- batch and says nothing about any one entry. `bindings` is deliberately
-- ABSENT: CR 603.6a fires one trigger per entrant, so a Soul Warden watching two
-- creatures enter contributes two entries whose only difference is which
-- creature each remembers -- and those two really are interchangeable, since
-- either order gains the same life. Carrying the bindings would make them
-- distinct on the wire and raise a question with no answer, which is the engine
-- making a player's choice from the other direction. Such a batch is still
-- ASKED about rather than elided (#590).
--
-- `source` is what the ability hangs on (CR 113.7), or CR 725.2's absence of one.
-- `ability` is the DISCRIMINATOR (#61): a source with two DISTINCT triggered
-- abilities keyed on one event puts two entries into a single choice, and Hero of
-- Bladehold is the card that does it -- CR 702.91a's battle cry alongside its
-- printed "whenever this creature attacks, create two ... tokens", both fired by
-- one CR 508.1 declaration, and their order decides whether the tokens are pumped.
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
--     store -- and the last of those is not an ability "of" the source at all.
--
-- It also covers what the payload could not say before: an interpreter sees WHAT
-- it is ordering, not merely how many. Both of CR 725.2's inherent abilities are
-- Sourceless, so `source` alone would leave the monarch's end-step draw and its
-- crown steal identical; as values the two abilities differ, so this one field
-- settles both holes at once.
data TriggerEntry = MkTriggerEntry
  { source :: TriggerSource.TriggerSource,
    ability :: TriggeredAbility.TriggeredAbility Card.Card
  }
  deriving (Eq, Show)
