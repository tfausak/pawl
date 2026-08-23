module Pawl.Types.Draw where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Draw arm (#1899).
--
-- "These players, this many" plus the slot the cards CR 121.1 put into a hand are
-- remembered under, so a later clause of the same resolution can say "it":
-- Shahrazad and Sindbad's "draw a card and reveal it. If it isn't a land card,
-- discard it." Absent for every draw nothing looks back at, which is all but one
-- card in the corpus.
--
-- A type of its own rather than a fourth field on Pawl.Types.PlayerQuantity, and
-- that record's haddock is where the rule is written: a Maybe carried for exactly
-- one of a shared record's users turns the field's absence back into how a reader
-- tells which arm it is looking at. Pawl.Types.Mill is the same spin-out.
--
-- The ids bound are the ones the CR 400.7 funnel ARRIVED at, since rule 121.1
-- puts the card into the hand and a later clause names it there -- Mill's
-- posture, and for rule 701.17c's reason restated: the card a clause names is the
-- card in the zone it moved to.
--
-- ACROSS drawers and across the individual draws CR 121.2 breaks an instruction
-- into, since the slot is one name and no reader is per-player. One card drawn
-- takes the singular binding and several take the group, which is every other
-- group binder's split.
data Draw = MkDraw
  { player :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
