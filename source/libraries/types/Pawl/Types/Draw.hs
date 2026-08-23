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
-- The ids bound are the ones the CR 400.7 funnel ARRIVED at -- rule 121.1 puts
-- the card into the hand, rule 400.7 makes that a new object, and a later clause
-- names the card in the zone it moved to. Mill's posture.
--
-- What lets a clause find it there is CR 701.20a rather than CR 400.7j: the hand
-- is a HIDDEN zone (CR 400.2), so rule 400.7j's "public zone" allowance does not
-- reach it, and it is the printed "and reveal it" that keeps the card findable --
-- rule 701.20a leaves a revealed card revealed "for as long as necessary to
-- complete the parts of the effect that card is relevant to". A draw with no
-- reveal beside it has no business writing this slot, and no printing in the
-- corpus does.
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
