module Pawl.Types.Moved where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 400.7's zone change, plus the moving object's characteristics as of the
-- move. The characteristics are stamped because CR 608.2h's last-known
-- information is gone by the time a trigger reads them.
data Moved = MkMoved
  { change :: ZoneChange.ZoneChange,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 712.21: "If a melded permanent leaves the battlefield, one permanent
    -- leaves the battlefield and two cards are put into the appropriate zone."
    -- One departure, several arrivals -- so `change` above names the departure
    -- and the FIRST arrival, and this names every arrival after it.
    --
    -- Empty for every move but a melded permanent's departure, which is what
    -- makes `change` alone the whole answer for an ordinary one; `moved` below
    -- is the constructor that says so. CR 730.3 restates the rule for a merged
    -- permanent (#874), so the field is the shared carrier rather than meld's.
    --
    -- A FIELD ON THE EVENT and not the only announcement: rule 712.21's first
    -- clause makes the DEPARTURE one event, so a "whenever a creature dies"
    -- ability must see exactly one of these, while each arrival after the
    -- leading one also gets a GameEvent.CardArrived of its own so that rule's
    -- Example fires a card-arrival ability twice. What the field itself is for
    -- is CR 712.21c -- "if an effect can find the new object that a melded
    -- permanent becomes as it leaves the battlefield, it finds both cards" --
    -- which Pawl.Engine.Event.eventBindings reads through `arrivals` and binds
    -- as one group off the single departure event.
    --
    -- A Seq and not a Set: the order is the one the cards arrived in, which is
    -- CR 712.21a's arrangement where the owner was asked for one
    -- (Pawl.Engine.Event.arrangeComponents) and the order they melded in
    -- otherwise.
    others :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)

-- | The ordinary move: one departure, one arrival, no CR 712.21 split. Every
-- construction site but Pawl.Engine.Event's zone-change funnel uses this.
moved :: ZoneChange.ZoneChange -> ProjectedCharacteristics.ProjectedCharacteristics -> Moved
moved zc pc = MkMoved {change = zc, characteristics = pc, others = Seq.empty}

-- | CR 400.7e's "the new object it became", in the plural CR 712.21c asks for:
-- every incarnation this move minted, first arrival first. A singleton for every
-- move but a melded permanent's departure.
arrivals :: Moved -> Seq.Seq ObjectId.ObjectId
arrivals m = ZoneChange.object (change m) Seq.<| others m
