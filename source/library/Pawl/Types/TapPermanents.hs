module Pawl.Types.TapPermanents where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's TapPermanents arm: CR 601.2f's
-- "tapping permanents" as a cost, spent by tapping exactly this many permanents
-- the payer chooses out of the ones the Filter admits. Springleaf Drum's "Tap
-- an untapped creature you control" is the printing.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- A record of its OWN rather than one shared with
-- Pawl.Types.TapForTotalPower, whose fields have the same two types, for the
-- reason Pawl.Types.Sacrifice gives against sharing with that same record:
-- count is HOW MANY objects and must be matched exactly, where that one's
-- totalPower is a THRESHOLD on an aggregate and determines no count at all.
--
-- Pawl.Types.Sacrifice's fields, and deliberately -- this is that record's
-- question asked about tapping rather than about sacrificing, which is why the
-- field names agree. The two are still separate types because the ACTIONS
-- differ: Pawl.Engine.Cost's arms pay one through Pawl.Engine.Event.sacrifice
-- and the other with a tap, and no reader wants to be told they are the same.
data TapPermanents keyword = MkTapPermanents
  { count :: Natural.Natural,
    whichPermanents :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
