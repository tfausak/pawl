module Pawl.Types.ReturnPermanents where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's ReturnPermanents arm: CR 118.1's
-- cost written as a return to hand, spent by returning exactly this many
-- permanents the payer chooses out of the ones the Filter admits. Meloku the
-- Clouded Mirror's "Return a land you control to its owner's hand" is the
-- printing, and CR 400.3 is what makes the destination its OWNER's hand.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- Pawl.Types.TapPermanents' fields, and a record of its own for that type's
-- reason: count is HOW MANY objects and must be matched exactly. The two are
-- separate types because the ACTIONS differ -- Pawl.Engine.Cost pays one with a
-- tap and this one through Pawl.Engine.Event.changeZone.
data ReturnPermanents keyword = MkReturnPermanents
  { count :: Natural.Natural,
    whichPermanents :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
