module Pawl.Types.Sacrifice where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's Sacrifice arm (#1305): CR 701.21a
-- as a cost, sacrificing this many matching permanents.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- A record of its OWN rather than one shared with
-- Pawl.Types.TapForTotalPower, whose fields have the same two types: that
-- constructor's Natural is a THRESHOLD on an aggregate and this one's is HOW
-- MANY objects, matched exactly, which is the distinction the CostComponent arm
-- spends a paragraph drawing. A shared record would have to name the field
-- something that is true of neither.
data Sacrifice keyword = MkSacrifice
  { count :: Natural.Natural,
    whichPermanents :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
