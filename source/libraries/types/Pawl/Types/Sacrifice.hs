module Pawl.Types.Sacrifice where

import qualified Pawl.Types.CostAmount as CostAmount
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's Sacrifice arm (#1305): CR 701.21a
-- as a cost, sacrificing this many matching permanents.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- A record of its OWN rather than one shared with
-- Pawl.Types.TapForTotalPower: that constructor's number is a THRESHOLD on an
-- aggregate and this one's is HOW MANY objects, matched exactly, which is the
-- distinction the CostComponent arm spends a paragraph drawing.
data Sacrifice keyword = MkSacrifice
  { count :: CostAmount.CostAmount,
    whichPermanents :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
