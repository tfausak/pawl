module Pawl.Types.ExilePermanents where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's ExilePermanents arm: CR 118.1's
-- cost written as an exile (CR 406.2), spent by exiling exactly this many
-- permanents the payer chooses out of the ones the Filter admits. Food Chain's
-- "Exile a creature you control" is the printing.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is. Pawl.Types.ReturnPermanents' fields, a record
-- of its own because the action differs.
data ExilePermanents keyword = MkExilePermanents
  { count :: Natural.Natural,
    whichPermanents :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
