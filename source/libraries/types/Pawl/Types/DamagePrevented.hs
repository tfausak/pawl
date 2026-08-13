module Pawl.Types.DamagePrevented where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.1: how much damage a prevention shield stopped, and who it was headed
-- for.
data DamagePrevented = MkDamagePrevented
  { recipient :: Recipient.Recipient,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
