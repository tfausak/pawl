module Pawl.Types.ExchangeZones where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.ZonePair as ZonePair

-- | CR 701.12d / 701.12f: the named players each exchange the cards in two of
-- their own zones.
data ExchangeZones = MkExchangeZones
  { player :: PlayerRef.PlayerRef,
    zones :: ZonePair.ZonePair
  }
  deriving (Eq, Ord, Show)
