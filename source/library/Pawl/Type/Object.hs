module Pawl.Type.Object where

import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Sickness (Sickness)
import Pawl.Type.Source (Source)
import Pawl.Type.TapState (TapState)
import Pawl.Type.Zone (Zone)

data Object = MkObject
  { owner :: PlayerId,
    source :: Source,
    zone :: Zone,
    tapped :: TapState,
    -- CR 302.6. Per-incarnation state: reset by changeZone.
    sickness :: Sickness
  }
  deriving (Eq, Ord, Show)
