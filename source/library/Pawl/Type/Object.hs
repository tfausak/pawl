module Pawl.Type.Object where

import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Source (Source)
import Pawl.Type.TapState (TapState)
import Pawl.Type.Zone (Zone)

data Object = MkObject
  { owner :: PlayerId,
    source :: Source,
    zone :: Zone,
    tapped :: TapState
  }
  deriving (Eq, Ord, Show)
