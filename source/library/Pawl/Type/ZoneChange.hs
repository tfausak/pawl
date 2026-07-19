module Pawl.Type.ZoneChange where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Zone (Zone)

-- One zone-change event (CR 400.7): `object` is the RESULTING object's id (the
-- fresh incarnation in the destination), which is what an enters trigger scans.
-- `from` is carried for the future leaves-the-battlefield pass (M3f reads `to`).
data ZoneChange = MkZoneChange
  { object :: ObjectId,
    from :: Zone,
    to :: Zone
  }
  deriving (Eq, Ord, Show)
