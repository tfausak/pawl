module Pawl.Type.ZoneChangePattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.Zone (Zone)

-- CR 614.1a: which zone changes a redirect intercepts. Rest in Peace is
-- (Graveyard, Anyones) -- any object that would be put into a graveyard from
-- anywhere. `whenDestination` is compared against the event's CURRENT
-- destination, which is why a redirect whose output no longer matches its own
-- trigger destination cannot re-fire even before CR 614.5 is consulted.
data ZoneChangePattern = MkZoneChangePattern
  { whenDestination :: Zone,
    whoseObject :: ControllerRelation
  }
  deriving (Eq, Ord, Show)
