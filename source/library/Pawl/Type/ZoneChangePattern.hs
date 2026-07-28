module Pawl.Type.ZoneChangePattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.Zone (Zone)
import Pawl.Type.ZoneChangeSubject (ZoneChangeSubject)

-- CR 614.1a: which zone changes a redirect intercepts. Rest in Peace is
-- (Graveyard, Anyones, AnyObject) -- any object that would be put into a
-- graveyard from anywhere. `whenDestination` is compared against the event's
-- CURRENT destination, which is why a redirect whose output no longer matches
-- its own trigger destination cannot re-fire even before CR 614.5 is consulted.
--
-- `whichObject` narrows to the effect's own source (CR 702.34a's "exile THIS
-- card"); `whoseObject` narrows by the object's OWNER. Orthogonal: a self-scoped
-- pattern leaves `whoseObject` at Anyones, which is vacuously true of the one
-- object it already admits.
--
-- `whenDestination` is ONE zone, so CR 702.34a's "instead of putting it anywhere
-- else" is expressible only as the one destination a spell actually leaves the
-- stack for in this pool (#293).
data ZoneChangePattern = MkZoneChangePattern
  { whenDestination :: Zone,
    whoseObject :: ControllerRelation,
    whichObject :: ZoneChangeSubject
  }
  deriving (Eq, Ord, Show)
