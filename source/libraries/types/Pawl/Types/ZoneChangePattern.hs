module Pawl.Types.ZoneChangePattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

-- | CR 614.1a: which zone changes a redirect intercepts. Rest in Peace is
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
  { whenDestination :: Zone.Zone,
    whoseObject :: ControllerRelation.ControllerRelation,
    whichObject :: ZoneChangeSubject.ZoneChangeSubject
  }
  deriving (Eq, Ord, Show)
