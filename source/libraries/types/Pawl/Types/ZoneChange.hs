module Pawl.Types.ZoneChange where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone

-- | One zone-change event (CR 400.7): `object` is the RESULTING object's id (the
-- fresh incarnation in the destination), which is what an enters trigger scans,
-- and `departed` is the id the object had in the zone it LEFT.
--
-- BOTH ids, because CR 400.7 destroys the only link between them: the departing
-- id is the key GameState.lastKnown files the object's CR 608.2h information
-- under, so without it a departure event names nothing that can answer what the
-- permanent was or who controlled it -- which is what CR 603.10a's look-back for
-- leaves-the-battlefield abilities needs.
--
-- The two are the same value in the PROPOSED event Pawl.Engine.Replacement resolves
-- (Pawl.Types.ProposedEvent.WouldChangeZone): nothing has moved yet, so the
-- object that will leave is the only one that exists. They diverge only in the
-- RECORDED event, once the move has minted the destination incarnation.
data ZoneChange = MkZoneChange
  { departed :: ObjectId.ObjectId,
    object :: ObjectId.ObjectId,
    from :: Zone.Zone,
    to :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
