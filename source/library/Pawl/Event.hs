-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and, later, the sole casing on ReplacementEffect and TriggerCondition.
-- changeZone lives here (not in Pawl.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Event where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Pawl.Game as Game
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.Zone (Zone)
import Pawl.Type.ZoneChange (ZoneChange)
import qualified Pawl.Type.ZoneChange as ZoneChange

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid dest gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        (newId, gs1) = Game.freshObjectId gs
        (ts, gs1b) = Game.freshTimestamp gs1
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty, Object.chosenSubtypes = Map.empty, Object.timestamp = ts}
        gs2 = Game.removeFromZones pid oid gs1b
        gs3 = gs2 {GameState.objects = Map.insert newId newObj (Map.delete oid (GameState.objects gs2))}
     in Game.insertIntoZone dest pid newId gs3

-- CR 614: rewrite the proposed zone change by each active replacement. CR 614.5:
-- a replacement gets ONE opportunity -- applied left-to-right, each sees the
-- running event; RedirectZoneChange's output destination no longer matches its
-- own `whenDestination`, so it cannot re-fire. This module is the sole home of
-- casing on ReplacementEffect.
applyReplacements :: [ReplacementEffect] -> ZoneChange -> ZoneChange
applyReplacements res zc = List.foldl' applyOne zc res

applyOne :: ZoneChange -> ReplacementEffect -> ZoneChange
applyOne zc re = case re of
  ReplacementEffect.RedirectZoneChange whenDest toDest ->
    if ZoneChange.to zc == whenDest
      then zc {ZoneChange.to = toDest}
      else zc
