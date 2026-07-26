-- CR 302.6 (the "summoning sickness" rule) and the haste exemptions that relax
-- it. One predicate, three readers: Pawl.Combat asks it before attacking (CR
-- 702.10b), Pawl.Activate before a {T} activated ability and Pawl.Mana before a
-- {T} mana ability (both CR 702.10c).
--
-- Shared rather than repeated because the three had already drifted: combat read
-- haste and the two ability paths did not, so a creature with haste could attack
-- but not tap (#205). A single predicate makes that particular disagreement
-- unrepresentable.
module Pawl.Summoning where

import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Sickness as Sickness

-- CR 302.6: has `pid` controlled `oid` continuously since their most recent turn
-- began -- or, failing that, does it have haste?
--
-- Keyed to a player because CR 302.6's subject is one: "under ITS CONTROLLER'S
-- control since THEIR most recent turn began". A settle recorded for anyone else
-- answers a different question (#198).
--
-- The two haste rules say the same thing for different actions, so one predicate
-- serves both. CR 702.10b: "If a creature has haste, it can attack even if it
-- hasn't been controlled by its controller continuously since their most recent
-- turn began." CR 702.10c: the same for "activated abilities whose cost includes
-- the tap symbol or the untap symbol". Haste is read through the projection, so
-- a GRANTED haste (Act of Treason's rider) counts exactly as a printed one does.
settledOrHasty :: PlayerId -> ObjectId -> GameState -> Bool
settledOrHasty pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.sickness obj == Sickness.Settled pid
      || Projection.hasKeyword Keyword.Haste oid gs
