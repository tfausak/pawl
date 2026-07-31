-- CR 302.6 (the "summoning sickness" rule) and the haste exemptions that relax
-- it. One predicate, three readers: Pawl.Engine.Combat asks it before attacking (CR
-- 702.10b), Pawl.Engine.Activate before a {T} activated ability and Pawl.Engine.Mana before a
-- {T} mana ability (both CR 702.10c).
--
-- Shared rather than repeated because the three had already drifted: combat read
-- haste and the two ability paths did not, so a creature with haste could attack
-- but not tap (#205). A single predicate makes that particular disagreement
-- unrepresentable.
module Pawl.Engine.Summoning where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Sickness as Sickness

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
settledOrHasty = settledOrHastyGiven Map.empty

-- The same predicate against a pre-projected board, so a caller sweeping the
-- battlefield reads haste out of the one projection it already took rather than
-- projecting per creature (#200). See Projection.projectGiven for what the board
-- is and why Map.empty above is the same answer.
settledOrHastyGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> Bool
settledOrHastyGiven pcs pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.sickness obj == Sickness.Settled pid
      || Projection.hasKeywordGiven pcs Keyword.Haste oid gs
