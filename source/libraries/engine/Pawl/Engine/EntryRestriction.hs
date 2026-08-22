-- CR 400.4a / 101.2 / 613.11: the continuous effects that FORBID an object from
-- entering the battlefield. One of the modules on the axis CR 613.11 reaches past
-- the layer system (alongside Pawl.Engine.PlayerEffect,
-- Pawl.Engine.BlockRequirement, Pawl.Engine.AttackRequirement,
-- Pawl.Engine.CombatRestriction, Pawl.Engine.AttackCost,
-- Pawl.Engine.SacrificeRestriction and Pawl.Engine.UntapRestriction). None is a
-- layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.EntryRestriction. Its caller asks a Bool about one
-- move and never learns which card produced it.
--
-- ONE place asks, unlike Pawl.Engine.SacrificeRestriction's two:
-- Pawl.Engine.Event.changeZoneAttaching is the funnel every battlefield entry
-- reaches -- a resolving permanent spell, a resolving Effect.MoveToZone, a land
-- play, a search that puts a card onto the battlefield, CR 701.40a's manifest --
-- so gating it there gates all of them at once. CR 111.5's token, which
-- Event.createTokens mints without taking that funnel, is the one entry this
-- module does not reach (gap #2066).
module Pawl.Engine.EntryRestriction where

import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.EntryRestriction as EntryRestriction
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Zone (Zone)

-- CR 400.4a with CR 101.2: does an effect in force right now say that `oid`,
-- currently in `origin`, CAN'T ENTER THE BATTLEFIELD? Grafdigger's Cage's first
-- sentence.
--
-- Pawl.Engine.SacrificeRestriction.cantBeSacrificed's body, gate for gate, and
-- every step of its argument holds here unchanged: the same CR 305.7 and CR
-- 613.1f ability losses are asked of the SOURCE, the same CR 612.1 word swap is
-- applied to its affected set, and CR 613.11 is why the set is read against the
-- FULL projection rather than a layer-bounded one.
--
-- A Bool about ONE object rather than that function's set over a candidate list,
-- which is the shape of the question rather than a narrowing: `origin` is a
-- property of the individual move, so a candidate list would need one zone per
-- candidate, and the single caller holds exactly one move.
--
-- `origin` is passed in rather than read off the object, because the caller knows
-- which move is being judged: Pawl.Engine.Event.changeZoneAttaching asks after CR
-- 616.1's replacement loop has settled the event (CR 614.6), where the object is
-- still in the zone it is leaving.
prohibited :: ObjectId -> Zone -> GameState -> Bool
prohibited oid origin gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection.
      named source affected =
        Projection.affects
          source
          oid
          affected
          (Projection.project oid gs)
          gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> False
        Just face -> case Face.entryRestrictions face of
          -- Every permanent in almost every game.
          [] -> False
          restrictions ->
            (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              && any (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
      fromRestriction source changes restriction =
        let affected = EntryRestriction.affected restriction
         in Set.member origin (EntryRestriction.origins restriction)
              && named source (if null changes then affected else Projection.rewriteAffected changes affected)
   in any fromPermanent (Set.toList (GameState.battlefield gs))
