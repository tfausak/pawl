-- CR 701.21a / 101.2 / 613.11: the continuous effects that FORBID a sacrifice.
-- One of the modules on the axis CR 613.11 reaches past the layer system
-- (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement, Pawl.Engine.CombatRestriction and
-- Pawl.Engine.AttackCost). None is a layer, and Pawl.Engine.Projection sees
-- none of them.
--
-- The only reader of Pawl.Types.SacrificeRestriction. Every caller asks for a
-- SET OF IDS -- or for one id's answer -- and never learns which card produced
-- it.
--
-- WHERE THE ANSWER IS ASKED is two kinds of place, and both are needed. CR
-- 101.2 makes the "can't" beat the "can", so a prohibited permanent must
-- neither be OFFERED as a sacrifice (Pawl.Engine.Replacement.sacrificeCandidates,
-- which is what CR 118.3's payability count and every sacrifice prompt read)
-- nor be sacrificed by an instruction that named it without asking
-- (Pawl.Engine.Event.sacrifice, the CR 701.21 funnel). Gating only the first
-- would leave every targeted sacrifice in the pool going through.
module Pawl.Engine.SacrificeRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction

-- CR 701.21a with CR 101.2: which of `candidates` an effect in force right now
-- says CAN'T BE SACRIFICED. Garland, Royal Kidnapper's third clause.
--
-- A set of ids and not a per-candidate predicate, for the reason
-- Pawl.Engine.CombatRestriction.restricted gives: a caller narrowing a whole
-- battlefield asks this once and tests against the answer, where a predicate
-- would re-walk the battlefield per candidate.
cantBeSacrificed :: [ObjectId] -> GameState -> Set ObjectId
cantBeSacrificed candidates gs =
  let -- Hoisted out of the walk as AttackRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a prohibition.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of Projection.affects's
      -- callers inside the layer fold, which read characteristics as of their
      -- own layer.
      named source affected candidate =
        Projection.affects
          source
          candidate
          affected
          (Projection.project candidate gs)
          gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.sacrificeRestrictions face of
          -- Every permanent in almost every game.
          [] -> []
          restrictions ->
            -- The same two ability losses AttackRequirement.instances asks
            -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
            -- CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
            -- restriction that has started to apply is argued in
            -- BlockRequirement.instances, as is why CR 613.11 also lets the CR
            -- 305.7 gate be liveAfterLayers rather than liveGiven.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then
                -- CR 612.1's word swap over the SOURCE's own text, computed here
                -- rather than hoisted beside setEffs, the placement
                -- CombatRestriction.restricted argues for: textChangesAffecting
                -- folds the whole continuous-effect list, and the empty case
                -- above already turned away every permanent that prints no
                -- prohibition.
                concatMap (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
              else []
      fromRestriction source changes restriction =
        let affected = SacrificeRestriction.affected restriction
         in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))

-- The same question about ONE permanent, for the callers that hold a victim
-- rather than a candidate list: the CR 701.21 funnel itself, and the two cost
-- components that name the permanent paying with itself.
--
-- Not a cheaper computation, just a narrower one -- the walk above is over the
-- battlefield either way, and the candidate list it filters is the singleton.
prohibited :: ObjectId -> GameState -> Bool
prohibited oid gs = Set.member oid (cantBeSacrificed [oid] gs)
