-- CR 502.3 / 101.2 / 613.11: the continuous effects that FORBID CR 502.3's untap.
-- One of the modules on the axis CR 613.11 reaches past the layer system
-- (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement, Pawl.Engine.CombatRestriction,
-- Pawl.Engine.AttackCost and Pawl.Engine.SacrificeRestriction). None is a layer,
-- and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.UntapRestriction. Its caller asks for a SET OF
-- IDS and never learns which card produced it.
--
-- ONE place asks, unlike Pawl.Engine.SacrificeRestriction's two: the sentence
-- names the untap step's turn-based action and nothing else, so an Effect.Untap
-- during the turn (CR 701.26b) is untouched -- which is the printed reading, not
-- a shortcut.
--
-- The PRINTED carrier alone. The one-shot prohibition stored on the victim
-- (Object.doesNotUntapNext, written by Effect.DoesNotUntapNext or by CR 508.1g's
-- exert payment) is not gathered here and could not be: it is a field on the VICTIM rather than on anything the
-- battlefield walk below would reach. Engine.untapAll subtracts both.
module Pawl.Engine.UntapRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.UntapRestriction as UntapRestriction

-- CR 502.3 with CR 101.2: which of `candidates` an effect in force right now says
-- DOESN'T UNTAP during its controller's untap step. Tsabo's Web's second
-- sentence.
--
-- Pawl.Engine.SacrificeRestriction.cantBeSacrificed's body, gate for gate, and
-- every step of its argument holds here unchanged: the same CR 305.7 and CR
-- 613.1f ability losses are asked of the SOURCE, the same CR 612.1 word swap is
-- applied to its affected set, and CR 613.11 is why the set is read against the
-- FULL projection rather than a layer-bounded one.
doesNotUntap :: [ObjectId] -> GameState -> Set ObjectId
doesNotUntap candidates gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      named source affected candidate =
        Projection.affects
          source
          candidate
          affected
          (Projection.project candidate gs)
          gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.untapRestrictions face of
          -- Every permanent in almost every game.
          [] -> []
          restrictions ->
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then concatMap (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
              else []
      fromRestriction source changes restriction =
        let affected = UntapRestriction.affected restriction
         in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))
