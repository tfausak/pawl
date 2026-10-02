-- CR 702.3b / 613.11: the continuous effects that let a creature with defender
-- attack anyway. One of the modules on the axis CR 613.11 reaches past the layer
-- system, alongside Pawl.Engine.BlockPermission and
-- Pawl.Engine.CombatRestriction; none is a layer.
--
-- The only reader of Pawl.Types.AttackPermission. Pawl.Engine.Combat asks for a
-- SET OF IDS and never learns which card produced it.
module Pawl.Engine.AttackPermission where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.AttackPermission as AttackPermission
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.RuleAbilities as RuleAbilities

-- CR 702.3b: which of `candidates` an effect in force right now lets attack as
-- though it didn't have defender. Pawl.Engine.CrewRestriction.cantCrew's body,
-- gate for gate: the same CR 305.7 and CR 613.1f ability losses asked of the
-- source, the same CR 612.1 word swap over its affected set, read against the
-- FULL projection (CR 613.11).
waivesDefender :: [ObjectId] -> GameState -> Set ObjectId
waivesDefender candidates gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      named source affected candidate = Projection.affectsOn pcs grants source candidate affected gs
      -- CR 613.1f: what a stored grant gave the permanent (Prison Barricade's
      -- kicked entry), with no CR 612.1 word swap (CR 612.3).
      grantedRules = Projection.grantedRuleAbilities gs
      fromGrant source = concatMap (fromPermission source []) (RuleAbilities.attackPermissions (grantedRules source))
      fromPermanent source = case RuleAbilities.attackPermissions (Projection.ruleAbilitiesOf source gs) of
        -- Every permanent in almost every game.
        [] -> []
        permissions ->
          if (null setEffs || Projection.liveAfterLayers setEffs source gs)
            && not (removed source)
            then concatMap (fromPermission source (Projection.readerTextChanges source gs)) permissions
            else []
      fromPermission source changes permission =
        let affected = AttackPermission.affected permission
         in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap (\source -> fromPermanent source <> fromGrant source) (Set.toList (GameState.battlefield gs)))
