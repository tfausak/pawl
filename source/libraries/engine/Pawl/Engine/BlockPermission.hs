-- CR 509.1a / 613.11: the continuous effects that let a creature block MORE
-- than the one creature CR 509.1a gives it. The fourth module on the axis CR
-- 613.11 reaches past the layer system, alongside Pawl.Engine.PlayerEffect,
-- Pawl.Engine.CombatRestriction and the two requirement modules. None is a
-- layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.BlockPermission. Pawl.Engine.Combat asks for a
-- NUMBER per creature -- or for no bound at all -- and never learns which card
-- produced it.
module Pawl.Engine.BlockPermission where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 509.1a: how many creatures BEYOND the first each of `candidates` may block
-- right now. Absent from the map is the ordinary creature, which blocks one.
--
-- SUMMED and not minimised, which is what separates this from
-- Pawl.Engine.CombatRestriction.bounded: two permissions are two additional
-- creatures, since each says "an additional" about the same blocker. That is the
-- whole reason Pawl.Types.BlockPermission is not an arm of
-- Pawl.Types.CombatRestriction.
--
-- Nothing is a card's "any number of creatures" (Palace Guard) -- no bound at
-- all rather than a large one -- and it ABSORBS in the sum, which is what
-- `(+) <$> _ <*> _` does: a creature that may block any number still may after a
-- High Ground adds one.
--
-- A map and not a per-creature predicate, for the reason
-- Pawl.Engine.CombatRestriction.restricted gives: the caller asks once per
-- declaration pass and tests against the answer, where a function would walk the
-- battlefield per candidate (#200).
additionalBlocks :: [ObjectId] -> GameState -> Map.Map ObjectId (Maybe Natural)
additionalBlocks candidates gs =
  let -- Hoisted out of the walk exactly as CombatRestriction.inForce hoists them,
      -- and unforced until some permanent actually prints a permission.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts this effect after every layer, so the affected set is read
      -- against the FULL projection.
      named source affected creature =
        Projection.affects source creature affected (Projection.project creature gs) gs
      -- CR 604.2's "as long as", read exactly as CombatRestriction.inForce reads
      -- its "unless" and with the opposite polarity: a permission whose gate does
      -- NOT hold grants nothing. Asked once per permission and not per candidate,
      -- because CR 109.5 fixes the "you" inside it as the SOURCE's controller.
      granted source changes permission = case BlockPermission.while permission of
        Nothing -> True
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.contextFor (Projection.controllerOf source gs) (Just source))
            gs
            source
            (if null changes then condition else Projection.rewriteCondition changes condition)
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.blockPermissions face of
          -- Every permanent in almost every game.
          [] -> []
          permissions ->
            -- The same two ability losses CombatRestriction.inForce asks about:
            -- CR 305.7's basic-land subtype set, and CR 604.2 against a CR
            -- 613.1f layer-6 removal.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs) && not (removed source)
              then
                -- CR 612.1's word swap over the source's own text, computed here
                -- rather than hoisted for CombatRestriction.inForce's reason: the
                -- empty case above already turned away every permanent that
                -- prints no permission.
                let changes = Projection.textChangesAffecting source gs
                 in [ (creature, BlockPermission.additional permission)
                    | permission <- permissions,
                      granted source changes permission,
                      let affected = BlockPermission.affected permission,
                      creature <- candidates,
                      named source (if null changes then affected else Projection.rewriteAffected changes affected) creature
                    ]
              else []
   in Map.fromListWith (\a b -> (+) <$> a <*> b) (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))
