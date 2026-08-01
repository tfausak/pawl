-- CR 508.1c / 509.1b / 613.11: the continuous effects that FORBID an attack or a
-- block. The fourth module on the axis CR 613.11 reaches past the layer system --
-- Pawl.Engine.PlayerEffect answers rules questions about a PLAYER,
-- Pawl.Engine.BlockRequirement the one CR 509.1c asks about a pair of CREATURES,
-- Pawl.Engine.AttackRequirement the one CR 508.1d asks about a single one, and this
-- the two CR 508.1c and CR 509.1b ask about a single one. None is a layer, and
-- Pawl.Engine.Projection sees none of them -- CR 613.11 applies all four "after all
-- other continuous effects have been applied".
--
-- This module is the only reader of Pawl.Types.CombatRestriction, and the only
-- module that may case on it. Pawl.Engine.Combat asks for a SET OF IDS -- which of
-- these creatures may not attack, which may not block -- and never learns which
-- card produced one, the same posture Pawl.Engine.Cast takes toward
-- Pawl.Engine.PlayerEffect.prohibitsCasting.
module Pawl.Engine.CombatRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1c: which of `candidates` an effect in force right now says CAN'T
-- ATTACK. Pacifism's first half.
cantAttack :: [ObjectId] -> GameState -> Set ObjectId
cantAttack = restricted attacking

-- CR 509.1b: which of `candidates` an effect in force right now says CAN'T
-- BLOCK. Pacifism's second half.
cantBlock :: [ObjectId] -> GameState -> Set ObjectId
cantBlock = restricted blocking

-- The two selectors, written out rather than a wildcard: this type has exactly
-- two arms, so an exhaustive case is what makes a third arm a compile error at
-- both of the sites that would have to decide about it.
attacking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attacking cr = case cr of
  CombatRestriction.CantAttack a -> Just a
  CombatRestriction.CantBlock _ -> Nothing

blocking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
blocking cr = case cr of
  CombatRestriction.CantAttack _ -> Nothing
  CombatRestriction.CantBlock a -> Just a

-- The shared walk behind both questions above, over the restrictions `select`
-- keeps.
--
-- A set of ids and not a per-creature predicate, which is the hoist
-- Pawl.Engine.Combat.canAttackGiven's comment argues for its other arguments: the
-- caller asks this once per declaration pass and then tests every candidate
-- against the answer, where a predicate would walk the whole battlefield per
-- candidate and make the pass quadratic in it (#200). `candidates` is the
-- caller's chosen-from set -- CR
-- 508.1a's for attacking, CR 509.1a's for blocking -- because the only creatures
-- whose restrictions can matter are ones the declaring player might have
-- declared.
--
-- The answer is applied to the CANDIDATE LIST rather than to the finished
-- declaration, which is CR 702.3b's defender doing the same thing one line up in
-- Pawl.Engine.Combat.canAttackGiven. That is sound only because every restriction
-- this type can state is per creature: such a creature is in no legal
-- declaration at all, so dropping it from the candidates changes no answer -- and
-- it is what keeps CR 508.1d's and CR 509.1c's maximizations honest, since a
-- creature that cannot act can obey no requirement either. A restriction on a SET
-- of creatures would need the whole declaration and could not be applied here
-- (#533).
restricted :: (CombatRestriction.CombatRestriction -> Maybe Affected.Affected) -> [ObjectId] -> GameState -> Set ObjectId
restricted select candidates gs =
  let -- Hoisted out of the walk exactly as Pawl.Engine.AttackRequirement.instances hoists
      -- them, and both unforced until some permanent actually declares a
      -- restriction -- so a board carrying no restriction at all pays for no
      -- projection.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.cardOf source gs of
        Nothing -> []
        Just card -> case Card.combatRestrictions card of
          -- Every permanent in almost every game.
          [] -> []
          restrictions ->
            -- The same TWO ability losses Pawl.Engine.AttackRequirement.instances asks
            -- about, for the same reasons and with the same unconditional cut:
            -- CR 305.7's basic-land subtype set, and CR 604.2's "remains on the
            -- battlefield and HAS THE ABILITY" against a CR 613.1f layer-6
            -- removal. The argument for why CR 613.6 cannot rescue a restriction
            -- that has "started to apply" is written out in
            -- Pawl.Engine.BlockRequirement.instances and is not repeated here; it turns
            -- on a restriction being its own carrier, which this type is too.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRestriction source) restrictions
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection rather than a partial one -- the
      -- opposite of Pawl.Engine.Projection.affects's callers inside the layer fold,
      -- which must read the partial characteristics as of their own layer.
      named source affected creature =
        Projection.affects
          source
          creature
          affected
          (Projection.project creature gs)
          gs
      fromRestriction source restriction = case select restriction of
        Nothing -> []
        Just affected -> filter (named source affected) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))
