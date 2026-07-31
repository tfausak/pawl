-- CR 508.1d / 613.11: the continuous effects that REQUIRE an attack. The twin of
-- Pawl.Engine.BlockRequirement on the other side of the combat phase, and the third
-- module on the axis CR 613.11 reaches past the layer system: Pawl.Engine.PlayerEffect
-- answers rules questions about a PLAYER, Pawl.Engine.BlockRequirement the one CR 509.1c
-- asks about a pair of CREATURES, and this the one CR 508.1d asks about a single
-- one. None is a layer, and Pawl.Engine.Projection sees none of them -- CR 613.11 applies
-- all three "after all other continuous effects have been applied".
--
-- This module is the only reader of Pawl.Types.AttackRequirement. Pawl.Engine.Combat asks
-- for requirement INSTANCES -- bare creature ids -- and never learns which card
-- produced one, the same posture Pawl.Engine.Cast takes toward
-- Pawl.Engine.PlayerEffect.prohibitsCasting.
module Pawl.Engine.AttackRequirement where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.Card as Card
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1d: every requirement in force right now, INSTANTIATED as the creatures
-- the active player is required to declare as attackers.
--
-- A set of ids and not a count, because CR 508.1d counts requirements that are
-- "being OBEYED" by a particular declaration, so each one has to stay identifiable
-- against the declaration it is checked against. One instance per CREATURE, not
-- per ability: CR 508.1d opens "the active player checks EACH CREATURE THEY
-- CONTROL to see whether it's affected by any requirements", so one Curse over
-- three able creatures is three requirements, not one. Two Curses over the SAME
-- creature collapse to one instance, which is the same dedupe
-- Pawl.Engine.BlockRequirement.instances makes on identical pairs and costs no answer:
-- a declaration obeys both or neither.
--
-- `candidates` is CR 508.1a's chosen-from set (Pawl.Engine.Combat.legalAttackers), and
-- it carries the "if able" of "attacks each combat if able". Passed IN rather
-- than computed here, exactly as Pawl.Engine.BlockRequirement takes its `able`
-- predicate, so this module never learns the restrictions and Pawl.Engine.Combat stays
-- the only home for them. Pruning by it changes no answer -- a creature that
-- cannot attack is attacking in no legal declaration, so it would contribute zero
-- to every candidate INCLUDING the maximum -- but it is what makes the instance
-- set the answer to CR 508.1d's maximum rather than an upper bound on it.
--
-- No `able` predicate BESIDE the candidate list, where the blocking twin has one:
-- there, CR 509.1b's restrictions are pairwise (flying, fear) and cannot be
-- decided per blocker. Every attacking restriction pawl models today is per
-- creature and already inside Pawl.Engine.Combat.canAttack, so the two collapse into
-- the one argument (#459).
instances :: [ObjectId] -> GameState -> Set ObjectId
instances candidates gs =
  let -- Hoisted out of the walk exactly as Pawl.Engine.BlockRequirement.instances hoists
      -- them, and both unforced until some permanent actually declares a
      -- requirement -- so a board carrying no requirement at all pays for no
      -- projection.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.cardOf source gs of
        Nothing -> []
        Just card -> case Card.attackRequirements card of
          -- Every permanent in almost every game.
          [] -> []
          requirements ->
            -- The same TWO ability losses Pawl.Engine.BlockRequirement.instances asks
            -- about, for the same reasons and with the same unconditional cut:
            -- CR 305.7's basic-land subtype set, and CR 604.2's "remains on the
            -- battlefield and HAS THE ABILITY" against a CR 613.1f layer-6
            -- removal. The argument for why CR 613.6 cannot rescue a requirement
            -- that has "started to apply" is written out there and is not
            -- repeated here; it turns on a requirement being its own carrier,
            -- which this type is too.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRequirement source) requirements
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection rather than a partial one -- the
      -- opposite of Pawl.Engine.Projection.affects's callers inside the layer fold,
      -- which must read the partial characteristics as of their own layer.
      named source requirement creature =
        Projection.affects
          source
          creature
          (AttackRequirement.subject requirement)
          (Projection.project creature gs)
          gs
      fromRequirement source requirement = filter (named source requirement) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))
