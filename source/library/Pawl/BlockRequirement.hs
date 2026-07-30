-- CR 509.1c / 613.11: the continuous effects that REQUIRE a block. A sibling of
-- Pawl.PlayerEffect, on the other axis CR 613.11 reaches: that module answers
-- rules questions about a PLAYER, this one answers the one rules question CR
-- 509.1c asks about a pair of CREATURES. Neither is a layer, and Pawl.Projection
-- sees neither -- CR 613.11 applies both "after all other continuous effects have
-- been applied".
--
-- This module is the only reader of Pawl.Type.BlockRequirement. Pawl.Combat asks
-- for requirement INSTANCES -- bare (blocker, attacker) pairs -- and never learns
-- which card produced one, the same posture Pawl.Cast takes toward
-- Pawl.PlayerEffect.prohibitsCasting.
module Pawl.BlockRequirement where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.BlockRequirement as BlockRequirement
import qualified Pawl.Type.Card as Card
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ObjectId (ObjectId)

-- CR 509.1c: every requirement in force right now, INSTANTIATED as the
-- (blocker, attacker) pairs the defending player is required to declare.
--
-- A pair and not a count, because CR 509.1c counts requirements that are "being
-- OBEYED" by a particular declaration, so each one has to stay identifiable
-- against the declaration it is checked against. Per BLOCKER, not per ability:
-- CR 509.1c opens "the defending player checks EACH CREATURE THEY CONTROL to see
-- whether it's affected by any requirements", so one Lure over three able
-- creatures is three requirements, not one.
--
-- `able` is the caller's CR 509.1b restriction check, which is what Lure's "able
-- to block" means. Passed IN rather than computed here, so this module never
-- learns the restrictions and Pawl.Combat stays the only home for them. Pruning
-- by it changes no answer -- a pair that disobeys a restriction is obeyed by no
-- legal declaration, so it would contribute zero to every candidate INCLUDING
-- the maximum -- but it is what keeps the maximization's search space to the
-- pairs that can actually happen.
--
-- `candidates` is CR 509.1a's chosen-from set (Pawl.Combat.legalBlockers) and
-- `attackers` the attacking creatures. Both are handed in for the same reason:
-- they are the defending player's, and only the caller knows who that is.
instances ::
  (ObjectId -> ObjectId -> Bool) ->
  [ObjectId] ->
  [ObjectId] ->
  GameState ->
  Set (ObjectId, ObjectId)
instances able candidates attackers gs =
  let -- Hoisted out of the walk exactly as Pawl.PlayerEffect.applying hoists them,
      -- and both unforced until some permanent actually declares a requirement --
      -- so a board carrying no requirement at all pays for no projection.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.cardOf source gs of
        Nothing -> []
        Just card -> case Card.blockRequirements card of
          -- Every permanent in almost every game.
          [] -> []
          requirements ->
            -- TWO ability losses, the same pair Pawl.PlayerEffect.applying asks
            -- about for the other axis CR 613.11 reaches.
            --
            -- CR 305.7: a land whose subtype has been SET to a basic type loses
            -- its rules-text abilities, this one included.
            --
            -- CR 604.2: a static ability's continuous effect is active only while
            -- the permanent "remains on the battlefield and has the ability", so a
            -- CR 613.1f layer-6 removal takes this one with it (Humility on Prized
            -- Unicorn). CR 613.6's rescue -- an effect that has STARTED to apply
            -- continues "even if the ability generating the effect is removed
            -- during this process" -- cannot reach it: CR 613.11 applies a
            -- requirement "after all other continuous effects have been applied",
            -- so it has no layer to have started applying in. Nor could another
            -- part of the same card's text have started on its behalf: a
            -- requirement is its OWN carrier (Pawl.Type.BlockRequirement), never a
            -- part of a StaticAbility, so CR 613.6's "the same set of objects in
            -- each other applicable layer" has nothing here to hold together. The
            -- cut is therefore unconditional.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRequirement source) requirements
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection rather than a partial one -- the
      -- opposite of Pawl.Projection.affects's callers inside the layer fold,
      -- which must read the partial characteristics as of their own layer.
      named source requirement attacker =
        Projection.affects
          source
          attacker
          (BlockRequirement.attacker requirement)
          (Projection.project attacker gs)
          gs
      fromRequirement source requirement =
        let pairsFor attacker = fmap (\blocker -> (blocker, attacker)) (filter (\blocker -> able blocker attacker) candidates)
         in concatMap pairsFor (filter (named source requirement) attackers)
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))
