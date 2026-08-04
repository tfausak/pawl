-- CR 509.1c / 613.11: the continuous effects that REQUIRE a block. One of the
-- modules on the axis CR 613.11 reaches past the layer system, alongside
-- Pawl.Engine.PlayerEffect (which answers rules questions about a PLAYER).
-- Neither is a layer, and Pawl.Engine.Projection sees neither.
--
-- The only reader of Pawl.Types.BlockRequirement. Pawl.Engine.Combat asks for
-- requirement INSTANCES -- bare (blocker, attacker) pairs -- and never learns
-- which card produced one.
module Pawl.Engine.BlockRequirement where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as Card
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 509.1c: every requirement in force right now, INSTANTIATED as the
-- (blocker, attacker) pairs the defending player is required to declare.
--
-- A pair and not a count, because CR 509.1c counts requirements being OBEYED by
-- a particular declaration, so each one must stay identifiable against the
-- declaration it is checked against. Per BLOCKER, not per ability: CR 509.1c
-- checks each creature the defending player controls, so one Lure over three
-- able creatures is three requirements.
--
-- `able` is the caller's CR 509.1b restriction check, which is what Lure's
-- "able to block" means. Passed IN rather than computed here, so this module
-- never learns the restrictions. Pruning by it changes no answer -- a pair that
-- disobeys a restriction is obeyed by no legal declaration -- but it keeps the
-- maximization's search space to the pairs that can actually happen.
--
-- `candidates` is CR 509.1a's chosen-from set and `attackers` the attacking
-- creatures. Both are handed in because they are the defending player's, and
-- only the caller knows who that is.
instances ::
  (ObjectId -> ObjectId -> Bool) ->
  [ObjectId] ->
  [ObjectId] ->
  GameState ->
  Set (ObjectId, ObjectId)
instances able candidates attackers gs =
  let -- Hoisted out of the walk as PlayerEffect.applying hoists them, and both
      -- unforced until some permanent actually declares a requirement.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.cardOf source gs of
        Nothing -> []
        Just card -> case Card.blockRequirements card of
          -- Every permanent in almost every game.
          [] -> []
          requirements ->
            -- TWO ability losses, the same pair PlayerEffect.applying asks
            -- about.
            --
            -- CR 305.7: a land whose subtype has been SET to a basic type loses
            -- its rules-text abilities, this one included.
            --
            -- CR 604.2: a static ability's continuous effect is active only
            -- while the permanent remains on the battlefield and HAS the
            -- ability, so a CR 613.1f layer-6 removal takes this one with it
            -- (Humility on Prized Unicorn). CR 613.6's rescue for an effect
            -- that has started to apply cannot reach it: CR 613.11 applies a
            -- requirement after every layer, so there is no layer for it to
            -- have started in. Nor could another part of the same card's text
            -- have started on its behalf -- a requirement is its OWN carrier,
            -- never part of a StaticAbility, so CR 613.6 has nothing here to
            -- hold together. The cut is unconditional.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRequirement source) requirements
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
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
