module Pawl.Combat where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import Pawl.Type.Combat (Combat)
import qualified Pawl.Type.Combat as Combat
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

emptyCombat :: Combat
emptyCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty
    }

-- CR 511.3: creatures stop being attacking and blocking at end of combat.
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 506.2. M1b is two-player, so this is "the other one" and choosing whom to
-- attack is not a choice at all.
--
-- Grows: multiplayer, where the attacking player chooses among opponents, and
-- planeswalkers/battles, at which point AttackTarget becomes a real decision.
defendingPlayers :: GameState -> [PlayerId]
defendingPlayers gs = filter (/= GameState.activePlayer gs) (Sba.stillPlaying gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject oid gs = fmap Card.isCreature (Game.cardOf oid gs) == Just True

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6).
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Game.controllerOf oid gs == Just pid
      && GameState.activePlayer gs == pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && Object.sickness obj == Sickness.Settled
      && isCreatureObject oid gs

legalAttackers :: PlayerId -> GameState -> [ObjectId]
legalAttackers pid gs = filter (\oid -> canAttack pid oid gs) (Game.zoneMembers Zone.Battlefield pid gs)

-- CR 509.1a: a blocking creature must be untapped and controlled by the
-- defending player.
--
-- Summoning sickness is NOT a blocking restriction. CR 302.6 restricts attacking
-- and activated abilities with the tap symbol, and says nothing about blocking.
canBlock :: PlayerId -> ObjectId -> GameState -> Bool
canBlock pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Game.controllerOf oid gs == Just pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && isCreatureObject oid gs

legalBlockers :: PlayerId -> GameState -> [ObjectId]
legalBlockers pid gs = filter (\oid -> canBlock pid oid gs) (Game.zoneMembers Zone.Battlefield pid gs)

blockersOf :: ObjectId -> GameState -> Set ObjectId
blockersOf oid gs = Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h: a creature remains blocked even if its blockers leave. M1b cannot
-- construct that state -- nothing removes a blocker mid-combat without
-- instant-speed interaction -- so this is derived rather than stored.
-- EXPIRES at M2.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = not (Set.null (blockersOf oid gs))
