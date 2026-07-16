module Pawl.Combat where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import qualified Pawl.Type.AttackTarget as AttackTarget
import Pawl.Type.Combat (Combat)
import qualified Pawl.Type.Combat as Combat
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
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

-- CR 508.1: the active player chooses which creatures attack, then they become
-- tapped and attacking (CR 508.1f).
--
-- No legal attackers means no prompt: declining is then the only legal answer,
-- and asking would be inventing a decision. Same reasoning as CR 510.1c's single
-- blocker.
declareAttackers :: PlayerId -> Game ()
declareAttackers pid = do
  gs <- State.get
  let candidates = legalAttackers pid gs
  case defendingPlayers gs of
    -- Nobody to attack. Cannot happen while the game is running -- the last
    -- opponent leaving ends it -- but the branch keeps this total.
    [] -> pure ()
    defender : _ ->
      Monad.unless (null candidates) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.DeclareAttackers decider pid candidates))
        -- Filtered, not trusted: an interpreter cannot attack with a creature
        -- that is not legally an attacker.
        let isCandidate oid = List.elem oid candidates
            attacking = filter isCandidate chosen
            tapIt g oid = g {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects g)}
            recorded = Map.fromList (map (\oid -> (oid, AttackTarget.OfPlayer defender)) attacking)
            attach g = g {GameState.combat = (GameState.combat g) {Combat.attackers = recorded}}
        State.modify' (\g -> attach (List.foldl' tapIt g attacking))

-- CR 509.1: each defending player chooses which of their creatures block, and
-- which attacker each blocks.
declareBlockers :: Game ()
declareBlockers = do
  start <- State.get
  let attacking = Map.keys (Combat.attackers (GameState.combat start))
  Monad.unless (null attacking) $
    Monad.forM_ (defendingPlayers start) $ \pid -> do
      gs <- State.get
      let candidates = legalBlockers pid gs
      Monad.unless (null candidates) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.DeclareBlockers decider pid candidates attacking))
        -- Filtered, not trusted: the blocker must be theirs and legal, and the
        -- attacker must actually be attacking.
        let legal b a = List.elem b candidates && List.elem a attacking
            accepted = Map.filterWithKey legal chosen
            add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
            merged = List.foldl' add (Combat.blockers (GameState.combat gs)) (Map.toList accepted)
        State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged}}
