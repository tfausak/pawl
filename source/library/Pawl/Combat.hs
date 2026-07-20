module Pawl.Combat where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.AttackTarget as AttackTarget
import Pawl.Type.Combat (Combat)
import qualified Pawl.Type.Combat as Combat
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
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
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing
    }

-- CR 511.3: creatures stop being attacking and blocking at end of combat.
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 508.8: if no creatures were declared as attackers, skip the declare
-- blockers and combat damage steps. Called right after declareAttackers, when
-- the attacker set is final. "Put onto the battlefield attacking" (508.8) has no
-- source at M2b; EXPIRES at M4+ with the effects that create attacking creatures.
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Map.null (Combat.attackers (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.remaining gs)}
    else gs

-- CR 506.2. M1b is two-player, so this is "the other one" and choosing whom to
-- attack is not a choice at all.
--
-- Grows: multiplayer, where the attacking player chooses among opponents, and
-- planeswalkers/battles, at which point AttackTarget becomes a real decision.
defendingPlayers :: GameState -> [PlayerId]
defendingPlayers gs = filter (/= GameState.activePlayer gs) (Sba.stillPlaying gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject = Projection.isCreatureOf

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6).
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOf oid gs == Just pid
      && GameState.activePlayer gs == pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      -- CR 302.6, relaxed by CR 702.10b: a creature with haste can attack even if
      -- it hasn't been controlled continuously since its controller's most recent
      -- turn began.
      && (Object.sickness obj == Sickness.Settled || Projection.hasKeyword Keyword.Haste oid gs)
      && isCreatureObject oid gs
      -- CR 702.3b: a creature with defender can't attack. It may still block --
      -- 702.3b says nothing about blocking.
      && not (Projection.hasKeyword Keyword.Defender oid gs)

legalAttackers :: PlayerId -> GameState -> [ObjectId]
legalAttackers pid gs = filter (\oid -> canAttack pid oid gs) (Projection.controls pid gs)

-- CR 509.1a: a blocking creature must be untapped and controlled by the
-- defending player.
--
-- Summoning sickness is NOT a blocking restriction. CR 302.6 restricts attacking
-- and activated abilities with the tap symbol, and says nothing about blocking.
canBlock :: PlayerId -> ObjectId -> GameState -> Bool
canBlock pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOf oid gs == Just pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && isCreatureObject oid gs

legalBlockers :: PlayerId -> GameState -> [ObjectId]
legalBlockers pid gs = filter (\oid -> canBlock pid oid gs) (Projection.controls pid gs)

-- CR 702.9b: a creature with flying can't be blocked except by creatures with
-- flying and/or reach (CR 702.17b).
--
-- Note the asymmetry, which is easy to get backwards: 702.9b's second sentence
-- says a creature with flying CAN block a creature with or without flying.
-- Flying restricts being blocked, never blocking. The question is asked of the
-- ATTACKER first, and only then of the blocker.
evasionAllows :: ObjectId -> ObjectId -> GameState -> Bool
evasionAllows blocker attacker gs =
  not (Projection.hasKeyword Keyword.Flying attacker gs)
    || Projection.hasKeyword Keyword.Flying blocker gs
    || Projection.hasKeyword Keyword.Reach blocker gs

-- CR 509.1b: the defending player checks each creature for RESTRICTIONS, and if
-- any are disobeyed the DECLARATION is illegal.
--
-- The unit of legality is the whole declaration, not the pair, and that is not a
-- stylistic choice. Menace (CR 702.111, one punchlist entry away) says a creature
-- can't be blocked except by TWO OR MORE creatures -- a constraint on the SET
-- blocking an attacker, which no per-pair predicate can express. Only flying and
-- reach are pairwise; designing to them would be designing to the case that
-- misleads. See the M2a spec, section 3.
--
-- A conjunction of independent restriction checks, because CR 509.1b says
-- different evasion abilities are cumulative: an attacker with flying AND shadow
-- admits only blockers that answer both.
--
-- CR 509.1c REQUIREMENTS ("must block if able") are NOT implemented, and are not
-- a check but a maximization: 509.1c demands the declaration obey the maximum
-- possible number of requirements achievable without disobeying any restriction.
-- Nothing in M2a creates a requirement, so that maximum is trivially zero. This
-- function is named for restrictions so requirements arrive as a SECOND function
-- rather than as a surprise inside this one. EXPIRES with the first requirement,
-- which also invalidates declareBlockers' fallback -- see there.
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockers pid gs
      -- CR 509.1a: the blocker must be one this player could block with at all,
      -- and the attacker must actually be attacking.
      wellFormed blocker attacker = List.elem blocker candidates && List.elem attacker attackers
      ok (blocker, attacker) = wellFormed blocker attacker && evasionAllows blocker attacker gs
   in all ok (Map.toList declaration)

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
            -- CR 508.1f: declaring an attacker taps it -- unless it has vigilance
            -- (CR 702.20b), which does not change WHETHER it attacks, only what
            -- attacking does to it.
            tapIt g oid =
              if Projection.hasKeyword Keyword.Vigilance oid g
                then g
                else g {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects g)}
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
        -- CR 509.1b: an illegal declaration is illegal AS A WHOLE. It is NOT
        -- filtered down to its legal entries -- that is unsound, not merely
        -- inelegant: under menace, dropping one blocker from a pair leaves an
        -- illegal single block, so the filter would manufacture the illegality it
        -- was meant to remove. M1b settled the identical question for CR 510.1e:
        -- "checks the assignment AS A WHOLE, so this cannot be repaired by
        -- filtering the way a discard can."
        --
        -- This is NOT CR 733's rewind. An enforcing engine never offers an
        -- illegal declaration, so only a broken interpreter arrives here, and
        -- re-prompting a pure `Prompt r -> r` returns the identical wrong answer.
        --
        -- Declining to block is always legal today, so "no blocks" is a legal
        -- state to fall back to. EXPIRES with CR 509.1c requirements: once
        -- something must block, "no blocks" can itself be illegal and this
        -- fallback stops being available.
        gs1 <- State.get
        Monad.when (legalBlockDeclaration pid chosen gs1) $ do
          let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
              merged = List.foldl' add (Combat.blockers (GameState.combat gs1)) (Map.toList chosen)
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged}}
