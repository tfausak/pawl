module Pawl.Combat where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Decide as Decide
import qualified Pawl.Departure as Departure
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
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
      Combat.struckFirst = Nothing,
      Combat.defender = Nothing
    }

-- CR 511.3: as soon as the end of combat step ends, all creatures, battles and
-- planeswalkers are removed from combat -- which by CR 506.4 is what stops them
-- being attacking and blocking creatures. Resetting `defender` alongside them is
-- CR 506.2, not CR 511.3: the designation is scoped to the combat phase, so it
-- cannot survive the phase ending.
--
-- Engine.runTurnBasedActions calls this from its end of combat step arm, i.e. as
-- that step BEGINS -- one step earlier than CR 511.3's boundary, and earlier than
-- either rule's scope ends (#180).
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 508.8: if no creatures were declared as attackers, skip the declare
-- blockers and combat damage steps. Called right after declareAttackers, when
-- the attacker set is final. "Put onto the battlefield attacking" (508.8) has no
-- source in the pool, so the attacker set really is final here (#30).
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Map.null (Combat.attackers (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.remaining gs)}
    else gs

-- CR 506.2a: the candidates the attacking player chooses from. Read only by
-- chooseDefender; the CHOSEN one lives in Combat.defender.
--
-- CR 506.2a says the attacking player chooses one of their opponents, and three
-- rules get from "opponents" to this list. CR 102.1: a player is one of the
-- people IN THE GAME, so someone who has left is not a player and cannot be an
-- opponent. CR 806.1: in a free-for-all the players compete as individuals, so
-- every other player is an opponent. CR 102.3 is the one reading this is wrong
-- for -- a teammate is not an opponent -- and pawl has no teams to express
-- (#175). Same argument Count.playersFor's PlayerRelation.Opponent arm carries,
-- phrased the same way on purpose. Filter.matches has an Opponent arm too, but it
-- carries no such note and is deliberately not cited here as though it did.
--
-- SEATING order (Departure.stillPlayingInOrder), not player-id order: the seating
-- roster is the game's own ordering for anything player-shaped (CR 800.5), and
-- Departure.stillPlaying's order is an artifact of reading the players map. It
-- makes the first candidate the next seat rather than the lowest id, which is
-- what an interpreter that takes the head should get.
attackableOpponents :: GameState -> [PlayerId]
attackableOpponents gs = filter (/= GameState.activePlayer gs) (Departure.stillPlayingInOrder gs)

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
      && (Object.sickness obj == Sickness.Settled pid || Projection.hasKeyword Keyword.Haste oid gs)
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
-- and activated abilities with the tap or untap symbol, and says nothing about
-- blocking.
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

-- CR 702.36b: a creature with fear can't be blocked except by artifact creatures
-- and/or black creatures.
--
-- The same asymmetry as flying (see evasionAllows): fear restricts being BLOCKED,
-- never blocking, so the question is asked of the ATTACKER first. Both halves of
-- the exception read the PROJECTION -- a creature made black by a CR 613 layer-5
-- effect blocks legally, and a devoid creature with a black mana cost does not.
fearAllows :: ObjectId -> ObjectId -> GameState -> Bool
fearAllows blocker attacker gs =
  not (Projection.hasKeyword Keyword.Fear attacker gs)
    || Set.member CardType.Artifact (Projection.cardTypesOf blocker gs)
    || Set.member Color.Black (Projection.colorsOf blocker gs)

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
-- Nothing in the pool creates a requirement, so that maximum is trivially zero.
-- This function is named for restrictions so requirements arrive as a SECOND
-- function rather than as a surprise inside this one (#27). The first
-- requirement also invalidates declareBlockers' fallback -- see there.
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockers pid gs
      -- CR 509.1a: the blocker must be one this player could block with at all,
      -- and the attacker must actually be attacking.
      wellFormed blocker attacker = List.elem blocker candidates && List.elem attacker attackers
      ok (blocker, attacker) =
        wellFormed blocker attacker
          && evasionAllows blocker attacker gs
          && fearAllows blocker attacker gs
   in all ok (Map.toList declaration)

blockersOf :: ObjectId -> GameState -> Set ObjectId
blockersOf oid gs = Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h: a creature remains blocked even if its blockers leave. This derives
-- blocked-ness from the map rather than storing it, so a departed blocker is not
-- honoured (#28). No library caller today -- CombatSpec is the only reader.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = not (Set.null (blockersOf oid gs))

-- CR 507.1 / CR 703.4h: immediately after the beginning of combat step begins,
-- the active player chooses one of their opponents, and that player becomes the
-- defending player. The turn-based action does not use the stack (CR 507.1), which
-- is why this is a plain Game () and not an ability.
--
-- Runs before any trigger, by construction rather than by arrangement:
-- Engine.runStep calls runTurnBasedActions before priorityLoop, and it is
-- priorityLoop's settleForPriority that first places a triggered ability.
--
-- Not prompted with one candidate (#169): CR 507.1's condition is a multiplayer
-- game, and a two-player game's defending player is CR 506.2's nonactive player
-- with nothing to ask.
--
-- No candidates means the action does not happen and Combat.defender stays
-- Nothing, which declareAttackers reads as no attack being possible. Unreachable
-- in a running game -- the last opponent leaving ends it (CR 104.2a) -- and
-- NonEmpty is what makes the prompt's fallback total rather than this branch.
--
-- An answer outside the candidate list is a broken interpreter, not a game state,
-- and degrades to the first candidate: always legal, least eventful, and the same
-- SHAPE (degrade totally rather than fail) Setup.subgameStateFrom uses for an
-- out-of-order starting player -- and the same VALUE Replay.defaultAnswer gives
-- this very prompt (NonEmpty.head candidates). The two must agree: a diverging
-- fallback here would be an invisible bug, since neither path can observe the
-- other.
chooseDefender :: Game ()
chooseDefender = do
  gs <- State.get
  let pid = GameState.activePlayer gs
  -- CR 800.4j: a turn whose active player has left continues without one, so the
  -- action the rules assign to the active player has no subject. CR 800.4j is a
  -- priority rule and stops there; CR 800.4h is the one that would hand this
  -- choice -- required of the active player by CR 507.1 and CR 703.4h -- to the
  -- next player in turn order. pawl skips it, which is an unobservable divergence
  -- rather than a vacuous case (#181); the argument is on Pawl.Type.Combat's
  -- defender field and is not repeated here.
  --
  -- Engine.runTurnBasedActions binds the identical test (hasActive) before
  -- calling this, so on the engine's path the two guards are the same value BY
  -- EQUIVALENCE -- the mechanism (what runs between the bind and the call, and
  -- why the predicate here is the same expression) is argued at that site, not
  -- here. Redundant on that path, and declined for a reason that is not a green
  -- suite: this is the copy a DIRECT caller depends on -- a spec, or a second
  -- combat phase spliced by an effect, neither of which goes through Engine's
  -- guard -- and CombatSpec's direct-call case is the test that discriminates
  -- it. Do not delete it as redundant; Engine.runTurnBasedActions's comment
  -- states this same argument from the other end.
  Monad.when (List.elem pid (Departure.stillPlaying gs)) $
    case NonEmpty.nonEmpty (attackableOpponents gs) of
      Nothing -> pure ()
      Just candidates -> do
        chosen <- case candidates of
          only NonEmpty.:| [] -> pure only
          _ -> do
            let decider = Decide.deciderFor pid gs
            answer <- Trans.lift (Program.prompt (Prompt.ChooseDefender decider pid candidates))
            pure $
              if List.elem answer (NonEmpty.toList candidates)
                then answer
                else NonEmpty.head candidates
        State.modify' $ \g ->
          g {GameState.combat = (GameState.combat g) {Combat.defender = Just chosen}}

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
  case Combat.defender (GameState.combat gs) of
    -- Nothing means no attack is possible, and that is the right answer rather
    -- than a fallback: either the beginning of combat step's turn-based action
    -- has not run, or it ran on a turn with no active player (CR 800.4j), or it
    -- found no opponents. Never a place to recompute a defender -- doing so is
    -- the head-of-list behaviour this replaced.
    Nothing -> pure ()
    Just defender ->
      -- CR 508.1b asks for a per-creature announcement only if the defending
      -- player controls a planeswalker, protects a battle, or the game lets the
      -- active player attack multiple other players. None of the three exists
      -- here, so every chosen creature attacks the one defending player and no
      -- second prompt is issued (#59).
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
            recorded = Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer defender)) attacking)
            attach g = g {GameState.combat = (GameState.combat g) {Combat.attackers = recorded}}
        State.modify' (\g -> attach (List.foldl' tapIt g attacking))

-- CR 509.1: the defending player declares blockers -- singular. The loop is over
-- at most one player, and Maybe.maybeToList is what makes "nobody is being
-- attacked" and "one player is" the same code path.
--
-- CR 802.4 is the rule that has each of several defending players declare blocks
-- in APNAP order, and CR 802.4a restricts each to blocking creatures attacking
-- them. Both need the attack-multiple-players option, which pawl has no options
-- concept to read (#175).
--
-- No still-playing guard: at three or more seats, a defending player who left
-- the game has had every object they owned removed by CR 800.4a, so
-- legalBlockers finds nothing for them and the inner Monad.unless
-- short-circuits. At two seats CR 800.4a never runs at all -- CR 800.1 gates it
-- on "more than two players" (Departure.continuesAfterDeparture) -- but CR
-- 104.2a ends the game the instant a player's last opponent leaves, which
-- Sba.checkSba records as GameState.result and Engine.playGame's loop reads
-- before ever calling declareBlockers again; see GameSpec.hs's
-- CR 800.4j/703.4i test for the state that leaves unreachable in play.
declareBlockers :: Game ()
declareBlockers = do
  start <- State.get
  let attacking = Map.keys (Combat.attackers (GameState.combat start))
  Monad.unless (null attacking)
    . Monad.forM_ (Maybe.maybeToList (Combat.defender (GameState.combat start)))
    $ \pid -> do
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
        -- state to fall back to. With a CR 509.1c requirement in the pool, "no
        -- blocks" can itself be illegal and this fallback stops being
        -- available (#27).
        gs1 <- State.get
        Monad.when (legalBlockDeclaration pid chosen gs1) $ do
          let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
              merged = List.foldl' add (Combat.blockers (GameState.combat gs1)) (Map.toList chosen)
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged}}
