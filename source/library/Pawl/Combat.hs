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
import qualified Pawl.BlockRequirement as BlockRequirement
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Summoning as Summoning
import qualified Pawl.Turn as Turn
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import Pawl.Types.Combat (Combat)
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

emptyCombat :: Combat
emptyCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attackersJoined = False,
      Combat.defender = Nothing
    }

-- CR 511.3: as soon as the end of combat step ends, all creatures, battles and
-- planeswalkers are removed from combat -- which by CR 506.4 is what stops them
-- being attacking and blocking creatures. Resetting `defender` alongside them is
-- CR 506.2, not CR 511.3: the designation is scoped to the combat phase, so it
-- cannot survive the phase ending.
--
-- Engine.runStep calls this as the end of combat step ENDS, alongside CR 500.5's
-- mana emptying -- not from runTurnBasedActions, which is a step's opening and
-- which CR 511.1 says this step has none of. Neighbours in time only: CR 703.4q
-- makes that emptying a turn-based action, and this is not one.
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 508.8: "If no creatures are declared as attackers or put onto the
-- battlefield attacking, skip the declare blockers and combat damage steps."
--
-- BOTH of that rule's clauses are the same question of Combat.attackersJoined,
-- because both things that can make one true write that flag: declareAttackers
-- below, and putOntoBattlefieldAttacking. Engine.runStepThatBegan asks it as the
-- declare attackers step ENDS -- after the priority round in which an attack
-- trigger resolves -- rather than the moment the turn-based action finishes,
-- which is what made the second clause unrepresentable before.
--
-- The flag and NOT Map.null on Combat.attackers, which is the same question only
-- while nothing leaves combat. CR 508.8 asks whether a creature WAS declared or
-- put onto the battlefield attacking, and CR 508.1k makes that a different
-- question from whether one is attacking now: a declared creature "remains an
-- attacking creature until it's removed from combat", and CR 506.4's removal
-- takes away the attacking, never the declaration. Asking the map skipped both
-- steps for a lone attacker that a Ray of Command took during the step, which is
-- TurnSpec's proving test; Pawl.Replacement's CR 701.19a regeneration reaches the
-- same Game.removeFromCombat door.
--
-- A creature put onto the battlefield attacking LATER than that step cannot
-- un-skip anything, and does not need to: the steps this drops are the only ones
-- of THIS combat phase it could enter during, so if they were dropped there is
-- no such moment, and if they were not then nothing is skipped anyway. A CR
-- 500.8 additional combat phase later in the turn is not this call's business
-- and Turn.dropSkippedCombatSteps does not touch it -- that phase reaches its
-- own declare attackers step and asks this question again for itself.
--
-- No GAMEPLAY-level test exercises the second clause on its own -- a creature put
-- onto the battlefield attacking while nothing was declared. The pool's only
-- source of one is an attack trigger, which cannot fire unless its own creature
-- was declared, so TurnSpec proves that clause by calling
-- putOntoBattlefieldAttacking directly (#370).
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if not (Combat.attackersJoined (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.phase gs) (GameState.remaining gs)}
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
-- SEATING order (Game.stillPlayingInOrder), not player-id order: the seating
-- roster is the game's own ordering for anything player-shaped (CR 800.5), and
-- Game.stillPlaying's order is an artifact of reading the players map. It
-- makes the first candidate the next seat rather than the lowest id, which is
-- what an interpreter that takes the head should get.
attackableOpponents :: GameState -> [PlayerId]
attackableOpponents gs = filter (/= GameState.activePlayer gs) (Game.stillPlayingInOrder gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject = isCreatureObjectGiven Map.empty

isCreatureObjectGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureObjectGiven = Projection.isCreatureGiven

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6).
--
-- canAttackGiven is the half a LOOP wants: `grants` is one control-grant walk
-- (Projection.controlGrants) and `pcs` one whole-board projection
-- (Projection.projectAll), taken once per declaration pass by legalAttackers
-- below rather than once per candidate -- the questions this asks are otherwise
-- as many as three fresh gathers (haste, creature-ness, defender) and a fresh
-- grant walk apiece, which made the pass quadratic in the battlefield (#200).
-- Same hoist Sba.performStateBasedActions takes for the CR 704.3 sweep and
-- Projection.controls takes for the grant list; Projection.projectGiven carries
-- the argument for why a shared board is the same answer, and for why it is
-- valid only within one pure pass over one GameState.
--
-- canAttack itself passes Map.empty, so a lone query projects per read exactly
-- as it always did.
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = canAttackGiven (Projection.controlGrants gs) Map.empty pid oid gs

canAttackGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> Bool
canAttackGiven grants pcs pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      && GameState.activePlayer gs == pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      -- CR 302.6, relaxed by CR 702.10b: a creature with haste can attack even if
      -- it hasn't been controlled continuously since its controller's most recent
      -- turn began.
      && Summoning.settledOrHastyGiven pcs pid oid gs
      && isCreatureObjectGiven pcs oid gs
      -- CR 702.3b: a creature with defender can't attack. It may still block --
      -- 702.3b says nothing about blocking.
      && not (Projection.hasKeywordGiven pcs Keyword.Defender oid gs)

legalAttackers :: PlayerId -> GameState -> [ObjectId]
legalAttackers pid gs =
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
   in filter (\oid -> canAttackGiven grants pcs pid oid gs) (Projection.controlsGiven grants pid gs)

-- CR 509.1a: a blocking creature must be untapped and controlled by the
-- defending player.
--
-- Summoning sickness is NOT a blocking restriction. CR 302.6 restricts attacking
-- and activated abilities with the tap or untap symbol, and says nothing about
-- blocking.
--
-- canBlockGiven/legalBlockersGiven are canAttackGiven's pair, hoisted for the
-- same reason and with the same snapshot argument.
canBlock :: PlayerId -> ObjectId -> GameState -> Bool
canBlock pid oid gs = canBlockGiven (Projection.controlGrants gs) Map.empty pid oid gs

canBlockGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> Bool
canBlockGiven grants pcs pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && isCreatureObjectGiven pcs oid gs

legalBlockers :: PlayerId -> GameState -> [ObjectId]
legalBlockers pid gs = legalBlockersGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

legalBlockersGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
legalBlockersGiven grants pcs pid gs =
  filter (\oid -> canBlockGiven grants pcs pid oid gs) (Projection.controlsGiven grants pid gs)

-- CR 702.9b: a creature with flying can't be blocked except by creatures with
-- flying and/or reach (CR 702.17b).
--
-- Note the asymmetry, which is easy to get backwards: 702.9b's second sentence
-- says a creature with flying CAN block a creature with or without flying.
-- Flying restricts being blocked, never blocking. The question is asked of the
-- ATTACKER first, and only then of the blocker.
evasionAllows :: ObjectId -> ObjectId -> GameState -> Bool
evasionAllows = evasionAllowsGiven Map.empty

evasionAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
evasionAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Flying attacker gs)
    || Projection.hasKeywordGiven pcs Keyword.Flying blocker gs
    || Projection.hasKeywordGiven pcs Keyword.Reach blocker gs

-- CR 702.36b: a creature with fear can't be blocked except by artifact creatures
-- and/or black creatures.
--
-- The same asymmetry as flying (see evasionAllows): fear restricts being BLOCKED,
-- never blocking, so the question is asked of the ATTACKER first. Both halves of
-- the exception read the PROJECTION -- a creature made black by a CR 613 layer-5
-- effect blocks legally, and a devoid creature with a black mana cost does not.
fearAllows :: ObjectId -> ObjectId -> GameState -> Bool
fearAllows = fearAllowsGiven Map.empty

fearAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
fearAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Fear attacker gs)
    || Set.member CardType.Artifact (Projection.cardTypesGiven pcs blocker gs)
    || Set.member Color.Black (Projection.colorsGiven pcs blocker gs)

-- CR 509.1b asked of ONE (blocker, attacker) pair: may this creature block that
-- one at all? This is also what CR 509.1c's requirements mean by "able to block"
-- (Lure), which is why it is a named function and not a lambda inside the
-- declaration check.
--
-- A conjunction of independent restriction checks, because CR 509.1b says
-- different evasion abilities are cumulative: an attacker with flying AND shadow
-- admits only blockers that answer both.
--
-- Every restriction in the pool today happens to be pairwise. Menace (CR 702.111b,
-- "can't be blocked except by two or more creatures") is not -- it constrains the
-- SET blocking one attacker -- and when it lands it belongs in
-- declarationAllowed, which is asked of the whole declaration, never here.
pairAllowed :: [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowed = pairAllowedGiven Map.empty

-- pairAllowed against a pre-projected board, which is what the callers below
-- pass: this question is asked once per (blocker, attacker) PAIR, so each of its
-- evasion reads was a fresh gather in a doubly nested loop (#200).
pairAllowedGiven :: Map ObjectId PC.ProjectedCharacteristics -> [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowedGiven pcs candidates attackers blocker attacker gs =
  -- CR 509.1a: the blocker must be one this player could block with at all, and
  -- the attacker must actually be attacking.
  List.elem blocker candidates
    && List.elem attacker attackers
    && evasionAllowsGiven pcs blocker attacker gs
    && fearAllowsGiven pcs blocker attacker gs

-- CR 509.1b: the defending player checks each creature for RESTRICTIONS, and if
-- any are disobeyed the DECLARATION is illegal.
--
-- The unit of legality is the whole declaration, not the pair, and that is not a
-- stylistic choice. Menace (CR 702.111b, one punchlist entry away) says a creature
-- can't be blocked except by TWO OR MORE creatures -- a constraint on the SET
-- blocking an attacker, which no per-pair predicate can express. Only flying and
-- reach are pairwise; designing to them would be designing to the case that
-- misleads. See the M2a spec, section 3. So this stays a whole-declaration
-- function even though its body is currently a fold of pairAllowed: this is the
-- seam a set-shaped restriction plugs into, and it is the seam blockCeiling's
-- enumeration is filtered through.
declarationAllowed :: (ObjectId -> ObjectId -> Bool) -> Map ObjectId ObjectId -> Bool
declarationAllowed able declaration = all (uncurry able) (Map.toList declaration)

-- How many of `requirements` this declaration obeys -- CR 509.1c's "the number of
-- requirements that are being obeyed". A requirement instance is obeyed exactly
-- when the declaration has its blocker blocking its attacker.
requirementsMet :: Set (ObjectId, ObjectId) -> Map ObjectId ObjectId -> Int
requirementsMet requirements declaration =
  Set.size (Set.filter (\(blocker, attacker) -> Map.lookup blocker declaration == Just attacker) requirements)

-- Every declaration CR 509.1a lets the defending player write down, given the
-- pairs CR 509.1b allows: each candidate blocker independently either blocks
-- nothing or blocks one attacker it may block.
--
-- EXPONENTIAL, and honestly so: the list has product over candidates of
-- (1 + how many attackers that candidate may block) entries, so it is
-- O((attackers + 1) ^ blockers) in the worst case. Nothing caps it and nothing
-- samples it -- a cap would answer CR 509.1c's "maximum possible number" with a
-- number that is not the maximum, which is worse than being slow. What keeps it
-- off the hot path is blockCeiling's guard: this is never called unless some
-- requirement is actually in force, which needs a card like Lure on the
-- battlefield (#342).
candidateDeclarations :: (ObjectId -> ObjectId -> Bool) -> [ObjectId] -> [ObjectId] -> [Map ObjectId ObjectId]
candidateDeclarations able candidates attackers =
  let extend acc blocker =
        let options = Nothing : fmap Just (filter (\attacker -> able blocker attacker) attackers)
            apply declaration option = case option of
              Nothing -> declaration
              Just attacker -> Map.insert blocker attacker declaration
         in concatMap (\declaration -> fmap (apply declaration) options) acc
   in List.foldl' extend [Map.empty] candidates

-- CR 509.1c's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the "maximum possible
-- number of requirements that could be obeyed without disobeying any
-- restrictions".
--
-- Map.empty when no requirement is in force, WITHOUT enumerating anything. That
-- is not an optimization of the common case so much as the whole of it: with no
-- requirement the maximum is zero, every declaration obeys zero, and CR 509.1c
-- has nothing to say. A board without Lure never pays a search.
--
-- The maximum is taken by folding rather than by `maximum`, and the fold's seed is
-- Map.empty -- which is always a legal declaration under restrictions alone, since
-- declining to block disobeys no restriction -- so the answer is total and needs
-- no partial function. Ties go to the EARLIER declaration in enumeration order;
-- which one is picked matters only to forcedBlockDeclaration's broken-interpreter
-- path, never to legality, which compares counts.
--
-- One grant walk and one whole-board projection for the whole search, threaded
-- into the candidate list and into every pair `able` judges -- see canAttackGiven
-- and Projection.projectGiven. Nothing between the projection and its uses can
-- move: this is a pure function of one GameState. blockCeilingGiven is the half
-- legalBlockDeclaration reaches, so that the two of them share one board rather
-- than taking one apiece.
blockCeiling :: PlayerId -> GameState -> (Set (ObjectId, ObjectId), Map ObjectId ObjectId)
blockCeiling pid gs = blockCeilingGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

blockCeilingGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> (Set (ObjectId, ObjectId), Map ObjectId ObjectId)
blockCeilingGiven grants pcs pid gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      able blocker attacker = pairAllowedGiven pcs candidates attackers blocker attacker gs
      requirements = BlockRequirement.instances able candidates attackers gs
      better best declaration =
        if requirementsMet requirements declaration > requirementsMet requirements best
          then declaration
          else best
      legal = filter (declarationAllowed able) (candidateDeclarations able candidates attackers)
   in ( requirements,
        if Set.null requirements
          then Map.empty
          else List.foldl' better Map.empty legal
      )

-- CR 509.1: is this declaration one the defending player may make? Both checks
-- the rule asks for, in the order it asks them: CR 509.1b's restrictions, then CR
-- 509.1c's requirements.
--
-- CR 509.1c is not a check but a MAXIMIZATION -- "if the number of requirements
-- that are being obeyed is fewer than the maximum possible number of requirements
-- that could be obeyed without disobeying any restrictions, the declaration of
-- blockers is illegal" -- so it cannot be asked of the declaration alone. It is
-- what makes declaring no blockers at all illegal while a Lure is on the
-- battlefield.
--
-- CR 509.1c's cost clause ("if a creature can't block unless a player pays a
-- cost, that player is not required to pay that cost") and CR 509.1d's cost
-- locking are not implemented: no card in the pool makes blocking cost anything
-- (#343).
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  -- Hoisted exactly as blockCeiling hoists, and for the same reason.
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      able blocker attacker = pairAllowedGiven pcs candidates attackers blocker attacker gs
      (requirements, best) = blockCeilingGiven grants pcs pid gs
   in declarationAllowed able declaration
        && requirementsMet requirements declaration >= requirementsMet requirements best

-- A declaration that is always legal: one attaining CR 509.1c's maximum, which
-- with no requirement in force is the empty one (declining to block). Not an
-- answer the engine ever prefers to the defending player's own -- declareBlockers
-- reaches for it only when an interpreter hands back a declaration the rules
-- forbid.
forcedBlockDeclaration :: PlayerId -> GameState -> Map ObjectId ObjectId
forcedBlockDeclaration pid gs = snd (blockCeiling pid gs)

-- Who is CURRENTLY blocking this attacker -- not whether it is blocked. The two
-- questions come apart (see isBlocked), and a reader that wants blocked-ness must
-- ask isBlocked rather than test this for emptiness.
blockersOf :: ObjectId -> GameState -> Set ObjectId
blockersOf oid gs = Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h: "An attacking creature with one or more creatures declared as
-- blockers for it becomes a blocked creature ... A creature remains blocked even
-- if all the creatures blocking it are removed from combat."
--
-- So blocked-ness is a STATUS that the declaration confers once, not a running
-- count of who is still blocking. The map's KEY is that status -- declareBlockers
-- creates it and only Game.removeFromCombat's Map.delete arm (the attacker itself
-- leaving combat, CR 506.4) and Combat.clearCombat ever drop it. The SET behind
-- the key is the separate CR 510.1c question of who is currently blocking, and it
-- can empty out while the key stays: a regenerated blocker (CR 701.19a) is deleted
-- from it, and a blocker that merely died is filtered out at assignment time.
--
-- Testing the set for emptiness instead is the bug this replaced: a Goblin Piker
-- blocked by a Drudge Skeletons that regenerated before the combat damage step
-- became "unblocked" and hit the defending player for 2. DamageSpec's
-- "Blocked stays blocked" group is what proves it, both ways the set can empty.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = Map.member oid (Combat.blockers (GameState.combat gs))

-- Every creature currently IN combat: the attackers, plus everything still
-- blocking one of them. Not the keys of Combat.joinedUnder, which can outlive the
-- record it was taken for -- Pawl.Departure edits Combat.attackers directly, and
-- deliberately leaves Combat.blockers alone (CR 509.1h).
combatants :: Combat -> Set ObjectId
combatants c = Set.union (Map.keysSet (Combat.attackers c)) (Set.unions (Map.elems (Combat.blockers c)))

-- CR 506.4: "A permanent is removed from combat if ... its controller changes ...
-- or if it's an attacking or blocking creature that ... stops being a creature."
--
-- The two clauses of that rule whose trigger is DERIVED state, which is why this
-- is a sampler and not a hook. Neither has an event to hang a removal on: a
-- control-granting static ability (Control Magic's SetControllerToSource) is
-- re-read live by the projection, and even a stored SetController is installed by
-- a resolution that never announces "control changed" (#198); creature-ness is
-- the same, a CR 613 layer-4 answer that changes the moment the effect producing
-- it appears or ends. The same shape, and the same argument, as
-- Engine.checkControlContinuity's CR 302.6 scan; Engine's settleForPriority runs
-- both, at every point the board can change.
--
-- The TIMING that costs: the rules remove the permanent the instant the
-- characteristic changes, and this notices at the next settle. Nothing can see
-- the difference. CR 117.5 makes "whenever a player would get priority" the
-- coarsest moment anything observes the board, and the two readers of the combat
-- record -- the CR 510 damage steps and Filter.IsAttacking at targeting -- both
-- sit behind a priority grant, which settles first. The window that would open it
-- is a single resolution that changes control or card types and then reads combat
-- status in a LATER effect of the same resolution; no card in the pool has one,
-- and the settle loop is where such a card's fix would go.
--
-- It only ever REMOVES. That asymmetry is what makes the sampling sound, exactly
-- as it is there: a discrepancy proves the characteristic changed, so removing is
-- always right, while putting a creature BACK when control returns or the
-- animation is recast would invent a CR 506.4 the rules do not have -- removal
-- from combat is permanent for that combat phase (the glossary: "has no further
-- involvement in that combat phase").
--
-- Battlefield-scoped, so this stays these two clauses and nothing else. CR 110.1
-- makes a permanent something on the battlefield, and an object that has LEFT it
-- was already removed by that separate clause of CR 506.4 -- whose implementation
-- is elsewhere (Pawl.Departure, and Pawl.Damage's liveness filters for the
-- CR 509.1h key this must not disturb). Without the gate, an object gone from
-- GameState.objects would answer Nothing to both questions here and be swept up
-- under the wrong clause.
--
-- Removal goes through Game.removeFromCombat, so a removed ATTACKER takes its
-- blocked-ness with it (Map.delete) while a removed BLOCKER leaves the attacker
-- blocked with nothing blocking it (Set.delete inside a surviving key) --
-- CR 509.1h's last sentence, argued in full at that function.
--
-- The types clause reads the creature card type ALONE, and that is exact today
-- rather than a simplification of CR 506.4d/e: those two subrules are about a
-- permanent that is also an attacked planeswalker or battle, and neither card
-- type is modeled (#301, #302), so nothing in pawl's combat record is anything
-- but a creature. CR 506.4's "becomes a battle" clause is unreachable for the
-- same reason, and "phases out" for phasing's (#154).
--
-- A combatant with no entry in Combat.joinedUnder is left alone by the CONTROL
-- clause, because there is nothing to compare it against and this only ever
-- removes. Unreachable through the engine: declareAttackers and declareBlockers
-- write the snapshot in the same update that puts the creature into the record.
-- The types clause needs no such comparand -- CR 506.3 lets only a creature be
-- declared, so every combatant was one, and "is it one now" is the whole test.
--
-- Nobody in combat short-circuits, which is most of the game: the grant list and
-- the gathered candidate list each cost a whole-battlefield scan
-- (Projection.controlGrants, Projection.gather), both hoisted out of the
-- per-combatant loop, and this runs on every settle pass alongside
-- checkControlContinuity's own. The gather is NOT shared with the one
-- Sba.performStateBasedActions makes earlier in the same settle pass: a
-- state-based action can change the board between the two, and a sample has to
-- gather against the state it is judging (Projection.projectGiven's own caveat).
removeChanged :: GameState -> GameState
removeChanged gs =
  let c = GameState.combat gs
      inCombat = combatants c
      grants = Projection.controlGrants gs
      cands = Projection.gather gs
      controlChanged oid = case Map.lookup oid (Combat.joinedUnder c) of
        Nothing -> False
        Just who -> Projection.controllerOfGiven grants Set.empty oid gs /= Just who
      stoppedBeingCreature oid = not (Projection.isCreatureFrom cands oid gs)
      onBattlefield oid = Set.member oid (GameState.battlefield gs)
      changed oid = controlChanged oid || stoppedBeingCreature oid
      leaving = filter (\oid -> onBattlefield oid && changed oid) (Set.toList inCombat)
   in if Set.null inCombat
        then gs
        else List.foldl' (flip Game.removeFromCombat) gs leaving

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
  -- rather than a vacuous case (#181); the argument is on Pawl.Types.Combat's
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
  Monad.when (List.elem pid (Game.stillPlaying gs)) $
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
            -- CR 506.4's comparand, taken here because here is where the creature
            -- joins combat. `pid` and not a fresh Projection.controllerOf call:
            -- canAttack has already required controllerOf == Just pid of every
            -- creature in `attacking` (CR 508.1a), so the two are the same value
            -- and this one cannot disagree with the legality check.
            joined = Map.fromList (fmap (\oid -> (oid, pid)) attacking)
            -- UNIONED into the record, not written over it. Nothing in the pool
            -- can have joined combat before this runs -- putOntoBattlefieldAttacking
            -- is reachable only from a resolution, and the earliest one is the
            -- priority round after this action -- but "the record is mine alone"
            -- is exactly the assumption CR 508.8's second clause breaks, and
            -- replacing the map would silently remove such a creature from combat.
            attach g =
              g
                { GameState.combat =
                    (GameState.combat g)
                      { Combat.attackers = Map.union recorded (Combat.attackers (GameState.combat g)),
                        Combat.joinedUnder = Map.union joined (Combat.joinedUnder (GameState.combat g)),
                        -- CR 508.8's first clause, recorded here because here is
                        -- where the declaration happens. Never cleared, so a
                        -- CR 506.4 removal later in the step cannot un-declare
                        -- these creatures.
                        Combat.attackersJoined =
                          Combat.attackersJoined (GameState.combat g) || not (null attacking)
                      }
                }
        State.modify' (\g -> attach (List.foldl' tapIt g attacking))
        -- CR 508.2b: the declaration is what abilities trigger on, and CR 508.3a
        -- scopes them to a creature that "is declared as an attacker" -- so one
        -- event per creature chosen HERE, and none at all for a creature put onto
        -- the battlefield attacking. Recorded after the record is written, so the
        -- board a trigger's intervening-if clause reads (CR 603.4) already has
        -- these creatures attacking.
        State.modify' (\g -> List.foldl' (\h oid -> Event.recordEvent (GameEvent.AttackerDeclared oid) h) g attacking)

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player, planeswalker a defending player controls, or
-- battle a defending player protects it's attacking as it enters the
-- battlefield." Resolve's Create arm calls this for each token an effect says is
-- attacking; nothing else does.
--
-- The creature was never DECLARED, and this function's whole difference from
-- declareAttackers is what follows from that. It records no
-- GameEvent.AttackerDeclared, so CR 508.3a's "such abilities won't trigger if a
-- creature is put onto the battlefield attacking" holds by construction rather
-- than by a filter. It taps nothing, because CR 508.1f taps what is declared
-- (the tokens' own tapped status is the creating effect's, applied at entry). And
-- it asks none of canAttack's questions -- CR 508.4c: "a creature that's put onto
-- the battlefield attacking ... isn't affected by requirements or restrictions
-- that apply to the declaration of attackers" -- so summoning sickness (CR 302.6,
-- whose sentence is "a creature can't ATTACK unless...") and defender (CR 702.3b)
-- do not reach it. CombatSpec's "the tokens are attacking, and the attack trigger
-- fired only for the Garrison" proves the trigger half.
--
-- What it does check is the three ways the rules say the creature enters WITHOUT
-- being an attacking creature. CR 506.3a: a noncreature permanent "does enter the
-- battlefield but it's never considered to be an attacking or blocking
-- permanent". CR 506.3b: the same for a creature entering "under the control of
-- any player except an attacking player", which by CR 506.2's first sentence is
-- the active player.
-- CR 506.3c and CR 508.4a: the same for one attacking "a player not in the game".
-- Each is a silent no-op rather than a failure -- the permanent is already on the
-- battlefield and stays there, which is precisely what those rules say.
--
-- Combat.defender being Nothing is the fourth way, and it is CR 506.3c's clause
-- again rather than a fallback: outside the combat phase there is no defending
-- player at all (see Pawl.Types.Combat's defender field), so there is nobody for
-- the creature to be attacking.
--
-- CR 508.4's CHOICE is not prompted, because there is exactly one candidate: one
-- defending player (CR 506.2 at two seats; CR 802's attack-multiple-players
-- option is unavailable, #175), no planeswalkers (#301) and no battles (#302).
-- Hanweir Garrison's own ruling is what makes the choice real once any of those
-- lands -- "You choose which player, planeswalker, or battle each token is
-- attacking as you create the tokens ... the tokens don't both have to attack the
-- same one" -- so this becomes a per-token prompt then (#367).
--
-- CR 508.4d ("a creature that's put onto the battlefield attacking during the
-- declare blockers step, combat damage step, or end of combat step enters the
-- battlefield as an unblocked creature") is not implemented: every source in the
-- pool enters during the declare attackers step, before blockers exist (#368).
putOntoBattlefieldAttacking :: ObjectId -> Game ()
putOntoBattlefieldAttacking oid = do
  gs <- State.get
  let c = GameState.combat gs
  case (Combat.defender c, Projection.controllerOf oid gs) of
    (Just defender, Just controller)
      | Set.member oid (GameState.battlefield gs),
        -- CR 506.3a
        isCreatureObject oid gs,
        -- CR 506.3b / CR 506.2: the attacking player is the active player
        controller == GameState.activePlayer gs,
        -- CR 506.3c / CR 508.4a
        List.elem defender (Game.stillPlaying gs) ->
          State.put
            gs
              { GameState.combat =
                  c
                    { Combat.attackers = Map.insert oid (AttackTarget.OfPlayer defender) (Combat.attackers c),
                      -- CR 506.4's comparand, for the same reason declareAttackers
                      -- takes one: this is where the creature joins combat.
                      Combat.joinedUnder = Map.insert oid controller (Combat.joinedUnder c),
                      -- CR 508.8's SECOND clause -- "or put onto the battlefield
                      -- attacking". Set inside the guards, and so here rather
                      -- than in Resolve's Create arm: CR 506.3a-c and CR 508.4a
                      -- each let the permanent enter while it is "never
                      -- considered to be an attacking creature", and one that
                      -- never became an attacker cannot answer this rule.
                      Combat.attackersJoined = True
                    }
              }
    _ -> pure ()

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
        -- Declining to block is NOT always legal: with a CR 509.1c requirement on
        -- the board (Lure), "no blocks" can itself be the illegal answer, so
        -- doing nothing is not a state this can fall back to. It degrades to
        -- forcedBlockDeclaration instead -- always legal, and equal to "no
        -- blocks" whenever no requirement is in force, so this is the same
        -- fallback as before on every board that had one.
        --
        -- The same posture chooseDefender takes for an out-of-candidates answer:
        -- degrade TOTALLY rather than fail, and never re-prompt. It is not the
        -- engine choosing for the player -- the player's answer was taken and
        -- rejected -- and where a requirement leaves exactly one legal
        -- declaration, this hands back the one the rules already forced.
        -- Replay.defaultAnswer's "no blocks" for this prompt routes through here
        -- too, so the two cannot disagree about what an illegal answer becomes.
        gs1 <- State.get
        let declaration = if legalBlockDeclaration pid chosen gs1 then chosen else forcedBlockDeclaration pid gs1
        Monad.unless (Map.null declaration) $ do
          let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
              merged = List.foldl' add (Combat.blockers (GameState.combat gs1)) (Map.toList declaration)
              -- CR 506.4's comparand for the blockers, alongside the attackers'
              -- (declareAttackers). `pid` for the same reason it is there: every
              -- blocker here is one legalBlockers offered, which is
              -- controllerOf == Just pid (CR 509.1a) -- required by
              -- legalBlockDeclaration on the accepted path, and true by
              -- construction on the forcedBlockDeclaration one, whose candidates
              -- come from that same list. Unioned rather than replacing, since
              -- the attackers' entries are already in this map.
              joined = Map.union (Map.fromList (fmap (\b -> (b, pid)) (Map.keys declaration))) (Combat.joinedUnder (GameState.combat gs1))
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged, Combat.joinedUnder = joined}}
