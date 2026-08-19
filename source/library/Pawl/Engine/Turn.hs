module Pawl.Engine.Turn where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import Pawl.Types.ExtraPhase (ExtraPhase)
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Phase (Phase)
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import Pawl.Types.PlayerId (PlayerId)

allPhases :: [Phase]
allPhases =
  [ Phase.Beginning BeginningStep.Untap,
    Phase.Beginning BeginningStep.Upkeep,
    Phase.Beginning BeginningStep.DrawStep,
    Phase.PrecombatMain,
    Phase.Combat CombatStep.BeginningOfCombat,
    Phase.Combat CombatStep.DeclareAttackers,
    Phase.Combat CombatStep.DeclareBlockers,
    Phase.Combat CombatStep.CombatDamage,
    Phase.Combat CombatStep.EndOfCombat,
    Phase.PostcombatMain,
    Phase.Ending EndingStep.EndStep,
    Phase.Ending EndingStep.Cleanup
  ]

firstPhase :: Phase
firstPhase = Phase.Beginning BeginningStep.Untap

-- The steps of a fresh turn AFTER its first (the untap step). A new turn's
-- schedule refills to this; `firstPhase` is its current step.
laterPhases :: Seq Phase
laterPhases = Seq.fromList (drop 1 allPhases)

-- Which steps grant a priority round from the PHASE alone.
--
-- CR 502.4 is unqualified, so the untap step's False is the whole answer. CR
-- 514.3's is only the NORMAL case: its CR 514.3a exception is a question about
-- the board, not the phase, and Engine.grantsPriorityNow answers the cleanup
-- step entirely by itself rather than correcting this. The arm stays because a
-- predicate over Phase that reported True for cleanup would be wrong on its own
-- terms.
grantsPriority :: Phase -> Bool
grantsPriority phase = case phase of
  Phase.Beginning BeginningStep.Untap -> False
  Phase.Ending EndingStep.Cleanup -> False
  _ -> True

-- CR 307.5: the "as a sorcery" window. ONE predicate, because three rules need
-- the same conjuncts and a drifting second copy is what the CR-citation
-- discipline exists to prevent: CR 307.1 gates casting a sorcery
-- (Cast.castableSpells), CR 307.5 gates "Activate only as a sorcery"
-- (Activate.restrictionsOk), and CR 305.1 / 116.2a / 505.6b gate playing a land
-- (Action.legalActions). Every one of them names the same moment: a main phase
-- of the player's own turn with the stack empty.
--
-- Priority is NOT among the conjuncts. Every caller is reached only from
-- Action.legalActions, which the priority loop asks solely of the player who
-- holds it. Nothing else belongs either: CR 307.5's last sentence means no
-- prohibition (Rule of Law, Silence) may be consulted here, and CR 305.2's land
-- allowance is the land caller's business rather than this window's -- a tally
-- of what has already happened this turn, not a description of the moment.
sorcerySpeedWindow :: PlayerId -> GameState -> Bool
sorcerySpeedWindow pid gs =
  isMainPhase (GameState.phase gs)
    && GameState.activePlayer gs == pid
    && null (GameState.stack gs)

isMainPhase :: Phase -> Bool
isMainPhase phase = case phase of
  Phase.PrecombatMain -> True
  Phase.PostcombatMain -> True
  _ -> False

-- CR 500: the final step of the phase this one belongs to -- the step whose end
-- ends the phase. CR 511.3 names combat's; CR 501.1 and CR 512.1 list the
-- beginning and ending phases' steps. A main phase has no steps at all (CR
-- 505.2), so there is no step whose end ends it, which is what Nothing says.
lastStepOf :: Phase -> Maybe Phase
lastStepOf phase = case phase of
  Phase.Beginning _ -> Just (Phase.Beginning BeginningStep.DrawStep)
  Phase.PrecombatMain -> Nothing
  Phase.Combat _ -> Just (Phase.Combat CombatStep.EndOfCombat)
  Phase.PostcombatMain -> Nothing
  Phase.Ending _ -> Just (Phase.Ending EndingStep.Cleanup)

-- Split what is left of the turn into THIS phase's remaining steps and
-- everything after it. The one place that answers where a phase ends. The final
-- step goes in the PREFIX: it belongs to the phase it ends.
--
-- The bound is that final step rather than "the leading run of steps of the
-- same kind", because CR 500.8 permits a combat phase directly after a combat
-- phase (Aurelia, the Warleader) and the run would swallow both.
--
-- An absent final step yields an EMPTY prefix, not the whole schedule: the
-- phase is then already over as far as `remaining` shows, and "directly after
-- this phase" (CR 500.8) is the head. Unreachable from every caller, so no game
-- can observe the choice; dropping nothing is the safer failure than dropping
-- everything.
thisPhase :: Phase -> Seq Phase -> (Seq Phase, Seq Phase)
thisPhase phase remaining = case lastStepOf phase >>= \step -> Seq.elemIndexL step remaining of
  Nothing -> (Seq.empty, remaining)
  Just i -> Seq.splitAt (i + 1) remaining

-- CR 500.11: everything left of the turn AFTER this phase. The other half of
-- `thisPhase`'s split. Positional for the same CR 500.8 reason: skipping THIS
-- combat phase says nothing about a second one added later in the turn.
dropRestOfPhase :: Phase -> Seq Phase -> Seq Phase
dropRestOfPhase phase = snd . thisPhase phase

-- CR 500.1: the whole PHASE that is about to begin, if this step is its first.
-- Nothing for every other step, and for both main phases -- CR 505.2 makes a
-- main phase its own single schedule entry, so PhaseSelector.Step already names
-- it.
--
-- This is what pins CR 614.10 for a phase rather than a step: Engine.runStep
-- asks the phase question only where this answers Just, so a skip that arrives
-- during a phase cannot take the rest of it.
--
-- The first steps come from CR 501.1, CR 506.1 and CR 512.1. A CR 500.8
-- additional combat phase begins at the same step, since expandExtraPhase
-- builds it from CR 506.1's list.
phaseBeginningAt :: Phase -> Maybe PhaseSelector
phaseBeginningAt phase = case phase of
  Phase.Beginning BeginningStep.Untap -> Just PhaseSelector.BeginningPhase
  Phase.Beginning _ -> Nothing
  Phase.PrecombatMain -> Nothing
  Phase.Combat CombatStep.BeginningOfCombat -> Just PhaseSelector.CombatPhase
  Phase.Combat _ -> Nothing
  Phase.PostcombatMain -> Nothing
  Phase.Ending EndingStep.EndStep -> Just PhaseSelector.EndingPhase
  Phase.Ending _ -> Nothing

-- CR 500.1: the STEPPED phase this schedule entry belongs to, named as a whole.
-- Nothing for both main phases, because CR 505.2 gives them no steps -- so
-- PhaseSelector.Step already names the phase and there is no coarser window.
wholePhaseOf :: Phase -> Maybe PhaseSelector
wholePhaseOf phase = case phase of
  Phase.Beginning _ -> Just PhaseSelector.BeginningPhase
  Phase.PrecombatMain -> Nothing
  Phase.Combat _ -> Just PhaseSelector.CombatPhase
  Phase.PostcombatMain -> Nothing
  Phase.Ending _ -> Just PhaseSelector.EndingPhase

-- CR 500.1: is the game's current schedule entry INSIDE the window this
-- selector names? A containment test: a selector naming a step matches only
-- that step, while one naming a stepped phase matches every step of it -- Jade
-- Statue is live in all five of CR 506.1's combat steps.
--
-- Not the question Pawl.Engine.Replacement asks of a skip, which compares two
-- selectors by EQUALITY (CR 614.1b) so skipping a phase and skipping a step of
-- it stay distinct events. This compares a selector against the phase the game
-- is IN, where containment is what "during" means.
inWindow :: PhaseSelector -> Phase -> Bool
inWindow selector phase = selector == PhaseSelector.Step phase || wholePhaseOf phase == Just selector

-- CR 500.5: the whole PHASE that is ending, if the step that just ended is its
-- last. Nothing for every other step, and for both main phases -- CR 505.2
-- makes a main phase its own single schedule entry, so PhaseSelector.Step has
-- already named the window that ended.
--
-- The mirror of phaseBeginningAt, and CR 500.5a is why the engine needs both
-- grains: "until end of combat" expires at the end of the combat phase, not at
-- the start of the end of combat step.
--
-- Answers from the step alone, so a phase whose last step never runs --
-- skipped, or dropped with the rest of the phase -- never reports its end
-- (#526).
phaseEndingAt :: Phase -> Maybe PhaseSelector
phaseEndingAt phase = if lastStepOf phase == Just phase then wholePhaseOf phase else Nothing

-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps of the
-- combat phase NOW UNDER WAY from what is left of the turn. Positional, not a
-- filter over the whole schedule: CR 500.8 lets an effect add a second combat
-- phase, and skipping this one says nothing about that one.
--
-- Where this phase ends is `thisPhase`'s question. The end of combat step it
-- splits on is never one of the two steps dropped here, so which side of the
-- split it lands on is unobservable. The one caller, Combat.skipEmptyCombat,
-- runs as the declare attackers step ends, so the phase is always a combat
-- phase and its end of combat step is always still scheduled.
dropSkippedCombatSteps :: Phase -> Seq Phase -> Seq Phase
dropSkippedCombatSteps phase remaining =
  let kept p =
        p /= Phase.Combat CombatStep.DeclareBlockers
          && p /= Phase.Combat CombatStep.CombatDamage
      (current, rest) = thisPhase phase remaining
   in Seq.filter kept current <> rest

-- CR 510.4 / 500.9: a second combat damage step, spliced directly after the
-- current one -- i.e. at the head of the remaining schedule, so it runs next.
-- CR 500.9's "most recently created step occurs first" is exactly cons-at-head.
spliceSecondDamage :: Seq Phase -> Seq Phase
spliceSecondDamage remaining = Phase.Combat CombatStep.CombatDamage Seq.<| remaining

-- CR 514.3a: another cleanup step, spliced directly after the current one -- at
-- the head of the remaining schedule, so it runs next.
--
-- The twin of spliceSecondDamage, deliberately built the same way: a step that
-- repeats itself is one more entry at the front of the turn, not a special case
-- inside Engine.advance. Non-empty `remaining` is handled rather than assumed
-- -- CR 500.8 lets an effect add a phase after the ending phase, and the second
-- cleanup step belongs to the ending phase, so it goes in front of those steps.
spliceExtraCleanup :: Seq Phase -> Seq Phase
spliceExtraCleanup remaining = Phase.Ending EndingStep.Cleanup Seq.<| remaining

-- The steps one added phase expands to. CR 506.1 fixes the combat phase's five
-- and their order; CR 505.2 ("The main phase has no steps") is why a main phase
-- is one element.
expandExtraPhase :: ExtraPhase -> Seq Phase
expandExtraPhase extra = case extra of
  ExtraPhase.ExtraCombat ->
    Seq.fromList
      [ Phase.Combat CombatStep.BeginningOfCombat,
        Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat
      ]
  ExtraPhase.ExtraMain -> Seq.singleton Phase.PostcombatMain

-- CR 500.8: add phases directly after the specified phase, which is always the
-- one the effect resolves in.
--
-- Where that phase ends is `thisPhase`'s question -- which is what makes this
-- correct for an effect resolving inside a STEPPED phase, where the head of
-- `remaining` is still this phase's own later steps (Aurelia, the Warleader).
--
-- The list is inserted as one block, in written order. CR 500.8's "most
-- recently created phase occurs first" governs two SEPARATE effects splicing at
-- the same boundary, which stays true here. Phases within ONE effect's list are
-- created together, so that clause has nothing to order and they run as
-- written.
splicePhases :: Phase -> [ExtraPhase] -> Seq Phase -> Seq Phase
splicePhases phase extras remaining =
  let (current, rest) = thisPhase phase remaining
   in current <> foldMap expandExtraPhase extras <> rest

-- One whole combat phase followed by one whole main phase -- what Aggravated
-- Assault and Relentless Assault add.
combatAndMainPhase :: Seq Phase
combatAndMainPhase = foldMap expandExtraPhase [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain]
