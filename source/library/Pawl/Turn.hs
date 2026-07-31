module Pawl.Turn where

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
-- schedule refills to this; `firstPhase` is its current step. Demoted from the
-- old `next` walk: nothing computes a successor any more -- Engine.advance pops
-- this sequence instead.
laterPhases :: Seq Phase
laterPhases = Seq.fromList (drop 1 allPhases)

grantsPriority :: Phase -> Bool
grantsPriority phase = case phase of
  Phase.Beginning BeginningStep.Untap -> False
  Phase.Ending EndingStep.Cleanup -> False
  _ -> True

-- CR 307.5: the "as a sorcery" window. "It means only that the player must have
-- priority, it must be during the main phase of their turn, and the stack must
-- be empty."
--
-- ONE predicate, because two rules need the same three conjuncts and a drifting
-- second copy is exactly what the CR-citation discipline exists to prevent: CR
-- 307.1 gates casting a sorcery (Cast.castableSpells) and CR 307.5 gates an
-- ability that says "Activate only as a sorcery" (Activate.timingOk).
--
-- Priority is NOT among the conjuncts here. Both callers are reached only from
-- Action.legalActions, which the priority loop asks solely of the player who
-- holds it, so re-deriving it would be answering a question the caller has
-- already answered.
--
-- Deliberately nothing else. CR 307.5's last sentence: "Effects that would
-- preclude that player from casting a sorcery spell don't affect the player's
-- capability to perform that action" -- so no prohibition (Rule of Law, Silence)
-- may be consulted here.
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
-- ends the phase. CR 511.3 names combat's ("After the end of combat step ends,
-- the combat phase is over"); CR 501.1 lists the beginning phase's three steps,
-- of which draw is the last; CR 512.1 lists the ending phase's two, of which
-- cleanup is the last. A main phase has NO steps at all (CR 505.2), so there is
-- no step whose end ends it -- which is what Nothing says here.
lastStepOf :: Phase -> Maybe Phase
lastStepOf phase = case phase of
  Phase.Beginning _ -> Just (Phase.Beginning BeginningStep.DrawStep)
  Phase.PrecombatMain -> Nothing
  Phase.Combat _ -> Just (Phase.Combat CombatStep.EndOfCombat)
  Phase.PostcombatMain -> Nothing
  Phase.Ending _ -> Just (Phase.Ending EndingStep.Cleanup)

-- Split what is left of the turn into THIS phase's remaining steps and
-- everything after it. The one place that answers where a phase ends, so
-- CR 511.3 is cited once rather than once per caller.
--
-- The final step goes in the PREFIX: it belongs to the phase it ends.
--
-- The bound is that final step rather than "the leading run of steps of the same
-- kind", because CR 500.8 permits a combat phase directly after a combat phase
-- -- Aurelia, the Warleader's "after this phase, there is an additional combat
-- phase", added from within one -- and the run would swallow both.
--
-- An absent final step yields an EMPTY prefix, not the whole schedule: if this
-- phase's last step is no longer scheduled then the phase is already over as far
-- as `remaining` shows, and "directly after this phase" (CR 500.8) is the head.
-- Unreachable from every caller -- Combat.skipEmptyCombat runs as the declare
-- attackers step ends, Resolve's splice runs while the resolving object's phase
-- is current, and Engine.skipWholePhase runs at the phase's FIRST step -- so no
-- game can observe the choice; it is written this way because dropping nothing
-- is the safer failure than dropping everything.
thisPhase :: Phase -> Seq Phase -> (Seq Phase, Seq Phase)
thisPhase phase remaining = case lastStepOf phase >>= \step -> Seq.elemIndexL step remaining of
  Nothing -> (Seq.empty, remaining)
  Just i -> Seq.splitAt (i + 1) remaining

-- CR 500.11: everything left of the turn AFTER this phase -- what remains once a
-- skipped phase has been proceeded past "as though it didn't exist".
--
-- The other half of `thisPhase`'s split, exactly as dropSkippedCombatSteps below
-- keeps the first half. Positional for the same CR 500.8 reason: skipping THIS
-- combat phase says nothing about a second one added later in the turn.
dropRestOfPhase :: Phase -> Seq Phase -> Seq Phase
dropRestOfPhase phase = snd . thisPhase phase

-- CR 500.1: the whole PHASE that is about to begin, if this step is its first.
-- Nothing for every other step, and for both main phases -- CR 505.2 makes a
-- main phase its own single schedule entry, so PhaseSelector.Step already names
-- it and a second question about it would be the same question twice.
--
-- This is what pins CR 614.10's "once a step, phase, or turn has started, it can
-- no longer be skipped" for a phase rather than a step: Engine.runStep asks the
-- phase question only where this answers Just, so a skip that arrives during a
-- phase cannot take the rest of it.
--
-- CR 501.1 names the beginning phase's first step (untap), CR 506.1 the combat
-- phase's (beginning of combat) and CR 512.1 the ending phase's (end). A CR
-- 500.8 additional combat phase begins at that same step, because
-- expandExtraPhase below builds it from CR 506.1's list.
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

-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps of THE
-- COMBAT PHASE NOW UNDER WAY from what is left of the turn, so it proceeds "as
-- though they didn't exist". Positional, not a filter over the whole schedule:
-- CR 500.8 lets an effect add a second combat phase, and skipping this one says
-- nothing about that one.
--
-- Where this phase ends is `thisPhase`'s question, not this function's. The end
-- of combat step it splits on is never one of the two steps dropped here, so
-- which side of the split it lands on is unobservable.
--
-- The caller is Combat.skipEmptyCombat, which runs as the declare attackers step
-- ends, so the phase is always a combat phase and its end of combat step is
-- always still scheduled.
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

-- CR 500.8: add phases "directly after the specified phase", and the specified
-- phase is always the one the effect resolves in.
--
-- Where that phase ends is `thisPhase`'s question -- which is what makes this
-- correct for an effect resolving inside a STEPPED phase, where the head of
-- `remaining` is still this phase's own later steps rather than the next phase.
-- Aurelia, the Warleader's trigger resolves in the declare attackers step, with
-- this combat phase's declare blockers, combat damage and end of combat steps
-- all still ahead of it.
--
-- The list is inserted as one block, in written order. CR 500.8's "if multiple
-- extra phases are created after the same phase, the most recently created phase
-- will occur first" governs two SEPARATE effects adding phases after the same
-- phase -- which stays true here, since each such effect splices at the same
-- boundary and the later one lands in front of the earlier one's phases. The
-- phases within ONE effect's list are created together, so that clause has
-- nothing to order and they simply run as the card writes them (Full Throttle's
-- "there are two additional combat phases").
splicePhases :: Phase -> [ExtraPhase] -> Seq Phase -> Seq Phase
splicePhases phase extras remaining =
  let (current, rest) = thisPhase phase remaining
   in current <> foldMap expandExtraPhase extras <> rest

-- One whole combat phase followed by one whole main phase -- what Aggravated
-- Assault and Relentless Assault add. Named apart from the splice so a test can
-- say what it expects to be inserted without restating CR 506.1's order.
combatAndMainPhase :: Seq Phase
combatAndMainPhase = foldMap expandExtraPhase [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain]
