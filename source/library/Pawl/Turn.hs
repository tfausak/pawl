module Pawl.Turn where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Phase (Phase)
import qualified Pawl.Type.Phase as Phase
import Pawl.Type.PlayerId (PlayerId)

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

-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps of THE
-- COMBAT PHASE NOW UNDER WAY from what is left of the turn, so it proceeds "as
-- though they didn't exist". Positional, not a filter over the whole schedule:
-- CR 500.8 lets an effect add a second combat phase, and skipping this one says
-- nothing about that one.
--
-- What bounds "this phase" is CR 511.3 -- "After the end of combat step ends,
-- the combat phase is over" -- so the current phase is the schedule up to its
-- FIRST end of combat step, and that step and everything past it are left
-- alone. The end of combat step itself is never one of the two being dropped,
-- so which side of the split it lands on is unobservable; Seq.breakl leaves it
-- on the untouched side. That bound rather than "the leading run of combat
-- steps",
-- because CR 500.8 permits a combat phase directly after a combat phase --
-- Aurelia, the Warleader's "after this phase, there is an additional combat
-- phase", added from within one -- and the run would swallow both. No card in
-- the pool can build that arrangement, so nothing tests it (#393).
--
-- The caller is Combat.skipEmptyCombat, which runs as the declare attackers step
-- ends, so the schedule always still holds this phase's end of combat step. If
-- it somehow did not, Seq.breakl yields the whole schedule as "this phase" and
-- the old whole-schedule filter is recovered.
dropSkippedCombatSteps :: Seq Phase -> Seq Phase
dropSkippedCombatSteps remaining =
  let kept p =
        p /= Phase.Combat CombatStep.DeclareBlockers
          && p /= Phase.Combat CombatStep.CombatDamage
      (thisPhase, rest) = Seq.breakl (== Phase.Combat CombatStep.EndOfCombat) remaining
   in Seq.filter kept thisPhase <> rest

-- CR 510.4 / 500.9: a second combat damage step, spliced directly after the
-- current one -- i.e. at the head of the remaining schedule, so it runs next.
-- CR 500.9's "most recently created step occurs first" is exactly cons-at-head.
spliceSecondDamage :: Seq Phase -> Seq Phase
spliceSecondDamage remaining = Phase.Combat CombatStep.CombatDamage Seq.<| remaining

-- CR 500.8: an additional combat phase followed by an additional main phase,
-- added "directly after the specified phase" -- and the specified phase is
-- always the current one, because the only card that says this ("After this main
-- phase ...") can only be activated as a sorcery (CR 307.5), so it resolves in
-- the phase it names. Directly after the current phase IS the head of the
-- remaining schedule, the shape spliceSecondDamage already has, and CR 500.8's
-- "the most recently created phase will occur first" is again cons-at-head.
--
-- The combat phase's steps are CR 506.1's five, in that order. The main phase is
-- PostcombatMain by CR 505.1a: "Only the first main phase of the turn is a
-- precombat main phase. All other main phases are postcombat main phases ... It
-- is also true of a turn in which an effect has caused an additional combat
-- phase and an additional main phase to be created."
spliceCombatAndMainPhase :: Seq Phase -> Seq Phase
spliceCombatAndMainPhase remaining = combatAndMainPhase <> remaining

-- One whole combat phase (CR 506.1) followed by one whole main phase (CR 505.2:
-- "the main phase has no steps"). Named apart from the splice so a test can say
-- what it expects to be inserted without restating CR 506.1's order.
combatAndMainPhase :: Seq Phase
combatAndMainPhase =
  Seq.fromList
    [ Phase.Combat CombatStep.BeginningOfCombat,
      Phase.Combat CombatStep.DeclareAttackers,
      Phase.Combat CombatStep.DeclareBlockers,
      Phase.Combat CombatStep.CombatDamage,
      Phase.Combat CombatStep.EndOfCombat,
      Phase.PostcombatMain
    ]
