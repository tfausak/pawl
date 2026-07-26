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

-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps from a
-- schedule, so the turn proceeds "as though they didn't exist". There is one
-- combat phase per turn, so removing every occurrence is unambiguous. With a
-- second combat phase (CR 500.8) this must instead drop only the current phase's
-- steps, positionally (#31).
dropSkippedCombatSteps :: Seq Phase -> Seq Phase
dropSkippedCombatSteps =
  let kept p =
        p /= Phase.Combat CombatStep.DeclareBlockers
          && p /= Phase.Combat CombatStep.CombatDamage
   in Seq.filter kept

-- CR 510.4 / 500.9: a second combat damage step, spliced directly after the
-- current one -- i.e. at the head of the remaining schedule, so it runs next.
-- CR 500.9's "most recently created step occurs first" is exactly cons-at-head.
spliceSecondDamage :: Seq Phase -> Seq Phase
spliceSecondDamage remaining = Phase.Combat CombatStep.CombatDamage Seq.<| remaining
