module Pawl.Turn where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.Phase (Phase)
import qualified Pawl.Type.Phase as Phase

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

isMainPhase :: Phase -> Bool
isMainPhase phase = case phase of
  Phase.PrecombatMain -> True
  Phase.PostcombatMain -> True
  _ -> False

-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps from a
-- schedule, so the turn proceeds "as though they didn't exist". There is one
-- combat phase per turn at M2b, so removing every occurrence is unambiguous.
--
-- EXPIRES at M4: with a second combat phase (CR 500.8) this must drop only the
-- current phase's steps, positionally, not every combat step in the schedule.
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
