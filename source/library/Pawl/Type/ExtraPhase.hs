module Pawl.Type.ExtraPhase where

-- CR 500.8: one phase an effect adds to a turn. The rule does not fix WHICH
-- phases are added -- "Some effects can add phases to a turn. They do this by
-- adding the phases directly after the specified phase" -- and printed cards
-- vary: Aggravated Assault and Relentless Assault add a combat phase and a main
-- phase, Aurelia, the Warleader adds only a combat phase, and Full Throttle adds
-- two combat phases and no main phase.
--
-- A whole PHASE, never a step: what Pawl.Turn.expandExtraPhase inserts for
-- ExtraCombat is CR 506.1's five steps in order, and for ExtraMain is the single
-- stepless main phase CR 505.2 describes. So the effect's payload says what the
-- card says, and the rules detail of what a phase is made of stays in Pawl.Turn.
data ExtraPhase
  = -- CR 506.1's five steps: beginning of combat, declare attackers, declare
    -- blockers, combat damage, end of combat.
    ExtraCombat
  | -- CR 505.1a: an added main phase is a POSTCOMBAT main phase. "Only the first
    -- main phase of the turn is a precombat main phase. All other main phases
    -- are postcombat main phases ... It is also true of a turn in which an
    -- effect has caused an additional combat phase and an additional main phase
    -- to be created."
    ExtraMain
  deriving (Eq, Ord, Show)
