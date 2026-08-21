-- CR 602.5: the reader of Pawl.Types.ActivationRestriction -- does every clause
-- of an ability's printed "activate only ..." rider permit activating it right
-- now?
--
-- ITS OWN module, below Pawl.Engine.Cost, because CR 605.3a's windows are served
-- from there: Pawl.Engine.Activate refuses a mana ability outright (CR 605.3b),
-- so a rider printed on one is asked at Cost.manaActivations instead, and that
-- module cannot import Pawl.Engine.Activate. Every window an arm reads is
-- Pawl.Engine.Turn's or Pawl.Engine.Event's for the same reason:
-- Pawl.Engine.Combat imports Pawl.Engine.Cost, so the two combat-record readers
-- CR 506.7g and CR 508.3b share with the casting side moved down to
-- Pawl.Engine.Turn, beside the other two windows these arms ask about.
--
-- The only module that may CASE on Pawl.Types.ActivationRestriction, exactly as
-- Pawl.Engine.CombatRestriction is of the type it names. Casing on the arms is a
-- classification, not an effect's identity.
module Pawl.Engine.ActivationRestriction where

import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.DuringPhase as DuringPhase
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.PlayerId (PlayerId)

-- CR 602.5: does every clause of this ability's printed "activate only ..."
-- rider permit activating it right now?
--
-- ALL of them, which is what CR 602.5's "prohibited from being activated" means:
-- one clause failing is a prohibition in force, and a card printing two joins
-- them with "and". The empty list leaves the ability whatever window the rules
-- already give it -- CR 602.2's priority, CR 605.3a's two for a mana ability --
-- and passes vacuously.
--
-- Priority is not re-checked here. Pawl.Engine.Activate's caller is
-- Action.legalActions, which the priority loop asks only of the player who has
-- priority; Cost.manaActivations' two windows are CR 605.3a's, which is a
-- PERMISSION to activate outside priority rather than a fourth conjunct.
--
-- CASTING PROHIBITIONS ARE NOT CONSULTED, by any clause. CR 307.5 says so for the
-- sorcery-speed rider, and no rule extends Pawl.Engine.Cast's CR 601.3 list to
-- an activation either, since CR 601.3 is about beginning to CAST a spell. That
-- is why this reads the game state directly rather than reaching for
-- Pawl.Types.CastingRestriction, whose arms are spelled the same way and answer
-- a different question.
--
-- This gate makes the ability un-OFFERED, and Engine.priorityLoop is what makes
-- that binding: it rejects an action the interpreter was not offered (#219). On
-- the mana path it also makes the source unpayable, Cost.manaActivations being
-- asked at both of CR 605.3a's windows.
restrictionsOk :: PlayerId -> [ActivationRestriction.ActivationRestriction] -> GameState -> Bool
restrictionsOk pid restrictions gs = all (restrictionMet pid gs) restrictions

-- Does the game state satisfy this one printed clause?
restrictionMet :: PlayerId -> GameState -> ActivationRestriction.ActivationRestriction -> Bool
restrictionMet pid gs restriction = case restriction of
  -- CR 307.5's three conjuncts, and Turn.sorcerySpeedWindow is that window shared
  -- with CR 307.1's casting gate: two rules, the same three facts, one copy.
  --
  -- CR 307.5's empty-stack conjunct is the one window that can change under a
  -- caster between the offer and the payment, since CR 601.2a puts the spell on
  -- the stack in between. Not implemented: the cast gate agreeing with the
  -- payment for this arm on the mana path (#2005). Vacuous for data/cards/,
  -- where no mana ability prints this rider.
  ActivationRestriction.SorcerySpeed -> Turn.sorcerySpeedWindow pid gs
  -- CR 500.1's phases and steps: Turn.inWindow asks whether GameState.phase
  -- falls inside the window the rider names. CONTAINMENT rather than equality,
  -- because a rider may name a phase that has steps -- Jade Statue's "only
  -- during combat" is live in all five of CR 506.1's combat steps, while
  -- Desert's names one of them. Pawl.Engine.Cast reads the same
  -- Pawl.Types.DuringPhase bundle off CastingRestriction.DuringPhase;
  -- deliberately duplicated rather than shared, since the two gates differ in
  -- what else they may read.
  --
  -- CR 102.1 supplies the second conjunct, a genuinely separate fact: Desert's
  -- rider names no turn (EachTurn, and the step alone decides), while Llanowar
  -- Augur's "only during your upkeep" names alice's upkeep and not bob's. CR
  -- 109.5 is why `pid` answers "your" -- for an activated ability that is the
  -- player who activated it, which Activate.activatable has already pinned to
  -- activatorOf and which CR 109.4a pins to the permanent's controller for a
  -- mana ability -- so a stolen permanent's rider follows the thief.
  ActivationRestriction.DuringPhase (DuringPhase.MkDuringPhase window scope) ->
    Turn.inWindow window (GameState.phase gs)
      && Event.turnScopeAdmits scope (GameState.activePlayer gs) pid
  -- CR 102.1 alone, with no window beside it: the rider names a turn and every
  -- phase and step of that turn is inside it. The same second conjunct
  -- DuringPhase reads above, standing on its own -- so this is not DuringPhase
  -- with a wildcard window but the absence of that axis.
  ActivationRestriction.DuringTurn scope ->
    Event.turnScopeAdmits scope (GameState.activePlayer gs) pid
  -- CR 508.3b's question, asked of the ACTIVATING player, and the same reader the
  -- casting side's clause of this name uses -- see Turn.attackedThisStep for
  -- why it is the declaration record and not Combat.attacked.
  ActivationRestriction.AttackedThisStep -> Turn.attackedThisStep pid gs
  -- CR 506.7b through CR 506.7g, which says an "activate only" rider naming one
  -- of CR 506.7's points is governed exactly as the same words on a cast are --
  -- so this is Turn.afterBlockersDeclared verbatim, the reader Pawl.Engine.Cast's arm
  -- of this name uses. Trap Runner is the card.
  ActivationRestriction.AfterBlockersDeclared -> Turn.afterBlockersDeclared gs
