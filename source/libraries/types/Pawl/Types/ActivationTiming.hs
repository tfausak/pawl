module Pawl.Types.ActivationTiming where

import Pawl.Types.PhaseSelector (PhaseSelector)
import Pawl.Types.TurnScope (TurnScope)

-- CR 307.5: when an activated ability may be activated.
--
-- A sum type rather than a Bool on the ability: no boolean blindness, and the
-- next rider (flash, "only during your turn", split second) is a constructor
-- here rather than a second flag to keep consistent with the first.
--
-- CR 307.5 defines the restricted case exactly, and narrowly: "it means only
-- that the player must have priority, it must be during the main phase of their
-- turn, and the stack must be empty. The player doesn't need to have a sorcery
-- card they could cast. Effects that would preclude that player from casting a
-- sorcery spell don't affect the player's capability to perform that action."
--
-- That last sentence is load-bearing and easy to get wrong: the check must NOT
-- consult casting prohibitions (Rule of Law, Silence). For SorcerySpeed it is
-- three facts about the game state and nothing else. DuringPhase below is held
-- to the same discipline, and for the same reason -- Pawl.Engine.Activate.timingOk
-- reads GameState.phase and GameState.activePlayer, and never
-- Pawl.Types.CastingRestriction.
--
-- Deliberately NOT a synonym for Pawl.Types.CastingRestriction, which carries an
-- arm spelled the same way. Three reasons, and the first is the one that would
-- survive a card forcing the question:
--
--   1. Different logical role. A CastingRestriction is one of a LIST of
--      prohibitions layered on top of a window the rules already give the spell
--      (CR 117.1a for an instant, CR 302.1/307.1 otherwise), and CR 601.3 makes
--      that list a conjunction. This type is a TOTAL description: CR 602.2 gives
--      an ability no default window beyond "any time you have priority", and
--      AnyTime is that default stated as an arm. One type cannot be both.
--   2. CR 307.5's last two sentences forbid the two readers agreeing --
--      Pawl.Engine.Cast must consult casting prohibitions and Pawl.Engine.Activate must not.
--   3. No ONE card in the pool prints both shapes -- Rally the Troops prints the
--      casting side, Desert the ability side -- so nothing forces the merge.
--
-- ONE rider and never several: an ability whose printed text joins two clauses
-- with "and only if" (Kongming's Contraptions) is unrepresentable (#456).
data ActivationTiming
  = -- No rider: any time its controller has priority (CR 602.2).
    AnyTime
  | -- CR 702.6a's "Activate only as a sorcery", and every other ability that
    -- carries the same phrase.
    SorcerySpeed
  | -- CR 500.1: activatable only while the game is in this step or phase, on a
    -- turn the scope admits. Desert's "Activate only during the end of combat
    -- step" (CR 511.1) is EachTurn; Llanowar Augur's "Activate only during your
    -- upkeep" is ControllersTurn.
    --
    -- TWO axes because CR 500.1 supplies only one. It breaks a turn into phases
    -- and steps and says nothing about whose turn it is, and CR 102.1 -- "The
    -- active player is the player whose turn it is" -- makes that a separate
    -- fact. A rider that names both narrows both, so an arm carrying only the
    -- phase is strictly more permissive than the card. Pawl.ActivateSpec's
    -- PrintedActivationTurnScope group is what proves the two axes are read
    -- independently.
    --
    -- A Pawl.Types.PhaseSelector rather than a bare Pawl.Types.Phase, because
    -- CR 500.1 breaks three of the five phases into steps and a Phase value is
    -- one SCHEDULE ENTRY -- so it names a step, or a stepless main phase (CR
    -- 505.2), and can never name the combat phase as a whole. Jade Statue's
    -- "Activate only during combat" is PhaseSelector.CombatPhase; Desert's
    -- "Activate only during the end of combat step" is PhaseSelector.Step, and
    -- means exactly what it meant when this arm carried the Phase directly.
    -- Pawl.Engine.Turn.inWindow is the reader, and it is a containment test
    -- rather than an equality -- which is the whole difference between the two
    -- kinds of arm. The casting side still carries a bare Phase (#527).
    --
    -- The TurnScope is the same type Pawl.Types.TriggerCondition.StepBegins
    -- carries, and means the same thing on the same axis: CR 109.5 is what makes
    -- a printed "your" name the ability's controller, and for an activated
    -- ability that is "the player who activated the ability" -- the player CR
    -- 602.2 already restricts activation to. Sharing the SCOPE type is not the
    -- same as sharing the timing type, which the paragraphs above argue against.
    --
    -- Neither "an opponent's turn" (Trade Caravan, Nettling Imp) nor a turn named
    -- with no phase at all (Lavinia, Foil to Conspiracy) is sayable: TurnScope has
    -- two arms, and this one requires a window (#520).
    DuringPhase PhaseSelector TurnScope
  deriving (Eq, Ord, Show)
