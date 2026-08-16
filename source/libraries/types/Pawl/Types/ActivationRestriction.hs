module Pawl.Types.ActivationRestriction where

import qualified Pawl.Types.DuringPhase as DuringPhase

-- | CR 602.5: ONE clause of the "activate only ..." rider an activated ability
-- prints about itself -- CR 602.5b's "a restriction on its use".
--
-- A LIST of them on the ability (Pawl.Types.ActivatedAbility.restrictions), and
-- CR 602.5 -- "A player can't begin to activate an ability that's prohibited from
-- being activated" -- is what makes the list a conjunction: every clause printed
-- must hold. The EMPTY list is CR 602.2's default, which gives an ability no
-- window beyond its controller having priority, so it needs no arm of its own.
--
-- A list rather than one total description of the window because Kongming's
-- Contraptions prints two clauses at once -- "Activate only during the declare
-- attackers step and only if you've been attacked this step" -- and a total
-- description can hold exactly one. That is the same pressure Rally the Troops
-- put on the casting side, which is a list for the same reason.
--
-- A sum type rather than a Bool apiece: no boolean blindness, and the next clause
-- (split second, "only once each turn") is a constructor here rather than a flag
-- on the ability.
--
-- Deliberately NOT a synonym for Pawl.Types.CastingRestriction, which carries two
-- arms spelled the same way. CR 307.5's last two sentences are the reason and
-- they forbid the two READERS agreeing -- Pawl.Engine.Cast must consult casting
-- prohibitions and Pawl.Engine.Activate must not. The two vocabularies do not
-- coincide either: SorcerySpeed below is a clause only an ability can print,
-- since for a spell CR 307.1 IS the window rather than a rider on it.
data ActivationRestriction
  = -- | CR 602.5d's "Activate only as a sorcery" (CR 702.6a's equip is the
    -- pool's producer), and every other ability that carries the same phrase.
    --
    -- CR 307.5 defines it narrowly: the player must have priority, it must be
    -- during the main phase of their turn, and the stack must be empty. Its last
    -- sentence is load-bearing and easy to get wrong -- the check must NOT consult
    -- casting prohibitions (Rule of Law, Silence). Three facts about the game
    -- state and nothing else, and every arm below is held to the same discipline.
    SorcerySpeed
  | -- | CR 500.1: activatable only while the game is in this step or phase, on a
    -- turn the scope admits. Desert's "Activate only during the end of combat
    -- step" (CR 511.1) is EachTurn; Llanowar Augur's "Activate only during your
    -- upkeep" is ControllersTurn.
    --
    -- TWO axes because CR 500.1 supplies only one: it breaks a turn into phases
    -- and steps and says nothing about whose turn it is, which CR 102.1 makes a
    -- separate fact. A rider that names both narrows both, so an arm carrying
    -- only the phase would be strictly more permissive than the card.
    --
    -- A Pawl.Types.PhaseSelector rather than a bare Pawl.Types.Phase, because
    -- CR 500.1 breaks three of the five phases into steps and a Phase value is
    -- one SCHEDULE ENTRY -- so it names a step, or a stepless main phase (CR
    -- 505.2), and can never name the combat phase as a whole. Jade Statue's
    -- "Activate only during combat" is PhaseSelector.CombatPhase.
    -- Pawl.Engine.Turn.inWindow is the reader, and it is a containment test
    -- rather than an equality. The casting side still carries a bare Phase
    -- (#527).
    --
    -- The TurnScope is the same type Pawl.Types.TriggerCondition.StepBegins
    -- carries and means the same thing: CR 109.5 makes a printed "your" name the
    -- ability's controller, which for an activated ability is the player CR 602.2
    -- already restricts activation to.
    --
    -- "An opponent's turn" (Trade Caravan, Nettling Imp) is now sayable --
    -- TurnScope.OpponentsTurn -- but a turn named with no phase at all (Lavinia,
    -- Foil to Conspiracy) still is not: this arm requires a window (#520).
    DuringPhase DuringPhase.DuringPhase
  | -- | "Activate only if you've been attacked this step", asked of the player
    -- activating the ability. Kongming's Contraptions prints it alongside
    -- DuringPhase, which is what made this type a list.
    --
    -- CR 506.2 (CR 507.1 where the seat count makes it a choice) settles who the
    -- defending player is, and CR 508.3b makes "attacked" a fact about a creature
    -- having been DECLARED attacking that player rather than one of their
    -- planeswalkers.
    -- Pawl.Engine.Combat.attackedThisStep is the reader, shared verbatim with
    -- Pawl.Types.CastingRestriction's arm of the same name: the clause is one
    -- question about the combat record, and the two gates differ in what ELSE
    -- they may read rather than in what this asks.
    AttackedThisStep
  | -- | CR 506.7b, through CR 506.7g: "Activate only during combat after
    -- blockers are declared" (Trap Runner). CR 506.7g is what makes this the
    -- same clause Pawl.Types.CastingRestriction prints under this name -- "rules
    -- 506.7 and 506.7a-f apply to abilities ... just as they apply to spells" --
    -- so the two arms share Pawl.Engine.Combat.afterBlockersDeclared verbatim,
    -- as AttackedThisStep above already does.
    --
    -- No TurnScope beside it, unlike DuringPhase: CR 506.7c scopes the window by
    -- COMBAT PHASE and the printed clause names no turn, so a second axis here
    -- would be strictly narrower than the card.
    AfterBlockersDeclared
  deriving (Eq, Ord, Show)
