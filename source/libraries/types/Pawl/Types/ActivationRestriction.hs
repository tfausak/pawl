module Pawl.Types.ActivationRestriction where

import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 602.5: ONE clause of the "activate only ..." rider an activated ability
-- prints about itself -- CR 602.5b's "a restriction on its use".
--
-- A LIST of them on the ability (Pawl.Types.ActivatedAbility.restrictions), and
-- CR 602.5 -- "A player can't begin to activate an ability that's prohibited from
-- being activated" -- is what makes the list a conjunction: every clause printed
-- must hold. The EMPTY list is the default, which leaves the ability whatever
-- window the rules already give it -- CR 602.2's priority for one on the stack,
-- CR 605.3a's two for a mana ability -- so it needs no arm of its own.
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
-- prohibitions and Pawl.Engine.ActivationRestriction must not. The two
-- vocabularies do not coincide either: SorcerySpeed below is a clause only an ability can print,
-- since for a spell CR 307.1 IS the window rather than a rider on it.
data ActivationRestriction
  = -- | CR 602.5d's "Activate only as a sorcery" (CR 702.6a's equip mints it,
    -- and Grinning Ignus prints the phrase itself), and every other ability that
    -- carries the same phrase.
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
    -- rather than an equality. Pawl.Types.CastingRestriction.DuringPhase now
    -- carries this same bundle, so both sides read one window vocabulary.
    --
    -- The TurnScope is the same type Pawl.Types.TriggerCondition.StepBegins
    -- carries and means the same thing: CR 109.5 makes a printed "your" name the
    -- ability's controller, which for an activated ability is the player CR 602.2
    -- already restricts activation to.
    --
    -- "An opponent's turn" (Trade Caravan, Nettling Imp) is sayable here --
    -- TurnScope.OpponentsTurn -- beside the window this arm requires. A rider
    -- that names a turn and NO window is DuringTurn below.
    DuringPhase DuringPhase.DuringPhase
  | -- | CR 102.1 alone: activatable only on a turn the scope admits, in any phase
    -- or step of it. Lavinia, Foil to Conspiracy prints the whole rider as
    -- "Activate only during an opponent's turn"; Disrupting Scepter and
    -- Gwendlyn Di Corci print the ControllersTurn half.
    --
    -- A SIBLING ARM rather than a Maybe PhaseSelector inside DuringPhase. CR
    -- 500.1's windows and CR 102.1's turn are separate facts, and an arm meaning
    -- "no window at all" is a different thing from one naming a step: an
    -- optional field makes every reader of DuringPhase rule out an absence that
    -- Pawl.Types.DuringPhase deliberately has no form for on its other axis
    -- either.
    --
    -- Not expressible as DuringPhase over a "whole turn" PhaseSelector: CR 500.1
    -- breaks the turn into five phases and a PhaseSelector value names one
    -- schedule entry, so the turn as a whole is outside that type's vocabulary
    -- by construction.
    DuringTurn TurnScope.TurnScope
  | -- | "Activate only if you've been attacked this step", asked of the player
    -- activating the ability. Kongming's Contraptions prints it alongside
    -- DuringPhase, which is what made this type a list.
    --
    -- CR 506.2 (CR 507.1 where the seat count makes it a choice) settles who the
    -- defending player is, and CR 508.3b makes "attacked" a fact about a creature
    -- having been DECLARED attacking that player rather than one of their
    -- planeswalkers.
    -- Pawl.Engine.Turn.attackedThisStep is the reader, shared verbatim with
    -- Pawl.Types.CastingRestriction's arm of the same name: the clause is one
    -- question about the combat record, and the two gates differ in what ELSE
    -- they may read rather than in what this asks.
    AttackedThisStep
  | -- | CR 506.7b, through CR 506.7g: "Activate only during combat after
    -- blockers are declared" (Trap Runner). CR 506.7g is what makes this the
    -- same clause Pawl.Types.CastingRestriction prints under this name -- "rules
    -- 506.7 and 506.7a-f apply to abilities ... just as they apply to spells" --
    -- so the two arms share Pawl.Engine.Turn.afterBlockersDeclared verbatim,
    -- as AttackedThisStep above already does.
    --
    -- No TurnScope beside it, unlike DuringPhase: the printed clause names no
    -- turn, and CR 506.7c gives it every combat phase of one rather than a
    -- chosen player's, so a second axis here would be strictly narrower than the
    -- card.
    AfterBlockersDeclared
  | -- | CR 506.7, through CR 506.7g: "Activate only during combat before combat
    -- damage has been dealt" (Save Point). CR 506.7c is the "during combat" half
    -- -- a turn with two combat phases admits the ability in either -- and the
    -- rest of CR 506.7 the "before the combat damage step" half.
    -- Pawl.Engine.Turn.beforeCombatDamage is the reader.
    --
    -- NOT DuringPhase over PhaseSelector.CombatPhase, which would be strictly
    -- more permissive than the card: Turn.inWindow is a containment test, so that
    -- window also admits the combat damage step (CR 510.3 gives priority only
    -- after CR 510.2's turn-based action, so damage HAS been dealt there) and the
    -- end of combat step. Nor the negation of AfterBlockersDeclared above, which
    -- would exclude the declare blockers step the card admits.
    BeforeCombatDamage
  deriving (Eq, Ord, Show)
