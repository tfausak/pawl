module Pawl.Types.CastingRestriction where

import qualified Pawl.Types.Phase as Phase

-- | CR 601.3: "A player can begin to cast a spell only if a rule or effect allows
-- that player to cast it and no rule or effect prohibits that player from casting
-- it." This type is the PROHIBITION half of that sentence, printed on a card
-- about itself -- Rally the Troops' "Cast this spell only during the declare
-- attackers step and only if you've been attacked this step."
--
-- The exact mirror of Pawl.Types.CastingPermission, and deliberately not a member
-- of it: a permission ALLOWS a cast the rules would refuse (Panglacial Wurm from a
-- library, flashback from a graveyard), and a restriction WITHHOLDS one the rules
-- would otherwise allow. CR 601.3 is one sentence with two independent halves, and
-- collapsing them into one list would leave Pawl.Engine.Cast unable to say which half an
-- arm belongs to.
--
-- A LIST of these on a card, ALL of which must hold: the printed templates join
-- their clauses with "and only if", and the rule's "no ... prohibits" is a
-- conjunction over every prohibition in force.
--
-- Open-half card data, classified rather than identified: Pawl.Engine.Cast is the only
-- module that may case on it, exactly as it is the only reader of
-- CastingPermission. Casing here is casing on a RESTRICTION's classification, not
-- on an effect's identity.
data CastingRestriction
  = -- | CR 500.1: castable only while the game is in this step or phase. Rally the
    -- Troops' "only during the declare attackers step".
    --
    -- Pawl.Types.Phase is one type over the CR 500.1 phases and their steps, so
    -- naming a step and naming a STEPLESS phase (the two main phases) are the same
    -- act here. A phase that HAS steps is not nameable -- "Cast this spell only
    -- during combat" would have to name five steps at once.
    -- Pawl.Types.PhaseSelector is the type that already says it, and
    -- Pawl.Types.ActivationTiming.DuringPhase already carries one; this arm does
    -- not (#527).
    --
    -- The change is small; the CARD is the obstacle, and it is worth writing
    -- down so the next reader does not re-run the search. Scryfall's
    -- `oracle:/^Cast this spell only during combat.$/` returns FIVE cards
    -- (checked 2026-08-02), and every one drags in machinery pawl lacks --
    -- three DIFFERENT pieces of it, not one:
    --
    --   * Mandate of Peace needs an end-the-combat-phase effect. (Its other
    --     clause, "your opponents can't cast spells this turn", is already
    --     PlayerEffect.CantCastSpells.)
    --   * Angelic Favor needs a CONDITIONAL alternative cost that taps another
    --     creature. Card.alternativeCosts exists, but a Cost is mana plus
    --     components with no condition, and CostComponent's only tap arm is
    --     TapThis.
    --   * Cauldron Dance and Surprise Deployment both need an effect that puts
    --     a chosen card from a HAND onto the battlefield. Cauldron Dance's
    --     first arm is not the obstacle: "return target creature card from your
    --     graveyard to the battlefield ... return IT to your hand at the
    --     beginning of the next end step" is the Meandering Towershell shape,
    --     which Resolve.definedSlots covers.
    --   * Spinal Embrace is the one whose delayed ability really does name a
    --     TARGET slot ("sacrifice it"), which CardSpec's delayed-ability lint
    --     rejects: a delayed ability may read only the trigger source or a slot
    --     an effect minted, and a spell mode's target slot is neither.
    --
    -- The narrower cards ("only during combat before blockers are declared",
    -- Blaze of Glory and eight others) do not want this arm at all: their
    -- window is not a phase.
    --
    -- WHOSE turn is a second axis this arm does not carry: "only during an
    -- opponent's upkeep" (Festival) and "only during your end step" (Necrologia)
    -- narrow the same window by turn as well. Pawl.Types.TurnScope is the type that
    -- would say it; no card in the pool needs it yet (#445).
    DuringPhase Phase.Phase
  | -- | "and only if you've been attacked this step" -- the second clause fourteen
    -- cards carry on a CAST, Rally the Troops among them. (Kongming's
    -- Contraptions prints the same words on an activated ability, which is the
    -- ability-side type's problem and not this one's -- #456.)
    --
    -- Not a timing window at all, which is why it is its own arm rather than a
    -- field on DuringPhase: it is a question about what the combat record already
    -- holds (CR 506.2's defending player, CR 508.8's "creatures have joined this
    -- combat as attackers"), asked of the CASTING player.
    --
    -- Eightfold Maze's ruling is the interpretation pinned here: "To cast it, a
    -- creature needs to have attacked _you_" -- being attacked, not merely a
    -- combat happening.
    AttackedThisStep
  deriving (Eq, Ord, Show)
