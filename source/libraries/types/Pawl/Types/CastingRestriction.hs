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
    -- during combat" (Mandate of Peace, Angelic Favor) would have to name five
    -- steps at once. Pawl.Types.PhaseSelector is the type that already says it,
    -- and Pawl.Types.ActivationTiming.DuringPhase already carries one; this arm
    -- does not (#527).
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
