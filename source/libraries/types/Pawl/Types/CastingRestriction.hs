module Pawl.Types.CastingRestriction where

import qualified Pawl.Types.Phase as Phase

-- | CR 601.3's PROHIBITION half -- a rule or effect that stops a player casting a
-- spell -- printed on a card about itself (Rally the Troops).
--
-- The exact mirror of Pawl.Types.CastingPermission, and deliberately not a member
-- of it: a permission ALLOWS a cast the rules would refuse (Panglacial Wurm from
-- a library, flashback from a graveyard), and a restriction WITHHOLDS one the
-- rules would otherwise allow. Collapsing CR 601.3's two independent halves into
-- one list would leave Pawl.Engine.Cast unable to say which half an arm belongs
-- to.
--
-- A LIST of these on a card, ALL of which must hold: the printed templates join
-- their clauses with "and only if", and the rule's "no ... prohibits" is a
-- conjunction over every prohibition in force.
--
-- Open-half card data, classified rather than identified: Pawl.Engine.Cast is the
-- only module that may case on it. Casing here is casing on a RESTRICTION's
-- classification, not on an effect's identity.
data CastingRestriction
  = -- | CR 500.1: castable only while the game is in this step or phase. Rally the
    -- Troops' "only during the declare attackers step".
    --
    -- Pawl.Types.Phase is one type over the CR 500.1 phases and their steps, so
    -- naming a step and naming a STEPLESS phase (the two main phases) are the same
    -- act here. A phase that HAS steps is not nameable -- "Cast this spell only
    -- during combat" would have to name five steps at once.
    -- Pawl.Types.PhaseSelector is the type that already says it, and
    -- Pawl.Types.ActivationRestriction.DuringPhase already carries one; this arm
    -- does not (#527).
    --
    -- The change is small; the CARD is the obstacle. Every one of the five cards
    -- printing "Cast this spell only during combat" (checked against Scryfall
    -- 2026-08-02) drags in machinery pawl lacks: an end-the-combat-phase effect
    -- (Mandate of Peace), a CONDITIONAL alternative cost that taps another
    -- creature (Angelic Favor), an effect putting a chosen card from a HAND onto
    -- the battlefield (Cauldron Dance, Surprise Deployment), or a delayed ability
    -- naming a spell mode's TARGET slot, which CardSpec's delayed-ability lint
    -- rejects (Spinal Embrace). The narrower cards ("only during combat before
    -- blockers are declared", Blaze of Glory and eight others) do not want this
    -- arm at all: their window is not a phase.
    --
    -- WHOSE turn is a second axis this arm does not carry: "only during an
    -- opponent's upkeep" (Festival) and "only during your end step" (Necrologia)
    -- narrow the same window by turn as well. Pawl.Types.TurnScope is the type that
    -- would say it; no card in the pool needs it yet (#445).
    DuringPhase Phase.Phase
  | -- | "and only if you've been attacked this step" -- the second clause
    -- fourteen cards carry on a CAST, Rally the Troops among them. (Kongming's
    -- Contraptions prints the same words on an activated ability, where the arm
    -- of this name lives on Pawl.Types.ActivationRestriction; the two share
    -- Pawl.Engine.Combat.attackedThisStep as their reader.)
    --
    -- Not a timing window at all, which is why it is its own arm rather than a
    -- field on DuringPhase: it is a question about what the combat record already
    -- holds (CR 506.2's defending player, CR 508.1k's attacking creatures),
    -- asked of the CASTING player.
    --
    -- Eightfold Maze's ruling pins the interpretation: a creature needs to have
    -- attacked YOU, not merely a combat to have happened.
    AttackedThisStep
  deriving (Eq, Ord, Show)
