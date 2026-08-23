module Pawl.Types.CastingRestriction where

import qualified Pawl.Types.DuringPhase as DuringPhase

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
  = -- | CR 500.1: castable only while the game is in the window the rider names,
    -- on a turn its scope admits. Rally the Troops' "only during the declare
    -- attackers step" (any player's, so TurnScope.EachTurn) and Necrologia's
    -- "only during your end step" (TurnScope.ControllersTurn).
    --
    -- The same Pawl.Types.DuringPhase bundle Pawl.Types.ActivationRestriction's
    -- arm of this name carries, and for the same reasons: a
    -- Pawl.Types.PhaseSelector rather than a bare Pawl.Types.Phase, so a phase
    -- that HAS steps is nameable without naming five steps at once, and a
    -- Pawl.Types.TurnScope beside it, since CR 109.5's "your" narrows the window
    -- by turn independently of which window it is.
    --
    -- The PhaseSelector.CombatPhase window's producer is Mandate of Peace, whose
    -- CR 724.2 clause is Pawl.Types.Effect's EndCombatPhase; Pawl.TurnSpec's
    -- "EndTheCombatPhase" group asserts both directions of the rider on one
    -- board, and Pawl.Engine.Activate's Jade Statue cases keep the same
    -- Pawl.Engine.Turn.inWindow containment honest on the activation side. The
    -- other cards printing "Cast this spell only during combat" (checked against
    -- Scryfall 2026-08-16) each drag in machinery pawl lacks: a CONDITIONAL
    -- alternative cost that taps another creature (Angelic Favor), an effect
    -- putting a chosen card from a HAND onto the battlefield (Cauldron Dance,
    -- Surprise Deployment), or a delayed ability naming a spell mode's TARGET
    -- slot, which CardSpec's delayed-ability lint rejects (Spinal Embrace). The
    -- narrower cards ("only
    -- during combat before blockers are declared", Blaze of Glory and eight
    -- others) do not want this arm at all: their window is not a phase.
    DuringPhase DuringPhase.DuringPhase
  | -- | "and only if you've been attacked this step" -- the second clause every
    -- printed instant carrying these words puts on a CAST, Rally the Troops
    -- among them (Scryfall `o:"been attacked this step"`, 2026-08-21: fourteen
    -- instants, and all fourteen also print "only during the declare attackers
    -- step"). (Kongming's Contraptions prints the same words on an activated
    -- ability, where the arm of this name lives on
    -- Pawl.Types.ActivationRestriction; the two share
    -- Pawl.Engine.Turn.attackedThisStep as their reader.)
    --
    -- data/cards/synthetic-belated-rally.json is the one card without the
    -- step clause beside it, which is what lets a test tell CR 508.6's step from
    -- the combat phase.
    --
    -- Not a timing window at all, which is why it is its own arm rather than a
    -- field on DuringPhase: it is a question about what the combat record already
    -- holds (CR 506.2's defending player, CR 508.1k's attacking creatures),
    -- asked of the CASTING player.
    --
    -- Eightfold Maze's ruling pins the interpretation: a creature needs to have
    -- attacked YOU, not merely a combat to have happened.
    AttackedThisStep
  | -- | CR 506.7b: "Cast this spell only during combat after blockers are
    -- declared" (Curtain of Light). Neither a step nor a phase, so DuringPhase
    -- above cannot say it and neither could a Pawl.Types.PhaseSelector: the
    -- window opens as the declare blockers step begins and runs to the end of
    -- the combat phase, spanning that step, the combat damage step and the end
    -- of combat step.
    --
    -- The mirror of Pawl.Types.ActivationRestriction's arm of this name, sharing
    -- Pawl.Engine.Turn.afterBlockersDeclared as its reader -- which is CR
    -- 506.7g, the rule that says CR 506.7's points govern an activation exactly
    -- as they govern a cast.
    --
    -- NULLARY, rather than a before/after pair over the six points CR 506.7
    -- enumerates. The pool prints one of them, and the vocabulary grows at the
    -- card that needs the next: "only during combat before blockers are
    -- declared" (Blaze of Glory) is a different window with no producer here.
    --
    -- Two clauses of CR 506.7 ride along rather than needing gates of their own,
    -- because the reader asks about the combat record instead of about the phase
    -- the game is in. CR 506.7f: a combat phase whose declare blockers step is
    -- skipped admits the spell nowhere in that phase. CR 506.7c: the printed
    -- "during combat" gives every combat phase of the turn its own window rather
    -- than only the first (CR 506.7d).
    AfterBlockersDeclared
  deriving (Eq, Ord, Show)
