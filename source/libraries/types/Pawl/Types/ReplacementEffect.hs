module Pawl.Types.ReplacementEffect where

import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- | CR 614.1a: a replacement effect, classified by the EVENT CLASS it intercepts
-- and the REWRITE SHAPE it applies. One arm per replaceable event class -- the
-- arm count tracks the classes the comprehensive rules define, never the card
-- pool. Rest in Peace is DATA, not a constructor; so is Fog, so is regeneration,
-- so is Hardened Scales, and so is rule 702.34a's flashback exile. The scenario
-- the first invariant forbids -- casing on a replacement's identity -- is not
-- expressible.
--
-- An (effect, event) pair whose arms disagree simply does not apply, so the type
-- rules out "redirect a damage event" without a validity pass.
--
-- DestructionR carries NO pattern: both producers are self-only, and each for its
-- own rule. CR 201.5 makes a card's reference to itself by name mean just that
-- object, so "regenerate this creature" (CR 701.19a) names no other; and CR
-- 122.1c's replacement is minted onto the permanent whose counters create it, so
-- "this permanent" is the object it was minted for. The field appears when a card
-- needs it.
--
-- EntryR's pattern is a bare Filter rather than a pattern RECORD, which is CR
-- 614.1c and CR 614.1d collapsing into one field: 614.1c's "as [this permanent]
-- enters" is `Filter.IsSource`, an identity test the generic matcher already
-- answers off its Context, and 614.1d's is an ordinary characteristic filter. CR
-- 109.5's relation rides that Filter too (Filter.ControlledBy), so EntryR needs no
-- ControllerRelation field beside it.
--
-- ZoneChangePattern spells its subject the same way (see that module) and yet
-- KEEPS its relation field, which is not an inconsistency: a zone change is
-- scoped by the moving object's OWNER (CR 400.3), and Filter.ControlledBy asks
-- who controls it -- a different player for anything stolen.
--
-- PhaseR carries a pattern but NO rewrite, which is the rule rather than an
-- omission: CR 614.1b and CR 614.10 make a skip a replacement with NOTHING, so
-- there is nothing for a PhaseRewrite to choose between. (CR 614.10b's "skip,
-- then take another action" is a separate ability alongside the skip, and has no
-- producer.)
--
-- Nor does the pattern say "next": CR 614.10a's once-and-gone skip (Fatigue) is
-- Uses.Once on the ActiveReplacement holding it, which is what lets a permanent's
-- unbounded skip (Eon Hub) and a resolution's single-occurrence one share one
-- constructor.
--
-- Parametric in the EFFECT, which is CR 615.5's additional effect reaching the
-- one arm that can carry one (DamageR's riders, #1105) without a module cycle:
-- Pawl.Types.Effect holds this type and this type holds effects. See
-- Pawl.Types.DamageR. Every other arm ignores the parameter, so a card writing a
-- rider onto anything but a prevention is a lint's job rather than the type's.
--
-- The sole rules-casing site is Pawl.Engine.Replacement (CR 616.1's loop).
-- Pawl.Codec also cases on every constructor, but only as the JSON data boundary.
data ReplacementEffect effect
  = ZoneChangeR ZoneChangeR.ZoneChangeR
  | EntryR EntryR.EntryR
  | DamageR (DamageR.DamageR effect)
  | DestructionR DestructionRewrite.DestructionRewrite
  | CounterR CounterR.CounterR
  | TokenR TokenR.TokenR
  | -- | CR 614.1e: "As [this permanent] is turned face up . . ." A separate arm
    -- from EntryR and not a widening of it, because CR 614.1c's event class and
    -- this one are different events -- a permanent that turns face up does not
    -- enter, and CR 702.37e says so ("any abilities relating to the permanent
    -- entering the battlefield don't trigger when it's turned face up").
    --
    -- The Filter is EntryR's, read the same way: CR 614.1e's printed wording is
    -- always "as THIS permanent is turned face up", so every producer today is
    -- `Filter.IsSource`. The field is a Filter rather than nothing because CR
    -- 614.1a puts no restriction on what a replacement may look at, and
    -- narrowing it here would be this engine's restriction rather than a rule's.
    --
    -- CR 708.11 is what makes the arm reachable at all: the ability belongs to a
    -- permanent that has no abilities while it is face down, so it "is applied
    -- while that permanent is being turned face up, not afterward" --
    -- Pawl.Engine.FaceDown.turnFaceUp raises the event between the status write
    -- and the CR 708.7 record.
    TurnUpR TurnUpR.TurnUpR
  | PhaseR PhasePattern.PhasePattern
  deriving (Eq, Ord, Show)
