module Pawl.Types.ReplacementEffect where

import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DrawCountR as DrawCountR
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.LifeGainR as LifeGainR
import qualified Pawl.Types.LifeLossR as LifeLossR
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.UntapRewrite as UntapRewrite
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
-- DestructionR and UntapR carry NO pattern: every producer is self-only, and each
-- for its own rule. CR 201.5 makes a card's reference to itself by name mean just that
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
-- Not implemented: a floating EntryR whose rewrite ADDS COUNTERS -- Zameck
-- Guildmage's "this turn, each creature you control enters with an additional
-- +1/+1 counter on it" (#2886). A floating row aimed at another permanent's entry
-- does exist (Gather Specimens rewrites to UnderSourceControl); every
-- EntryRewrite.WithCounters in the pool rides a permanent's own static ability,
-- read off the entrant.
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
-- Parametric in the EFFECT, which is how the two arms that carry effects reach
-- them without a module cycle: CR 615.5's additional effect on DamageR's riders,
-- and CR 614.1c's "as [this permanent] enters, [do something]" on
-- Pawl.Types.EntryRewrite's RunEffects. Pawl.Types.Effect holds this type and
-- this type holds effects. See Pawl.Types.DamageR. Every other arm ignores the
-- parameter, so a card writing a rider onto anything but a prevention is a lint's
-- job rather than the type's.
--
-- Parametric in the CARD for Pawl.Types.Create's reason: the token TokenR
-- appends (Pawl.Types.TokenR.plus) is card data nested inside card data.
--
-- The sole rules-casing site is Pawl.Engine.Replacement (CR 616.1's loop).
-- Pawl.Codec also cases on every constructor, but only as the JSON data boundary.
data ReplacementEffect card effect
  = ZoneChangeR ZoneChangeR.ZoneChangeR
  | EntryR (EntryR.EntryR effect)
  | DamageR (DamageR.DamageR effect)
  | DestructionR DestructionRewrite.DestructionRewrite
  | CounterR CounterR.CounterR
  | TokenR (TokenR.TokenR card)
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
    -- Pawl.Engine.FaceDown.performTurnFaceUp raises the event between the status write
    -- and the CR 708.7 record.
    TurnUpR TurnUpR.TurnUpR
  | -- | CR 614.1a / 122.1d: "If a permanent with a stun counter on it would
    -- become untapped, instead remove a stun counter from it." A separate arm
    -- from every other because becoming untapped is its own event class: CR
    -- 701.26b's action, which CR 502.3's turn-based action, an Effect.Untap and
    -- CR 107.6's untap symbol in a cost all perform.
    --
    -- Carries NO pattern, for DestructionR's reason exactly: rule 122.1d's
    -- effect is minted onto the permanent whose counters create it, so "a
    -- permanent with a stun counter on it" is the object it was minted for. The
    -- field appears when a card needs it.
    UntapR UntapRewrite.UntapRewrite
  | -- | CR 614.1a / 120.4c: "damage that would reduce your life total to less
    -- than 1 reduces it to 1 instead" (Worship). A separate arm from DamageR
    -- because the two intercept different event classes: CR 120.4b's damage is
    -- already settled and dealt by the time CR 120.4c processes it "into its
    -- results, as modified by replacement effects that interact with those
    -- results (such as life loss or counters)". Worship's own ruling is the
    -- observable difference -- "Worship does not prevent damage. It causes some
    -- damage to be unable to lower your life total. So any damage rendered
    -- useless by Worship was still dealt" -- so a lifelink source still gains
    -- its controller every point it dealt.
    --
    -- The COUNTER half of that same rule already runs through CR 122.6's funnel
    -- (Pawl.Engine.Damage.applyDamage's counterResults); this is the life-loss
    -- half of it, and the pattern's cause field is what keeps the arm off CR
    -- 119.3's other roads.
    LifeLossR LifeLossR.LifeLossR
  | -- | CR 614.1a / 119.10: "if you would gain life, you gain twice that much
    -- life instead" (Boon Reflection). A separate arm from LifeLossR because CR
    -- 119.3's two directions are two event classes: rule 119.10 reads a gain
    -- clause as "if a source would cause you to gain life", and no life loss is
    -- a life gain however the amounts are signed.
    --
    -- Its rewrite sum is smaller than LifeLossR's for the same reason its
    -- pattern is barer: the printed gain clauses resize and nothing more.
    LifeGainR LifeGainR.LifeGainR
  | -- | CR 614.11 / 121.6: "the next time you would draw a card this turn, you
    -- gain 5 life instead" (Words of Worship). A separate arm from ZoneChangeR
    -- because CR 121.5 parts the two: a card an effect moves from a library to a
    -- hand without the word "draw" has not been drawn, so a row watching that
    -- move is a different claim from one watching the draw.
    --
    -- Carries no "next", for PhaseR's reason: CR 614.3's used-up count is
    -- Uses.Once on the carrier, which is what lets a one-shot shield and an
    -- unbounded one share this constructor.
    DrawR DrawR.DrawR
  | -- | CR 121.2a / 614.1a: "if an opponent would draw two or more cards, instead
    -- you and that player each draw a card" (Alms Collector). A separate arm from
    -- DrawR because rule 121.2a parts the two event classes: this one watches the
    -- INSTRUCTION and the number it names, DrawR watches one of the individual
    -- draws CR 121.2 breaks that instruction into. CR 616.1g orders them -- the
    -- instruction is settled before any draw inside it -- so a board carrying
    -- both rows applies each once.
    DrawCountR DrawCountR.DrawCountR
  | PhaseR PhasePattern.PhasePattern
  deriving (Eq, Ord, Show)
