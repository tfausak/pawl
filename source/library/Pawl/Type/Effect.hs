module Pawl.Type.Effect where

import Pawl.Type.AbilityName (AbilityName)
import Pawl.Type.CardCriterion (CardCriterion)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.Duration (Duration)
import Pawl.Type.ManaType (ManaType)
import Pawl.Type.Modification (Modification)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Uses (Uses)
import Pawl.Type.Zone (Zone)

-- The ISA (design.md section 1): first-order, non-recursive (in CONTROL FLOW --
-- no loops, branches, or recursive calls; design.md line 38), no functions in any
-- field. The ONLY module that may case on a constructor is Pawl.Resolve -- the
-- rules core asks classifications, never identities.
--
-- The `card` parameter lets an opcode embed a card's characteristics (a created
-- token, a future copy) WITHOUT a module cycle: Card embeds [Effect Card], so a
-- concrete `Effect Card` reference here would make Card and Effect mutually
-- import each other. Parameterizing instead keeps this module Card-free; Card ties
-- the knot by instantiating `Effect Card` (and likewise the two ability types).
-- The resulting data nesting (a token-maker holds a card that could hold effects)
-- is structural, not a recursive CALL -- resolving a maker never evaluates the
-- embedded card's effects, so the control-flow non-recursion above still holds.
data Effect card
  = DealDamage SlotName Quantity
  | -- CR 611: create a continuous effect on the slot's target for a duration.
    -- Giant Growth and Serpent's Gift are this one opcode, differing only in the
    -- Modification (layer 7c vs 6). Resolve stores it; it never cases on the
    -- Modification.
    ModifyTarget Duration Modification SlotName
  | -- CR 612: rewrite basic-land-type words in the target spell or permanent. The
    -- SlotName is the target slot; the two basic land types are read from the
    -- caster's binding (Binding.subtypes on Object.bindings) and baked into a
    -- stored ChangeSubtypeWord continuous effect. Resolve stores it; Projection
    -- applies it.
    ChangeText SlotName
  | -- CR 605: add one unit of this mana type. Executed by Mana.tapForMana at
    -- payment (CR 605.3b: a mana ability never uses the stack); Resolve.applyEffect
    -- never runs it. Read by Resolve.manaProduced (the "produces mana?" ABI bit).
    AddMana ManaType
  | -- CR 701.23: search the controller's library for a card matching the
    -- criterion, put it onto the battlefield tapped, then shuffle (Evolving
    -- Wilds' exact shape; destination/tapped are baked in for now).
    Search CardCriterion
  | -- CR 701.13 / Rest in Peace: exile every card in every graveyard. Targetless
    -- and bulk (Rest in Peace's exact shape); a general exile-from-zone is future.
    ExileAllGraveyards
  | -- CR 723.1: "you control target player during that player's next turn."
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's exact shape.
    ControlPlayerNextTurn SlotName
  | -- CR 701.8 / 702.12b: destroy the slot's target permanent -- move it to its
    -- owner's graveyard via the changeZone funnel UNLESS it is indestructible.
    -- NOT MoveToZone slot Graveyard: the indestructible check is why this is its
    -- own opcode (Murder vs Darksteel Myr). Indestructible aside, the destruction
    -- is itself interceptable: Pawl.Event.destroy offers a WouldBeDestroyed event
    -- to the CR 616.1 replacement loop before the graveyard move, which is how a
    -- regeneration shield (CR 701.19a) intercepts it.
    Destroy SlotName
  | -- CR 701.21/701.21a: the slot's target permanent is sacrificed -- its
    -- CONTROLLER moves it to its OWNER's graveyard. NOT a destruction: CR 701.21a
    -- says so explicitly, so this consults neither indestructible (CR 702.12b) nor
    -- a regeneration shield (CR 701.19a), and is therefore not a reuse of Destroy.
    --
    -- One opcode, not a targetless SacrificeSelf plus a slotted variant: "this
    -- creature" is expressible because Engine.placeOne binds the trigger's
    -- SOURCE into the reserved Pawl.Binding.triggerSource slot, and "this
    -- creature" recurs far too often to pay for a second opcode.
    Sacrifice SlotName
  | -- CR 400.7: move the slot's target object to a zone through the changeZone
    -- funnel. Bounce = MoveToZone slot Hand (owner-relative -- changeZone carries
    -- Object.owner); targeted exile = MoveToZone slot Exile. The destination is
    -- data; one opcode for every targeted single-object move. Distinct from
    -- Destroy (unconditional move, no indestructible check).
    MoveToZone SlotName Zone
  | -- CR 120: the controller draws this many cards. Targetless (a spell's
    -- controller draws, CR 120.2). Empty-library draw is a loss (CR 121.3),
    -- unlike Mill -- the semantic asymmetry that keeps Draw and Mill separate.
    Draw Quantity
  | -- CR 701.17: the slot's target player mills this many (top N of their library
    -- to their graveyard). Milling a short/empty library mills fewer, no penalty
    -- (CR 701.17b) -- unlike Draw, which loses on empty.
    Mill SlotName Quantity
  | -- CR 701.9: the slot's target player discards this many. The DISCARDING player
    -- chooses which (CR 701.9b) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. A hand smaller than the count discards all of it (CR
    -- 609.3, "does only as much as possible"), forced -- so it is not prompted.
    Discard SlotName Quantity
  | -- CR 111: create this many tokens with the given effect-defined characteristics
    -- (CR 111.3). The `card` is the token's "text", embedded literally in the card
    -- data (a nested card, tied to Card by Card's own instantiation; the codec and
    -- round-trip cover it). Quantity is how many (reused from M4a as
    -- Draw/Mill/Discard do); Create (Literal 2) mints two distinct objects.
    -- Targetless and unprompted -- creating a token is never a choice. Executed by
    -- Resolve.applyEffect via Event.createTokens. NOT a copy-token (CR 707) and NOT a
    -- predefined token (CR 111.10): given, not derived.
    --
    -- The Maybe SlotName BINDS the minted token into the resolving object's LIVE
    -- Object.bindings, so a delayed ability armed by this same resolution (CR
    -- 603.7c's "it") -- which re-reads that live state when ArmDelayedTrigger
    -- captures it -- can name the token. It does NOT make the token visible to a
    -- LATER EFFECT in the same resolution: applyEffect's `chosen` map is computed
    -- once before the effect fold begins, so a later Sacrifice/Destroy/etc. in the
    -- same list still reads the pre-Create snapshot. A DEFINITION, not a read: it
    -- is not a target and never appears in targetSpecs. Defined only for a
    -- single-token create; a Create that binds a slot while making several tokens
    -- is rejected by the Pawl.CardSpec lint family rather than guessed at (#53).
    Create Quantity card (Maybe SlotName)
  | -- CR 614.3 / 615.3: install a floating replacement effect for a duration, with
    -- a use count. Fog is
    -- `Replace UntilEndOfTurn Unlimited (DamageR (MkDamagePattern (Just Combat)) PreventAll)`;
    -- Drudge Skeletons' ability is
    -- `Replace UntilEndOfTurn Once (DestructionR Regenerate)`.
    --
    -- ONE opcode for both, where M3f/M4d had two separate opcodes -- a `Prevent`
    -- and a one-shot self-regenerate -- because the difference between a Fog and
    -- a regeneration shield is which event class the payload names, which is
    -- data. Targetless (a floating replacement watches a CLASS of events, not a
    -- chosen object) and unprompted. Resolve stores it into GameState.replacements
    -- with this effect's SOURCE (CR 113.7) and a fresh timestamp; Pawl.Replacement
    -- applies it.
    Replace Duration Uses ReplacementEffect
  | -- CR 701.6: counter the slot's target spell -- remove it from the stack and
    -- put it into its owner's graveyard (CR 701.6a) via the Event.counter funnel,
    -- so it does not resolve. Distinct from MoveToZone slot Graveyard the way
    -- Destroy is (M4b): Counter is a keyword action on rule 701's list, and this is
    -- the future home of "can't be countered" and a distinct "was countered" event.
    Counter SlotName
  | -- CR 122.6: put this many counters of this kind on the slot's target permanent.
    -- Battlegrowth = PutCounters PlusOnePlusOne (Literal 1) slot; Instill Infection
    -- = PutCounters MinusOneMinusOne (Literal 1) slot. A counter is persistent
    -- object state, NOT a zone change -- Resolve.applyEffect edits Object.counters
    -- in place (Map.insertWith (+)), never through Event.changeZone. Quantity is how
    -- many (reused from M4a; a future X-counter card rides ChooseX). The counter's
    -- P/T effect is applied by the projection (CR 122.1a / 613.4c), not here.
    PutCounters CounterKind Quantity SlotName
  | -- CR 701.26b: untap the slot's target permanent. Single-target (Act of
    -- Treason's "untap that creature"); mass/conditional untap is future.
    Untap SlotName
  | -- CR 613.1b / 611.2c: install a layer-2 control effect on the slot's target
    -- for a duration. The new controller is THIS effect's source's controller
    -- (the `controller` passed to applyEffect), baked into a stored
    -- SetController continuous effect -- derived, never chosen. Also re-Sicks the
    -- target (CR 302.6: the new controller has not controlled it continuously).
    -- Act of Treason's control clause. NOT a reuse of ModifyTarget, whose
    -- Modification is static card data and cannot carry a resolution-time
    -- PlayerId. Permanent control (CR 613), distinct from Mindslaver's
    -- player-control (CR 723, ControlPlayerNextTurn).
    GainControl Duration SlotName
  | -- CR 603.7: create a delayed triggered ability -- the one this card declares
    -- under this name (Card.delayedAbilities). First-order: the payload is card
    -- data joined by a name, so this opcode carries no nested ability and adds no
    -- type parameter. The resolving object's binding environment is captured as
    -- the ability is armed, which is how "it" / "that card" (CR 603.7c) is
    -- remembered after this resolution ends.
    ArmDelayedTrigger AbilityName
  deriving (Eq, Ord, Show)
