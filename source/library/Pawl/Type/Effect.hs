module Pawl.Type.Effect where

import Pawl.Type.AbilityName (AbilityName)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.Duration (Duration)
import Pawl.Type.Filter (Filter)
import Pawl.Type.ManaProduction (ManaProduction)
import Pawl.Type.Modification (Modification)
import Pawl.Type.MonarchTarget (MonarchTarget)
import Pawl.Type.PlayerCounterKind (PlayerCounterKind)
import Pawl.Type.PlayerEffect (PlayerEffect)
import Pawl.Type.PlayerRef (PlayerRef)
import Pawl.Type.PlayerScope (PlayerScope)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.Regenerability (Regenerability)
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
  | -- CR 605: add one unit of mana, of the type the ManaProduction names -- one
    -- fixed type, or one colour its controller chooses (CR 105.4). Executed by
    -- Mana.tapForMana at payment (CR 605.3b: a mana ability never uses the stack);
    -- Resolve.applyEffect never runs it. Read by Resolve.manaProduced (the
    -- "produces mana?" ABI bit).
    AddMana ManaProduction
  | -- CR 701.23: search the controller's library for a card matching the Filter,
    -- put it onto the battlefield tapped, then shuffle (Evolving Wilds' exact
    -- shape; destination/tapped are baked in for now). The Filter is evaluated
    -- over the PRINTED-card view (Projection.viewOfCard) -- a card in a library
    -- has no projection. Evolving Wilds' "basic land card" (CR 701.23a / 205.4c)
    -- is `And [HasCardType Land, HasSupertype Basic]`.
    Search Filter
  | -- CR 701.13 / Rest in Peace: exile every card in every graveyard. Targetless
    -- and bulk (Rest in Peace's exact shape); a general exile-from-zone is future.
    ExileAllGraveyards
  | -- CR 727.1/727.1a: restart the game. Targetless and game-wide (the
    -- ExileAllGraveyards / BecomeMonarch shape); the starting player of the new
    -- game is the resolving controller, so no target slot is needed.
    RestartGame
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
    -- The Regenerability is CR 701.19c's "It can't be regenerated" rider, carried
    -- by the destruction rather than looked up on the victim -- Terror has it and
    -- the state-based action of CR 704.5g does not, for the same creature.
    Destroy SlotName Regenerability
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
  | -- CR 701.3 / 702.6a: attach THIS permanent (the effect's source) to the
    -- slot's target. "Equip [cost]" means "[Cost]: Attach this permanent to
    -- target creature you control", so the equipment is the source and the only
    -- slot is what it attaches TO -- the opcode carries one slot, not two.
    --
    -- CR 701.3a: if the source is already attached to something else, attaching
    -- it here moves it, and CR 701.3c restamps it. CR 701.3b: if it cannot legally
    -- be attached to the target it does not move at all, and attaching it to what
    -- it already holds does nothing.
    Attach SlotName
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
  | -- CR 122 / 107.14: the players the PlayerRef names each get N counters of a
    -- player-counter kind. Subsumes any self-scoped player counter (energy,
    -- experience, rad) as `Relative You` -- Longtusk Cub's "you get {E}{E}" --
    -- without a new opcode.
    --
    -- The PlayerRef is what lets a player OTHER than the resolving controller
    -- receive them: CR 702.70a's "that player gets N poison counters" is
    -- `InSlot Binding.triggerPlayer`, reading the player the trigger's own event
    -- named. PlayerRef, not PlayerScope, for the reason PlayerRef's own comment
    -- gives -- only PlayerRef can name a binding slot.
    --
    -- Still targetless in itself: a slot this reads may have been filled by
    -- TARGETING (CR 601.2c), which is how a future "target player gets two
    -- poison counters" is written, but nothing here demands it (#120).
    GainPlayerCounters PlayerRef PlayerCounterKind Quantity
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
  | -- CR 611.1 / 613.11: install a stored PLAYER or RULES-modifying continuous
    -- effect on a class of players for a duration. Silence is
    -- `AffectPlayers UntilEndOfTurn Opponents CantCastSpells`.
    --
    -- Targetless, mirroring Replace: a rules-modifying effect watches a CLASS,
    -- not a chosen object, so there is nothing to target and nothing to prompt.
    -- Resolve stores it into GameState.playerEffects with this effect's source,
    -- its controller (CR 109.5, baked in -- the source may be in a graveyard by
    -- the time anyone asks), a fresh timestamp, and Expiry.arm's answer.
    AffectPlayers Duration PlayerScope PlayerEffect
  | -- CR 114.2: "[you] get an emblem with [abilities]." Puts an emblem owned and
    -- controlled by the resolving controller into the command zone. Targetless
    -- (the beneficiary is always the resolving controller); the abilities ride a
    -- Card so the emblem reuses the whole ability pipeline. First-order: a data
    -- Card, tied to Card by Card's own instantiation, exactly as Create's is.
    CreateEmblem card
  | -- CR 725: a player becomes the monarch. Targetless; the beneficiary is named
    -- by the MonarchTarget (the resolving controller, or the controller of the
    -- ability's bound source). Setting the monarch emits GameEvent.BecameMonarch.
    BecomeMonarch MonarchTarget
  | -- CR 725 (Palace Jailer): exile the slot's target UNTIL an opponent of the
    -- effect's controller becomes the monarch. The exile is the usual targeted
    -- move; the DURATION is the novelty -- the exiled incarnation is registered in
    -- GameState.exiledUntilMonarch and returned by Pawl.Monarch's settle-loop
    -- sweep. NOT MoveToZone: that has no duration and schedules no return.
    ExileUntilMonarch SlotName
  | -- CR 729.1/729.1b: play a Magic subgame, then bind its outcome (the derived
    -- loser) into this slot for a later effect to read. This slot is DEFINED here
    -- (like Create's minted-token slot), not a cast-time target -- the loser is
    -- determined only when the subgame ends, so the following effect reads it
    -- through the per-effect binding re-read in resolveSpellWith. Generic: the
    -- engine reaches subgames through this opcode, never Shahrazad's identity.
    PlaySubgame SlotName
  | -- CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many cards. Targetless and controller-scoped, the
    -- ExileAllGraveyards / Draw shape.
    --
    -- ONE opcode rather than an exile composed with a Draw: "that many" is the
    -- hand size BEFORE the exile, so a following Draw would read a hand that is
    -- already empty. Splitting it needs a Count that reads a value produced
    -- earlier in the same resolution, which nothing else wants.
    --
    -- The card granting the action is itself in the hand and is exiled with the
    -- rest: CR 103.5b's action is not a cost, and nothing sets it aside.
    ExileHandThenDraw
  deriving (Eq, Ord, Show)
