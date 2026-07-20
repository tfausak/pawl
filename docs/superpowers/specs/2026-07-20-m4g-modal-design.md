# M4g modal — design

Design for milestone **M4g**, the seventh and final letter of M4 in the split
table (`docs/design.md` §3): **modal — a choice at cast binds which effects and
targets apply.** Every spell so far has one effect list and one target namespace,
both flat on the `Card`. A modal spell has several *modes*, each its own bundle of
effects and targets, and the caster picks which apply as the spell is announced
(CR 700.2). M4g proves the engine can carry that choice through casting,
castability, targeting, and resolution — reading only the chosen mode's targets,
never the others'.

Gate card, Scryfall-verified
(`api.scryfall.com/cards/named?exact=Chaos+Charm`, fetched 2026-07-20):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Chaos Charm** | `{R}` | Instant | "Choose one —<br>• Destroy target Wall.<br>• Chaos Charm deals 1 damage to target creature.<br>• Target creature gains haste until end of turn." |

**Why Chaos Charm and not a "cleaner" charm.** Two of its three modes are already
built — `DealDamage` (M3a) and `ModifyTarget (GainKeyword Haste)` (M3b, the exact
Serpent's-Gift machinery). It is **mono-color** (`{R}`), so unlike a shard charm it
fits an existing random-game deck rather than being deterministic-only. And its one
restricted mode — *destroy target **Wall*** — is the feature, not a blemish: it is
what makes the gate's falsifier bite.

**The falsifier — CR 700.2c / 601.2c.** "Its controller will need to choose those
targets only if they chose that mode. Otherwise, the spell or ability is treated as
though it did not have those targets." The Wall mode's legal target set (creatures
that are Walls) is a strict subset of the two creature modes' (all creatures). On a
board holding a non-Wall creature and no Wall, a naive engine that computes every
mode's targets at cast — the M3a `Cast.targetable` posture, which requires **all**
of a card's slots to be fillable — wrongly finds Chaos Charm uncastable (no legal
Wall) and never offers it. The correct engine casts it via the damage or haste
mode and never asks for a Wall. An unrestricted charm (every mode "target creature")
could **not** witness this: if all modes share one legal set, the naive and correct
answers coincide and there is nothing to falsify. The Wall restriction is precisely
the asymmetry that separates them.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea

**Modality is a property of the (effects, targetSpecs) payload, not of the `Card`.**
The rules say so directly — CR 700.2: "A spell **or ability** is modal." That payload
appears in three places today, all parametric over `card` (M4c's knot): `Card`
(a spell), `ActivatedAbility` (`{cost, effects, targetSpecs}`), and
`TriggeredAbility` (`{condition, effects, targetSpecs}`). So the mode structure is
built **Card-free and parametric**, embeddable in all three — but M4g **wires only
the spell payload** (the `Card`), gating on Chaos Charm. Extending it to activated
and triggered abilities is a **fast-follow** (§13), the rules' own generalization
(CR 700.2a "spell or activated ability", 700.2b modal triggered abilities), and the
same M2c→M2d, M4b-random-follow pattern: prove the axis on one shape, then spread it.

Four moving parts, each on an existing seam:

1. **The shape.** Three new leaf types — `Mode card`, `ModeSelection`, `Modal card`
   — plus a `ModeIndex` newtype. `Card`'s flat `effects`/`targetSpecs` become one
   `spell :: Modal Card`. A non-modal card is one mode with `ChooseExactly 1`
   (forced, unprompted): behaviorally identical to today.
2. **The choice's home.** `Object.bindings` gains the chosen modes under a reserved
   slot, the `Binding.variableX` pattern (per-incarnation, replay-serialized).
3. **The prompt.** `ChooseModes` at cast, CR 601.2b (before targets, 601.2c),
   pre-filtered to the *legal* modes (CR 700.2a) and unasked when forced.
4. **Reading only the chosen mode.** Castability, targeting, resolution, and the CR
   608.2b fizzle all consult the chosen modes' target slots, never the whole card's.

The §1 invariant holds throughout: `Mode`/`Modal`/`ModeSelection`/`ModeIndex` are
first-order structural data (no functions, no control flow); the mode choice is a
genuine player choice, prompted (never elided except where forced); `Pawl.Resolve`
stays the sole `case effect of` home; nothing cases on a card's identity. Chaos
Charm is a printing whose `spell` is a three-mode `Modal`; the rules core learns
only that it is modal and how many modes are legal, never that it is Chaos Charm.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **The gate (CR 700.2 / 601.2b), each mode.** A player casts Chaos Charm and
  chooses the **damage** mode targeting a creature → 1 damage marked; the **haste**
  mode targeting a summoning-sick creature → that creature gains `Haste` (projected)
  and may attack the turn it came under control; the **Wall** mode targeting a Wall
  → the Wall is destroyed.
- **The falsifier (CR 700.2c / 601.2c).** On a board with a non-Wall creature and
  **no Wall**, Chaos Charm is castable — `ChooseModes` offers exactly the two
  fillable modes (damage, haste), the Wall mode is not offered, and no Wall target
  is ever required. The M3a all-slots-fillable engine would reject the spell here;
  this test is that rejection made observable.
- **Only the chosen mode's targets (CR 601.2c).** Casting the damage mode prompts
  exactly one **creature** target and never a Wall target; the other modes' slots
  are absent from the binding environment.
- **The chosen mode's fizzle (CR 608.2b).** A Chaos Charm cast for the damage mode
  whose sole target leaves before resolution fizzles — re-validation scoped to the
  chosen mode's slot, the M3a machinery, not the whole card's.
- **Forced choices ask nothing.** A non-modal spell (Lightning Bolt) still prompts
  no mode; a modal spell with exactly one legal mode auto-chooses it (no prompt) —
  the don't-prompt-where-nothing-is-open rule.
- **Round-trip and replay.** `chaos-charm.json` and the Wall fixture survive the
  `allPrintings` honesty round-trip (`jsonToCard . cardToJson ≡ Right`) through new
  `Codec` arms; a `DecisionLog` carrying `ChoseModes` replays deterministically.

**Out of scope (named deferred expiries, §13):** modality on **activated and
triggered abilities** (the fast-follow — CR 700.2a/700.2b); every `ModeSelection`
beyond `ChooseExactly` — "choose two"/commands, "choose one or more"/escalate with
its per-mode additional cost (CR 700.2h), "you may choose the same mode more than
once" (CR 700.2d), pawprint "worth of modes" (CR 700.2i), and a player *other than*
the controller choosing (CR 700.2e); a copy copying its source's modes (CR 700.2g);
the multi-mode slot-name collision (only reachable once a selection picks ≥2 modes);
and a general subtype-restricted target beyond `WallTarget`.

## 1. The mode types

Four new leaf modules, all parametric over `card` where they carry effects (the
M4c posture — a concrete `Effect Card` would cycle with `Card`, which embeds the
payload; `Card` ties the knot at `Modal Card`).

**`Pawl.Type.ModeIndex`** — the reference to a mode:

```
module Pawl.Type.ModeIndex where

import Numeric.Natural (Natural)

-- CR 700.2: a mode is one option in the printed bulleted list. Modes have no
-- meaningful label to conjure (unlike a target SlotName), so a mode is referenced
-- by its ORDINAL -- and the ordinal is load-bearing, not incidental: CR 608.2c
-- resolves modes in printed order, CR 700.2d treats a mode chosen twice as
-- "appear[ing] that many times in sequence," CR 700.2g copies modes by position.
-- A newtype, not a bare Natural, so the reference is typed. Ord is load-bearing:
-- ModeIndex is a Set element (the chosen modes) and its ordering IS printed order.
newtype ModeIndex = MkModeIndex Natural
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Mode`** — one option's payload, the shape lifted verbatim from the
flat `Card`/`ActivatedAbility`/`TriggeredAbility` fields:

```
module Pawl.Type.Mode where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- CR 700.2: one mode of a modal spell or ability -- its own effects and its own
-- target namespace. `effects` is a Seq (ordered; CR 608.2c resolves in written
-- order; duplicates allowed -- two DealDamage). `targetSpecs` is per-mode: CR
-- 601.2c/700.2c fill only the CHOSEN mode's slots. Parametric in `card` like
-- Effect. A non-modal payload is a single Mode.
data Mode card = MkMode
  { effects :: Seq (Effect card),
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.ModeSelection`** — how many modes to choose:

```
module Pawl.Type.ModeSelection where

import Numeric.Natural (Natural)

-- CR 700.2: the instruction preceding the bulleted list ("Choose one —"). A sum,
-- not a bare Natural, so it grows without boolean/primitive blindness. Only
-- ChooseExactly exists at M4g: n = 1 for a charm AND for every non-modal card (one
-- mode, forced). "Choose two"/commands are ChooseExactly 2; "choose one or more"
-- (escalate), pawprint "worth of modes" (700.2i), and "you may choose the same mode
-- more than once" (700.2d) are future constructors (§13).
data ModeSelection
  = ChooseExactly Natural
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Modal`** — the payload as a whole:

```
module Pawl.Type.Modal where

import Data.Sequence (Seq)
import Pawl.Type.Mode (Mode)
import Pawl.Type.ModeSelection (ModeSelection)

-- CR 700.2: a spell's or ability's modal payload. `modes` is a Seq -- ordered
-- (printed order, indexed by ModeIndex) and NON-EMPTY by invariant (a payload has
-- at least one mode; the codec rejects an empty modes array as a decode error, the
-- UnsafeX/textToX posture -- there is no type-level NonEmpty Seq in base). A
-- non-modal payload is `MkModal (mode :<| Empty) (ChooseExactly 1)`.
data Modal card = MkModal
  { modes :: Seq (Mode card),
    selection :: ModeSelection
  }
  deriving (Eq, Ord, Show)
```

`Data.Sequence` is a `containers` module — already a dependency; no new package.
`Mode.effects` is `Seq`, honoring the avoid-lists directive; the untouched
`ActivatedAbility.effects`/`TriggeredAbility.effects` stay `[Effect card]` this
milestone, a **documented interim divergence** the fast-follow (§13) removes when it
migrates those types to `Modal` (and can flip their effects to `Seq` in the same
stroke).

## 2. The `Card` change

`Pawl.Type.Card` drops `effects :: [Effect Card]` and `targetSpecs :: Map SlotName
TargetSpec` and gains one field:

```
-- The card's spell payload as data: what casting this card does when it resolves,
-- as one or more modes (CR 700.2). A non-modal card -- every card before M4g -- is
-- a single mode with ChooseExactly 1 (forced, unprompted). A land or vanilla
-- creature is a single EMPTY mode (no spell effects; resolution just enters the
-- battlefield). Card ties Modal's `card` knot at `Modal Card`.
spell :: Modal Card
```

Every one of the ~40 committed `data/cards/*.json` re-nests its `effects` and
`targetSpecs` under a `spell` object of one mode — the churn the universal shape
buys (the alternative, an overlay `modes :: [Mode Card]` beside the flat fields,
was rejected: two homes for effects, and it leaves the ability types unable to
share the shape). The re-nest is mechanical and the round-trip (§11) is the safety
net: a mis-nested file fails to decode or fails byte-stability.

`Card` no longer imports `Effect`/`TargetSpec` directly for these (it imports
`Modal`); it still imports them for `activatedAbilities`/`triggeredAbilities`, which
are unchanged.

## 3. The chosen modes — `Binding.modes`

The mode choice is a spell-level cast choice, exactly like X: it belongs in
`Object.bindings` under a reserved slot, per-incarnation (reset by `changeZone`, CR
400.7 — the M3a/M4a posture) and replay-serialized.

`Pawl.Type.Binding` gains one field (its own comment already anticipates "a mode"):

```
-- CR 700.2 / 601.2b: the modes chosen for a modal spell, by index. A Set: no
-- duplicate modes (CR 700.2d "same mode more than once" -- a multiset -- is future),
-- and Set's ordering IS printed order (CR 608.2c), so resolution reads them
-- pre-sorted for free. Stored under the reserved Binding.chosenModes slot, never a
-- target slot. Nothing for a slot that is not the reserved mode slot.
modes :: Maybe (Set ModeIndex)
```

`Binding.empty` extends with `modes = Nothing`; `mergeBinding` gains a
`modes = Binding.modes a <|> Binding.modes b` arm (the reserved slot is disjoint
from target slots, so the merge stays total and order-independent).

`Pawl.Binding` (the logic module) gains, mirroring `variableX`/`amountOf`:

```
-- CR 700.2: the reserved slot under which a modal spell's chosen modes are stored.
-- No card's targetSpecs may name it (the D4 lint enforces this): a mode is not a
-- target. Distinct from variableX.
chosenModes :: SlotName
chosenModes = SlotName.MkSlotName (Text.pack "modes")

-- The modes chosen for a spell, read from its binding environment. Empty when
-- absent (defensive; cast always stamps it, forced or prompted).
modesOf :: Map SlotName Binding -> Set ModeIndex
modesOf m = maybe Set.empty id (Binding.modes =<< Map.lookup chosenModes m)
```

`fromChoices` grows a `Set ModeIndex` parameter and inserts it under `chosenModes`
alongside the existing X insert (both reserved-slot writes).

## 4. The prompt and response

`Pawl.Type.Prompt` gains one constructor:

```
-- CR 601.2b / 700.2a: choose the mode(s) while casting (the ObjectId is the spell).
-- The Set ModeIndex is the LEGAL modes -- the engine pre-filters to modes whose
-- targets are all fillable (CR 700.2a: an illegal mode can't be chosen), the
-- ChooseTargets/SearchLibrary pre-filter posture. The Natural is how many to choose
-- (from the ModeSelection). The answer is the chosen subset. Prompted before X and
-- targets (CR 601.2b precedes 601.2c), and ONLY when there is a real choice
-- (#legal > count); a forced selection is not asked.
ChooseModes :: Decider -> PlayerId -> ObjectId -> Set ModeIndex -> Natural -> Prompt (Set ModeIndex)
```

`Pawl.Type.Response` gains `ChoseModes (Set ModeIndex)` (serialized as an array of
naturals, so a `DecisionLog` replays a modal cast deterministically — the `ChoseX`
posture).

## 5. The cast flow

`Cast.castSpell` interleaves modes into the CR 601.2b announcement, which the code
already walks (modes → X → text-change subtypes → targets). CR 601.2b lists modes
**first**; targets are 601.2c. So the new order is:

1. **Enumerate legal modes.** For each mode of the spell's `Modal`, compute whether
   it is *fillable*: every slot in that mode's `targetSpecs` has a non-empty legal
   set (`Target.legalSets` over the mode's specs), or the mode has no slots. Legal
   modes = the fillable ones (CR 700.2a). (Castability, §6, has already guaranteed
   at least `count` legal modes exist, so this set is large enough.)
2. **Choose modes.** Let `count` be the `ChooseExactly` number. If
   `Set.size legal > count`, prompt `ChooseModes decider pid oid legal count`;
   validate the answer is a size-`count` subset of `legal` (reject-not-repair, the
   whole cast a no-op otherwise). If `Set.size legal == count` (or the spell is
   non-modal: one mode), the selection is **forced** — take `legal` unprompted.
3. **Choose X** (unchanged, only if the cost carries a `Variable`).
4. **Choose text-change subtypes** (unchanged) — over the **chosen modes'** slots.
5. **Choose targets (CR 601.2c).** Compute `Target.legalSets` over the **union of
   the chosen modes' `targetSpecs`** only, and prompt `ChooseTargets` for those
   slots (CR 700.2c: unchosen modes contribute no targets).
6. **Stamp bindings.** `Binding.fromChoices` records the chosen targets, subtypes,
   X, **and the chosen `Set ModeIndex`** (under `chosenModes`) on the new stack
   incarnation.

For a non-modal card the chosen set is `{MkModeIndex 0}`, forced and unprompted, and
steps 4–5 range over the single mode's slots — identical behavior to today.

## 6. Castability — mode-aware

`Cast.targetable` (consumed by `castable`, hence `Action.legalActions`) is the M3a
CR 601.2c gate — today "every slot has a legal recipient." It becomes:

> **A modal spell is castable when at least `count` of its modes are fillable.**

For `ChooseExactly 1`: at least one mode fillable. For a non-modal card (one mode,
count 1): that mode fillable — exactly today's "all slots fillable" (a single mode's
slots *are* all the card's slots). A mode with no slots is trivially fillable, so a
charm with a targetless mode is always castable — the general M4g posture, though
Chaos Charm has no such mode (all three need a creature).

The `<count`-legal-modes case (a "choose two" with only one fillable mode) is a
`ChooseExactly n>1` concern and deferred (§13); at M4g `count` is always 1.

## 7. Resolution and the fizzle

`Resolve.effectsOf` — today `map (rewriteEffect …) (Card.effects card)` — becomes:
read the object's chosen modes (`Binding.modesOf` over its `bindings`), look each up
in the spell's `modes` `Seq`, and concatenate their `effects` in `ModeIndex` order
(the `Set` is already sorted → printed order, CR 608.2c), then rewrite each
(text-changing, M3d, unchanged). A non-modal object resolves its single mode's
effects. An out-of-range index (impossible post-validation) contributes nothing —
total.

The CR 608.2b fizzle (`resolveSpell`) is unchanged in mechanism but **scoped**: it
re-validates the filled slots, which are exactly the chosen modes' slots (the only
ones in `bindings`), against the chosen modes' `targetSpecs`. The re-validation
reads target specs from the chosen modes, not `Card`-flat — so a Chaos Charm cast
for the damage mode fizzles iff *its* creature target is gone, indifferent to the
Wall or haste modes. `Target`'s legality helpers take the relevant `Map SlotName
TargetSpec`; the caller (resolution) now sources it from the chosen modes.

`Pawl.Resolve` remains the sole `case effect of` home. No `Effect` constructor is
added: modes reshape *which* effects run, not *what* an effect is.

## 8. Incidental machinery — the Wall mode

Chaos Charm's three modes, as data on its `Modal`:

| Idx | Effect(s) | `targetSpecs` |
|---|---|---|
| 0 | `Destroy "wall"` (M4b) | `{"wall" ↦ WallTarget}` |
| 1 | `DealDamage "creature" (Literal 1)` (M3a) | `{"creature" ↦ CreatureTarget}` |
| 2 | `ModifyTarget UntilEndOfTurn (GainKeyword Haste) "creature"` (M3b) | `{"creature" ↦ CreatureTarget}` |

Only two additions to the closed half:

- **`Subtype.Wall`** — a new constructor (the M4b `Myr`/`Skeleton` posture).
- **`TargetSpec.WallTarget`** — "target Wall": a creature whose **projected**
  subtypes (M3c) include `Wall`. Read by `Target.legalSets` like `CreatureTarget`
  but intersected with the Wall subtype. Named specifically (the `LandTarget`
  posture); a general subtype-restricted target ("target Goblin") is future (§13).

The haste mode adds nothing: `GainKeyword Haste` is the Serpent's-Gift
`ModifyTarget`, differing only in the `Keyword` (a closed-half citation, not an
effect identity — M2a). `Projection` is untouched; the granted `Haste` folds through
the existing layer-6 path and `keywordsOf`, and summoning-sickness/attack legality
already read it.

**No new `Effect`, no new `Modification`, no new SBA, no projection change.** M4g's
only opcodes-adjacent additions are the modal *structure* and the one `WallTarget`
spec — the rest is Chaos Charm re-using M3a/M3b/M4b verbs under a mode wrapper.

## 9. The D4 lint and slot namespacing

The dataflow lint (test suite) — every effect's referenced slot is declared, and
declared-equals-read — iterates **per mode**: for each `Mode`, its `effects`' slots
⊆ its `targetSpecs` keys, and (for the read-equals-declared half) the mode's
declared slots equal the slots its effects read. The reserved `variableX` and
`chosenModes` slots are exempt (neither is a target). `Resolve.readsX` and
`slotsOf` already range over an effect list; they now range over the union of a
card's modes' effects.

**Slot namespacing.** Each `Mode` owns its `targetSpecs` namespace, and only the
chosen mode's slots enter `bindings`, so modes may reuse a slot name harmlessly
(Chaos Charm's modes 1 and 2 both name their slot `"creature"`; only one is ever
chosen). A collision is reachable only when a single selection picks **≥2 modes**
that share a name — impossible under `ChooseExactly 1`. That case is deferred with
its mitigation named: qualify a chosen slot by its `ModeIndex` when writing bindings
once "choose two" exists (§13).

## 10. The cards, fixtures, and deck

Both render to `data/cards/<slug>.json` (the M3.5 files-are-source-of-truth
pipeline; the round-trip regenerates and re-parses them):

- **Chaos Charm** (`chaos-charm.json`): type line `Instant`; mana cost `{R}`
  (a `Mountain` base); `spell` a three-mode `Modal` with `ChooseExactly 1` and the
  §8 modes.
- **Wall of Stone** (`wall-of-stone.json`), Scryfall-verified `{1}{R}{R}` `Creature
  — Wall` 0/8, `Defender` (a keyword, M2a): the vanilla-plus-Defender Wall fixture
  that gives the destroy-Wall mode a **positive** target and the `WallTarget` legal
  set a member. It joins `allPrintings` (round-trip) but **no deck** — it exists for
  the deterministic Wall tests; random red games need no Wall (the Wall mode simply
  is never legal there, which the falsifier coverage wants anyway).

**Deck/coverage.** Chaos Charm swaps **4-for-4 into the red deck** against the Bird
Maidens (8 → 4), keeping the deck at 60 (so the CR 400.7 conservation counts stay
120) and leaving creatures on board (Pikers 4, Bird Maidens 4) so its damage/haste
modes have legal targets. Random red games thereby exercise the **choose-among-
legal-modes** path (the Wall mode never legal, so `ChooseModes` offers `{1,2}`);
the Wall mode, its positive destroy, and the falsifier are **deterministic
fixtures** (a random game will not reliably produce a Wall-and-non-Wall board).

## 11. Tests

All gameplay-level (cast/resolve through the stack, assert on game state); names
carry their CR numbers.

- **Type-level:** `Binding.modesOf` round-trips a stamped `Set ModeIndex`;
  `Modal`/`Mode` decode rejects an empty `modes` array; `changeZone` yields a new
  incarnation whose `bindings` (hence chosen modes) are empty (CR 400.7).
- **Gate, damage mode (CR 700.2/601.2b):** cast Chaos Charm, choose mode 1, target a
  creature → 1 damage marked; only the `"creature"` slot is bound.
- **Gate, haste mode:** choose mode 2 on a summoning-sick creature → projected
  `keywordsOf` includes `Haste` and it is a legal attacker that turn.
- **Gate, Wall mode:** with a Wall of Stone in play, choose mode 0 targeting it →
  the Wall is destroyed (CR 700.4-clean; it is not indestructible).
- **Falsifier (CR 700.2c / 601.2c):** board = one non-Wall creature, no Wall. Chaos
  Charm is castable; `ChooseModes` offers `{1,2}` (mode 0 absent); no Wall target is
  requested. A comment names the M3a all-slots-fillable engine this rejects.
- **Only chosen mode's targets (CR 601.2c):** casting mode 1 prompts exactly one
  creature target; `bindings` contains no `"wall"` slot.
- **Fizzle scoped to the chosen mode (CR 608.2b):** Chaos Charm cast for mode 1 on a
  creature that leaves before resolution fizzles (to the graveyard, no effect).
- **Forced/no-prompt:** Lightning Bolt (non-modal) prompts no mode; a modal spell
  with exactly one legal mode auto-chooses it (assert the `DecisionLog` records no
  `ChooseModes` prompt).
- **Round-trip:** `chaos-charm.json` and `wall-of-stone.json` via the `allPrintings`
  honesty property and the new `Codec` arms.
- **Replay:** a `DecisionLog` containing `ChoseModes` replays byte-identically.

## 12. Module and dependency notes

- New leaf modules: `Pawl.Type.ModeIndex` (imports `Natural` only),
  `Pawl.Type.ModeSelection` (`Natural` only), `Pawl.Type.Mode` (imports `Effect`,
  `SlotName`, `TargetSpec`, `Seq` — Card-free), `Pawl.Type.Modal` (imports `Mode`,
  `ModeSelection`, `Seq`). No cycles: none imports `Card`; `Card` imports `Modal`
  and ties the knot at `Modal Card`.
- `Pawl.Type.Binding` imports `ModeIndex` and `Set`; `Pawl.Binding` imports
  `ModeIndex`, `Set`, and gains `chosenModes`/`modesOf` and the `fromChoices`
  parameter.
- `Pawl.Type.Prompt`/`Pawl.Type.Response` import `ModeIndex` and `Set`.
- `Pawl.Cast` computes per-mode fillability (already imports `Target`, `Prompt`,
  `Binding`) and prompts `ChooseModes`; `Pawl.Resolve.effectsOf` reads chosen modes
  (imports `Binding`, `Modal`, `Mode`); `Pawl.Target`'s legality helpers are
  unchanged in signature (they already take a `Map SlotName TargetSpec`; the caller
  sources it from the chosen modes).
- `Pawl.Codec` gains arms for `Modal`, `Mode`, `ModeSelection` (all tagged, the
  §2.12 discipline; `ModeIndex` inside `Response.ChoseModes` serializes as a bare
  natural array), `TargetSpec.WallTarget`, and `Subtype.Wall`. `Object`/`Binding`
  are not serialized, so the chosen-mode `Set` needs no `Codec` arm beyond the
  `Response` one.
- `Pawl.Resolve` stays the sole `case effect of` home; `Pawl.Projection` the sole
  `case … Modification` home; `Pawl.Target` the sole targeting-legality home. The
  new mode-selection casing (on `ModeSelection`) lives in `Pawl.Cast` — a
  cast-orchestration classification, not an effect identity.

## 13. Named deferred expiries and fast-follows

- **Fast-follow — modality everywhere (CR 700.2a/700.2b).** Adopt `Modal` in
  `ActivatedAbility` (`{cost, ability :: Modal card}`) and `TriggeredAbility`
  (`{condition, ability :: Modal card}`), wire `ChooseModes` into the activate path
  (CR 602.2b) and the trigger-put-on-stack path (CR 603.3c: "if no mode is chosen,
  the ability is removed"), and migrate those types' `effects` to `Seq`. Gated by a
  real modal activated or triggered ability. This is the design's intent (§0) and
  the immediate next step after M4g lands.
- **`ModeSelection` beyond `ChooseExactly`.** "Choose two"/commands (`ChooseExactly
  2`, needing the `<count` legal-modes semantics and multi-mode slot namespacing);
  "choose one or more"/escalate with its per-mode additional cost (CR 700.2h); "you
  may choose the same mode more than once" (CR 700.2d — the chosen set becomes a
  multiset, so `Binding.modes` grows past `Set`); pawprint "worth of modes" (CR
  700.2i); and a player other than the controller choosing (CR 700.2e). Each is a new
  `ModeSelection` constructor (or field) due with its first card.
- **Copy copies modes (CR 700.2g).** When a copy effect exists, it must carry the
  source's chosen `Set ModeIndex` rather than re-prompt.
- **Multi-mode slot-name collision.** Reachable only once a selection picks ≥2
  modes; mitigation (qualify a bound slot by its `ModeIndex`) lands with the first
  such card.
- **A general subtype-restricted target.** `WallTarget` is specific; "target
  Goblin"/"target Zombie" generalizes to a subtype-parameterized spec when needed.

## 14. Invariant check

- **Closed/open separation.** The rules core reads a *classification* of the payload
  — how many modes are legal, which effects the chosen modes carry — never a card's
  identity. `Pawl.Cast` cases on `ModeSelection` (an orchestration tag, like it
  already branches on timing); `Pawl.Resolve` stays the only module casing on
  `Effect`. There is no `case card of ChaosCharm → …` anywhere. `Mode`/`Modal` are
  the same kind of first-order structural data as `Effect`.
- **First-order, non-recursive DSL.** `Mode`, `Modal`, `ModeSelection`, `ModeIndex`
  carry effects, a count, and an ordinal — no function, no control flow. The
  parametric `card` nests structurally (a mode holds effects that could hold a
  token's card), never a recursive call, exactly as M4c established.
- **The engine makes no choice it shouldn't.** The mode choice is a real player
  choice, prompted through `ChooseModes` — elided only when forced (one legal mode,
  or a non-modal single mode), the CR-sanctioned "nothing to choose" case, with no
  new open-ended elision.
- **Numeric tower / collections.** Indices and counts are `Natural`; the new
  collections are `Seq` (ordered modes/effects) and `Set` (chosen modes), per the
  avoid-lists directive.
