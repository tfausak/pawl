# M4h modal abilities — design

Design for milestone **M4h**, the named fast-follow M4g gave itself
(`docs/design.md` §3, `progress.md`'s M4g entry): **modality on activated and
triggered abilities.** M4g proved a choice at cast binds which effects and targets
apply, but wired it only for the spell payload (`Card.spell :: Modal Card`). The
`Mode`/`Modal`/`ModeSelection`/`ModeIndex` types were deliberately built Card-free
and parametric in `card` — exactly like `Effect`/`ActivatedAbility`/
`TriggeredAbility` (M4c) — so the two ability types can adopt the same payload with
no module cycle. M4h cashes that: both ability types carry a `Modal card`, the mode
choice wires into the activation path (CR 602.2b) and the trigger-goes-on-the-stack
path (CR 700.2b / 603.3c / 603.3d), and those types' `effects` migrate to `Seq`
(the interim divergence M4g documented, retired here).

Gate card, Scryfall-verified
(`api.scryfall.com/cards/named?exact=Aether+Channeler`, fetched 2026-07-20):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Aether Channeler** | `{2}{U}` | Creature — Human Wizard | "When this creature enters, choose one —<br>• Create a 1/1 white Bird creature token with flying.<br>• Return another target nonland permanent to its owner's hand.<br>• Draw a card." |

**Why Aether Channeler.** Its three modes are three verbs the engine already has —
`Create` (M4c), `MoveToZone Hand` (M4b's bounce), `Draw` (M4b) — so the milestone
adds **zero opcodes**, exactly as Chaos Charm composed three existing verbs under a
modal *spell* wrapper (M4g). It gates the higher-value path: a modal **triggered**
ability is the first triggered ability that *targets* (no triggered ability targets
today), exercising CR 603.3d target-choice-at-placement on top of CR 700.2b mode
choice. It reuses M3f's `TriggerCondition.SelfEnters` unchanged.

**The gate is a deterministic fixture** (blue, the M3d/M4e posture): it joins
`allPrintings` for the honesty round-trip but no random-game deck. Aether Channeler
proves modality-on-triggers, per-mode targeting at placement, and only-chosen-mode
resolution — but it **cannot** exercise CR 603.3c *removal* (its Create and Draw
modes are always legal, so the trigger is never removed). A survey of Scryfall found
no clean all-targeted modal ETB in the engine's opcode set (every real candidate
needs a new opcode — tap/fight/scry, forbidden this pass — or a color / "dealt
damage this turn" / control model). So CR 603.3c removal is covered by a **labeled
synthetic fixture** (§9), the Wall-of-Stone-fixture posture M4g used for its
deterministic Wall tests.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea

**Modality is a property of the (effects, targetSpecs) payload, and that payload
already has a name: `Modal`.** CR 700.2 says so — "A spell **or ability** is modal"
— and 700.2a/700.2b/603.3c spell it out for activated and triggered abilities. M4g
built `Modal card` parametric precisely so the two ability types could adopt it. So
the reshape is mechanical: the flat `effects`/`targetSpecs` on each ability type
collapse into one `modal :: Modal card`, and the M4g cast-time machinery
(`fillableModes`, `Prompt.ChooseModes`, `Binding.modesOf`/`fromChoices`, the
mode-scoped readers) is threaded into two new sites.

Five moving parts, each on an existing seam:

1. **The reshape.** `ActivatedAbility` and `TriggeredAbility` drop their flat
   `effects`/`targetSpecs` and gain `modal :: Modal card`. Because `Mode.effects`
   is a `Seq`, this retires M4g's documented interim divergence (abilities'
   `effects :: [Effect card]`). A non-modal ability — every ability before M4h —
   is one mode with `ChooseExactly 1` (forced, unprompted), behaviorally identical.
2. **The shared reader.** M4g's mode-scoped projections (`modesEffects`,
   `modesTargetSpecs`, `allEffects`, `allTargetSpecs`), currently specialized to
   `Card.spell` on `Pawl.Card`, generalize into a `Pawl.Modal` logic module over
   any `Modal card`; `Pawl.Card` delegates, and the ability paths call it directly.
3. **The activation path (CR 602.2b → 601.2b–i).** `Activate.activateAbility` gains
   the M4g mode step: choose the mode(s), then choose only the chosen modes' targets,
   then stamp both onto the ability object's `bindings`. `Activate.activatable`
   becomes mode-aware (at least `count` modes fillable), the `Cast.targetable` move.
4. **The trigger-placement path (CR 700.2b / 603.3c / 603.3d).** `Engine.placeOne`
   gains the mode step *and its removal rule*: if fewer than `count` modes are legal,
   the ability — already created on the stack — is **removed** (CR 603.3c: "if no
   mode is chosen, the ability is removed from the stack"); otherwise choose modes,
   then the chosen modes' targets (CR 603.3d), then stamp.
5. **Mode-scoped resolution.** `Resolve.resolveEffects` reads the ability object's
   chosen modes and resolves only their effects, re-validating only their slots (CR
   608.2b/608.2c) — the `resolveSpell` shape, now shared by both ability paths.

The §1 invariant holds throughout: `Modal`/`Mode` are first-order structural data;
the mode choice is a genuine player choice, prompted (never elided except where
forced); `Pawl.Resolve` stays the sole `case effect of` home; nothing cases on a
card's identity. Aether Channeler is a printing whose ETB `TriggeredAbility`'s
`modal` is a three-mode `Modal`; the rules core learns only that it is modal and how
many modes are legal, never that it is Aether Channeler.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Gate — modality on a triggered ability (CR 700.2b), each mode.** Aether
  Channeler enters; its controller chooses the **create** mode → a 1/1 flying Bird
  token appears and nothing is bounced or drawn; the **bounce** mode targeting
  another nonland permanent → that permanent returns to its owner's hand; the
  **draw** mode → its controller draws one card.
- **Only the chosen mode's effect and targets (CR 601.2c/700.2c, at placement).**
  Choosing the bounce mode prompts exactly one nonland-permanent target and binds
  only that slot; choosing create or draw prompts no target and binds no slot. An
  unchosen mode's effect never happens.
- **"Another" self-exclusion (CR 601.2c targeting legality).** The bounce mode's
  legal set excludes Aether Channeler itself — the source is a nonland permanent on
  the battlefield, so a naive `NonlandPermanentTarget` would wrongly offer it.
- **CR 603.3c removal, synthetic fixture (§9).** A modal ETB trigger all of whose
  modes are targeted enters onto a board where **no** mode has a legal target; the
  ability, created on the stack, is **removed** — never resolves, chooses nothing.
- **Modality on an activated ability (CR 602.2b), synthetic fixture (§9).** A modal
  activated ability is activated, its controller chooses a mode, only that mode's
  slot is prompted and only its effect resolves; the CR 608.2b fizzle is scoped to
  the chosen mode's slot.
- **Forced choices ask nothing.** Every existing activated ability (Prodigal
  Sorcerer, Llanowar Elves, Evolving Wilds, Mindslaver, Drudge Skeletons) and the
  existing triggered ability (Rest in Peace), now one-mode `Modal`s, prompt no mode
  and behave exactly as before (the M4g behavior-preserving reshape, verified
  against the existing suite before any M4h test is added).
- **The mana-ability ABI survives the reshape.** Llanowar Elves' `{T}: Add {G}`
  stays a mana ability (resolved inline, off the stack), read across its single
  mode; the falsifier remains an engine that stacked it and deadlocked.
- **Round-trip and replay.** `aether-channeler.json` (and its Bird token) and both
  synthetic fixtures survive the `allPrintings` honesty round-trip through the
  reshaped `Codec` arms; the six migrated ability-bearing files re-parse and
  re-render byte-stable; a `DecisionLog` carrying `ChoseModes` for an ability
  replays deterministically.

**Out of scope (named deferred expiries, §13):** `X` in an activated-ability cost
(rides M3g's `AbilityCost.mana` and M4a's `ChooseModes`/`ChooseX`, deferred there);
text-change (M3d) reaching an ability's effects (no ability text-changes yet);
every `ModeSelection` beyond `ChooseExactly` (choose-two/escalate/pawprint/
same-mode-twice, CR 700.2d–i, the M4g deferrals, unchanged); a non-self-excluding
`NonlandPermanentTarget` (the spec is defined self-excluding for Aether Channeler,
split when a card needs "target nonland permanent" without "another"); a
subtype/type-parameterized general target beyond `NonlandPermanentTarget`/
`WallTarget`; and a real modal *activated* ability gate card (none clean in the
opcode set — the activation path is gated by the synthetic fixture until one lands).

## 1. The reshape

`Pawl.Type.ActivatedAbility` drops `effects`/`targetSpecs`, gains `modal`:

```
data ActivatedAbility card = MkActivatedAbility
  { cost :: AbilityCost,
    -- CR 700.2 / 602.2b: the ability's modal payload -- its mode(s), each with its
    -- own effects and target namespace. A non-modal ability is one Mode with
    -- ChooseExactly 1 (forced, unprompted). Parametric in `card` (M4c): a concrete
    -- Modal Card would drag Card in and cycle; Card ties the knot at Modal Card.
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
```

`Pawl.Type.TriggeredAbility` likewise:

```
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition,
    -- CR 700.2b / 603.3c: the ability's modal payload. Card-free/parametric (M4c).
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
```

`ActivatedAbility` no longer imports `Effect`/`SlotName`/`TargetSpec` directly (it
imports `Modal`); it still imports `AbilityCost`. `TriggeredAbility` imports `Modal`
and `TriggerCondition`. Because the effects now live in `Mode.effects :: Seq (Effect
card)`, **the reshape is the `Seq` migration** M4g's spec §1 promised — no separate
step, no list left on either type.

## 2. The shared reader — `Pawl.Modal`

M4g put the mode-scoped projections on `Pawl.Card`, reading `Card.spell`. They are
card-agnostic structural folds over a `Modal card`, so they lift verbatim into a new
logic module `Pawl.Modal` (parametric in `card`, importing only Type modules —
`Modal`, `Mode`, `ModeIndex`, `Effect`, `SlotName`, `TargetSpec` — so no cycle):

```
-- Every effect across all modes, printed (mode, then written) order.
allEffects :: Modal card -> [Effect card]
-- The union of every mode's target specs.
allTargetSpecs :: Modal card -> Map SlotName TargetSpec
-- CR 608.2c/700.2: only the CHOSEN modes' effects, in ModeIndex order.
modesEffects :: Set ModeIndex -> Modal card -> [Effect card]
-- CR 601.2c/700.2c: only the CHOSEN modes' target specs (union).
modesTargetSpecs :: Set ModeIndex -> Modal card -> Map SlotName TargetSpec
-- CR 700.2: how many modes the selection demands (the ChooseExactly count).
selectionCount :: Modal card -> Natural
```

`Pawl.Card`'s existing `allEffects`/`allTargetSpecs`/`modesEffects`/
`modesTargetSpecs`/`modeTargetSpecs` become one-line delegations to `Pawl.Modal`
over `Card.spell` (unchanged call sites, unchanged behavior). The ability paths
(`Activate`, `Engine`, `Resolve`, `Mana`) call `Pawl.Modal` directly on the
ability's `modal`.

## 3. The mana-ability ABI under modes

`Mana.isManaAbility` (CR 605.1a) reads `ActivatedAbility.effects`/`targetSpecs`
today. Post-reshape it reads across the payload: an ability is a mana ability iff
some effect across its modes produces mana **and** it has no target in any mode.

```
isManaAbility ab =
  not (null (Maybe.mapMaybe Resolve.manaProduced (Modal.allEffects (ActivatedAbility.modal ab))))
    && Map.null (Modal.allTargetSpecs (ActivatedAbility.modal ab))
```

Llanowar Elves is a single-mode payload with one `AddMana` effect and no target
specs, so this is unchanged for it. `Mana.manaTypesOf` likewise maps
`Resolve.manaProduced` over `Modal.allEffects (ActivatedAbility.modal ab)`. No modal
mana ability exists in the pool, and `allTargetSpecs` non-empty (any targeted mode)
correctly disqualifies one — the reshape does not widen the mana-ability predicate.

## 4. The activation path (CR 602.2b)

CR 602.2b: activating an ability follows the spell rules 601.2b–i, so it gains the
same mode step `Cast.castSpell` has (M4g §5). `Activate.activateAbility` today puts
the ability on the stack, prompts targets, stamps `fromChoices chosen Map.empty
Nothing Set.empty`, then pays. The new flow interleaves modes (601.2b, before
targets 601.2c):

1. **Create the ability object on the stack** (unchanged).
2. **Enumerate legal modes.** `legal = Target.fillableModes srcId (ActivatedAbility
   .modal ability) gs` (§7 — `fillableModes` is generalized to any `Modal` and takes
   the source id for "another" self-exclusion). `activatable` (below) has already
   guaranteed `size legal >= count`.
3. **Choose modes.** `count = Modal.selectionCount modal`. If `size legal > count`,
   prompt `ChooseModes decider pid abilId legal count`; else the selection is forced
   (`legal`, unprompted). Validate the answer is a size-`count` subset of `legal`
   (reject-not-repair → the whole activation is a no-op, restoring `gs`, the existing
   posture).
4. **Choose targets (CR 601.2c).** `sets = Target.legalSetsExcluding srcId
   (Modal.modesTargetSpecs chosen modal) gs` — only the chosen modes' slots. Prompt
   `ChooseTargets` (unless empty), validate (reject-not-repair).
5. **Stamp.** `fromChoices chosen Map.empty Nothing chosenModes` on the ability
   object (no X, no text-change subtypes: abilities carry neither this milestone —
   `X` in ability costs is deferred, §13).
6. **Pay the costs** (unchanged).

`Activate.activatable` becomes mode-aware, the `Cast.targetable` move: replace
"every slot of the ability has a legal recipient" with

```
Set.size (Target.fillableModes srcId (ActivatedAbility.modal ability) gs)
  >= fromIntegral (Modal.selectionCount (ActivatedAbility.modal ability))
```

For a single-mode ability (all existing) this is identical to today ("the one mode's
slots all fillable"). The mana-ability / additional-cost / `{T}`-sickness / mana
gates are unchanged.

## 5. The trigger-placement path (CR 700.2b / 603.3c / 603.3d)

`Engine.placeOne` puts one triggered ability on the stack as a fresh `OfTrigger`
object with empty `bindings`. CR 700.2b/603.3c/603.3d make placement a choice point.
The new flow (each trigger, as it is placed, CR 603.3b order):

1. **Create the ability object on the stack** (unchanged).
2. **Enumerate legal modes.** `legal = Target.fillableModes srcId (TriggeredAbility
   .modal ability) gs`; `count = Modal.selectionCount modal`.
3. **CR 603.3c removal.** If `size legal < count` — not enough modes can be legally
   chosen — the ability, already on the stack, is **removed** (drop it from
   `stack` and `objects`, the `Resolve.cease` shape) and placement ends. This is the
   trigger-only rule with no spell analog: a spell that can't choose is simply never
   offered; a trigger is placed *then* removed. (For a non-modal trigger — Rest in
   Peace — `count = 1` and its single empty mode is trivially fillable, so `size
   legal = 1 >= 1`: never removed, unchanged.)
4. **Choose modes.** If `size legal > count`, prompt `ChooseModes decider controller
   abilId legal count`; else forced (`legal`). Validate size-`count` subset
   (reject-not-repair → remove from the stack, CR 603.3d's "removed" for an illegal
   required choice).
5. **Choose targets (CR 603.3d → 601.2c).** `sets = Target.legalSetsExcluding srcId
   (Modal.modesTargetSpecs chosen modal) gs`; prompt `ChooseTargets` (unless empty);
   validate (reject-not-repair → remove from the stack).
6. **Stamp.** `fromChoices chosen Map.empty Nothing chosenModes` on the ability
   object.

`placeOne` becomes monadic-with-prompts (it already runs in `Game` inside
`placePendingTriggers`'s `mapM_`). The `decider` is the controller's
(`Decide.deciderFor controller gs`). Aether Channeler is the first triggered ability
to reach steps 4–5 with a real choice; Rest in Peace and every future non-modal,
targetless trigger take the forced/empty path.

## 6. Mode-scoped resolution

`Resolve.resolveEffects` today takes an explicit `[Effect]` + `Map SlotName
TargetSpec` from the caller (`Stack.resolveTop` passes `TriggeredAbility.effects`/
`targetSpecs`; `resolveAbility` passes the activated ability's). It becomes payload-
and-binding-aware, the `resolveSpell` shape:

```
resolveEffects :: ObjectId -> ObjectId -> Modal.Modal Card -> Game ()
resolveEffects stackId srcId modal = do
  ... obj <- lookup stackId ...
  let chosen  = Binding.modesOf (Object.bindings obj)
      effects = Modal.modesEffects chosen modal
      specs   = Modal.modesTargetSpecs chosen modal
  ... (unchanged CR 608.2b fizzle over `specs`, fold applyEffect over `effects`) ...
```

`Resolve.resolveAbility abilId srcId ability = resolveEffects abilId srcId
(ActivatedAbility.modal ability)`; `Stack.resolveTop`'s trigger arm calls
`resolveEffects oid srcId (TriggeredAbility.modal ability)`. Both read the chosen
modes off the object, symmetric with `resolveSpell`. Abilities do **not** rewrite
text (M3d) — `resolveEffects` uses `Modal.modesEffects` directly, no
`rewriteEffect`, unchanged from today (only `resolveSpell`'s `effectsOf` rewrites).

`Stack.resolveTop`'s pre-resolution `searchesLibrary` scan (CR 601.3, Panglacial)
reads the **chosen** modes' effects: `any Resolve.searchesLibrary (Modal.modesEffects
(Binding.modesOf (Object.bindings obj)) (ActivatedAbility.modal ability))`. Evolving
Wilds is single-mode, so `chosen = {0}` and this is unchanged.

`Pawl.Resolve` stays the sole `case effect of` home. No `Effect` constructor is
added: modes reshape *which* effects run, not *what* an effect is.

## 7. Incidental machinery — the gate

Aether Channeler's ETB `TriggeredAbility` (`condition = SelfEnters`, M3f) carries a
three-mode `Modal` with `ChooseExactly 1`:

| Idx | Effect(s) | `targetSpecs` |
|---|---|---|
| 0 | `Create (Literal 1) birdToken` (M4c) | `{}` |
| 1 | `MoveToZone "permanent" Hand` (M4b) | `{"permanent" ↦ NonlandPermanentTarget}` |
| 2 | `Draw (Literal 1)` (M4b) | `{}` |

Two additions to the closed half, both in M4g's WallTarget budget:

- **`TargetSpec.NonlandPermanentTarget`** — "another target nonland permanent": a
  permanent on the battlefield whose **projected** card types (M3c) do **not**
  include `Land`, **excluding the targeting source** (CR "another"). `Target
  .legalRecipients` reads the projection like `CreatureOrEnchantmentTarget`; the
  self-exclusion is applied by `legalSetsExcluding` (below), not inside
  `legalRecipients` (which is source-blind). Defined self-excluding because Aether
  Channeler is the only card that needs it; a non-excluding variant splits the spec
  when one lands (§13, the `WallTarget` specific-then-general posture).
- **The Bird token card** — `birdToken`: a 1/1 `Creature — Bird` with `Flying`
  (M2a). Colour ("white") is unmodelled, as for every token (M4c). `Subtype.Bird`
  already exists; no new subtype. Embedded in the `Create` effect via M4c's
  parametric `card` knot; the `Codec` `Create` arm already serializes a nested token
  `Card`, so `aether-channeler.json` round-trips it.

**Self-exclusion, minimally.** `stillLegal` (CR 608.2b re-validation) checks the
*chosen* target, which is never the source, so re-validation stays source-blind and
correct — self-exclusion is only a *choice-time* filter. `Pawl.Target` gains:

```
-- CR "another": specs that exclude the targeting source from their legal set.
-- Only NonlandPermanentTarget so far; a general per-slot "another" flag is future.
selfExcludes :: TargetSpec -> Bool
selfExcludes spec = case spec of
  NonlandPermanentTarget -> True
  _ -> False

-- legalSets, then drop the source recipient from each self-excluding slot. Callers
-- with a source (Cast, Activate, Engine.placeOne) use this; existing specs are
-- unchanged (the source recipient is not in any non-self-excluding spec's set --
-- e.g. Prodigal Sorcerer may still target itself with AnyTarget, CR 115.4).
legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)

-- CR 700.2a: the mode indices all of whose slots have a legal recipient (source-
-- relative for "another"). Generalized from Cast to any Modal payload.
fillableModes :: ObjectId -> Modal.Modal Card -> GameState -> Set ModeIndex
```

`Cast.fillableModes`/`castSpell`/`castableWhileSearching`'s target computation move
to `Target.fillableModes`/`legalSetsExcluding` (passing the spell object id — a
no-op for every current spell, none self-excluding, so behavior is identical). This
keeps one home for fillability and self-exclusion across spells and abilities.

**No new opcode, no new `Modification`, no new SBA, no projection change** beyond the
one `NonlandPermanentTarget` reader — Aether Channeler re-uses M4b/M4c verbs under a
modal trigger wrapper, the Chaos Charm posture.

## 8. Codec and the JSON migration

`activatedAbilityToJson`/`jsonToActivatedAbility` and the triggered pair reshape to
serialize `{cost | condition, modal}`, reusing the existing `modalToJson`/
`jsonToModal` arms (M4g, already used by `Card.spell`):

```
activatedAbilityToJson aa =
  Object [ ("cost", abilityCostToJson (ActivatedAbility.cost aa)),
           ("modal", modalToJson (ActivatedAbility.modal aa)) ]
```

`TargetSpec.NonlandPermanentTarget` gains a (tagged, nullary) `Codec` arm alongside
`WallTarget`. The Bird token needs no new arm (`Create` already serializes a nested
`Card`).

**Migration.** Six committed files carry a non-empty ability array and must re-nest
`effects`/`targetSpecs` under a one-mode `modal` (`{"modes":[{"effects":…,
"targetSpecs":…}],"selection":{"type":"ChooseExactly","value":1}}`): `drudge-
skeletons`, `evolving-wilds`, `llanowar-elves`, `mindslaver`, `prodigal-sorcerer`
(activated) and `rest-in-peace` (triggered). The edit is mechanical and the
round-trip is the net: a mis-nested file fails to decode or fails byte-stability
(the M4g files-are-source-of-truth pipeline; the whole pool is re-rendered and
re-parsed byte-stable). Every other file's empty `activatedAbilities`/
`triggeredAbilities` array is untouched.

## 9. The synthetic fixtures

Two labeled, expiring fixtures (the `tests-prefer-real-cards` crutch discipline —
each retires when its real gate lands), reusing existing opcodes only:

- **A modal ETB trigger, all-targeted (CR 603.3c removal).** A creature whose ETB
  `TriggeredAbility` is `ChooseExactly 1` over two modes, both `CreatureTarget`:
  `DealDamage "creature" (Literal 1)` and `PutCounters PlusOnePlusOne (Literal 1)
  "creature"` (M3a + M4f). On a board with no other creature, **neither** mode is
  fillable, so the placed trigger is removed (CR 603.3c). **Expiry:** a real
  all-targeted modal ETB in the opcode set (none today).
- **A modal activated ability (CR 602.2b).** A creature with `{cost}: Choose one —
  DealDamage "creature" (Literal 1) / PutCounters PlusOnePlusOne (Literal 1)
  "creature"`, exercising the activation-path mode prompt, mode-scoped targeting, and
  the mode-scoped CR 608.2b fizzle. **Expiry:** a real modal activated ability in the
  opcode set (none clean — Goblin Cratermaker needs colour, Jitte needs charge
  counters, Insidious Fungus/Cankerbloom have land-play/proliferate riders).

Both are deterministic fixtures (no random-game deck), join `allPrintings` for the
round-trip, and are clearly named as crutches in the test source.

## 10. Tests

All gameplay-level (activate/trigger through the stack, assert on game state); names
carry their CR numbers.

- **Type/reader:** `Modal.modesEffects`/`modesTargetSpecs` over a two-mode ability
  payload; the reshaped `ActivatedAbility`/`TriggeredAbility` decode; a single-mode
  ability payload behaves as the old flat one (behavior-preserving reshape, run
  against the existing suite before any M4h assertion).
- **Gate, each mode (CR 700.2b):** Aether Channeler's create/bounce/draw modes as in
  the exit criterion; choosing the bounce mode binds only `"permanent"`.
- **Self-exclusion (CR 601.2c):** the bounce mode's legal set omits Aether Channeler
  itself; with it the *only* nonland permanent, the bounce mode is unfillable (so
  `ChooseModes` offers `{0,2}`).
- **Only chosen mode (CR 601.2c/700.2c):** choosing create adds a token and neither
  bounces nor draws; `bindings` has no `"permanent"` slot.
- **CR 603.3c removal (synthetic §9):** the all-targeted modal ETB on a lone board is
  removed from the stack, resolves nothing. A comment names the M3a/M4g posture (a
  spell is never offered; a trigger is placed then removed).
- **CR 602.2b (synthetic §9):** the modal activated ability, activated, prompts a
  mode then only that mode's target; the other mode's effect never resolves; a
  chosen-mode target that leaves before resolution fizzles (CR 608.2b), scoped to the
  chosen mode.
- **Forced/no-prompt regression:** Prodigal Sorcerer, Llanowar Elves (still a mana
  ability, off the stack), Evolving Wilds (still offers cast-while-searching),
  Mindslaver, Drudge Skeletons, Rest in Peace — each prompts no mode and behaves as
  before.
- **Round-trip:** `aether-channeler.json`, both synthetic fixtures, and the six
  migrated files via the `allPrintings` honesty property.
- **Replay:** a `DecisionLog` with `ChoseModes` for the Aether Channeler trigger
  replays byte-identically.

## 11. Module and dependency notes

- New logic module `Pawl.Modal` (imports only Type modules: `Modal`, `Mode`,
  `ModeIndex`, `Effect`, `SlotName`, `TargetSpec`). No cycle: it imports no `Card`.
  `Pawl.Card` imports `Pawl.Modal` and delegates.
- `Pawl.Type.ActivatedAbility`/`Pawl.Type.TriggeredAbility` import `Modal` (drop
  `Effect`/`SlotName`/`TargetSpec`).
- `Pawl.Target` imports `Pawl.Modal` and `ModeIndex`; gains `selfExcludes`,
  `legalSetsExcluding`, `fillableModes` (the last moved/generalized from `Cast`).
  Target already imports `Projection`/`Game`/`Sba`; adding the `Modal` logic module
  (Type-only imports) is acyclic.
- `Pawl.Cast` drops its own `fillableModes` (now `Target.fillableModes`) and routes
  target computation through `Target.legalSetsExcluding`.
- `Pawl.Activate` computes `Target.fillableModes` and prompts `ChooseModes`;
  `Pawl.Engine.placeOne` does the same plus the CR 603.3c removal; `Pawl.Resolve
  .resolveEffects` takes a `Modal Card`; `Pawl.Stack` passes each ability's `modal`.
- `Pawl.Mana` reads `Modal.allEffects`/`allTargetSpecs`.
- `Pawl.Codec` reshapes the two ability arms (reusing `modalToJson`/`jsonToModal`)
  and adds the `NonlandPermanentTarget` arm.
- `Pawl.Resolve` stays the sole `case effect of` home; `Pawl.Target` the sole
  targeting-legality home; `Pawl.Cast`/`Pawl.Activate`/`Pawl.Engine` case on
  `ModeSelection` (via `Modal.selectionCount`) as an orchestration tag, never a
  card's identity.

## 12. Invariant check

- **Closed/open separation.** The rules core reads a *classification* of the ability
  payload — how many modes are legal, which effects the chosen modes carry — never a
  card's identity. The mode-selection casing is `Modal.selectionCount` (an
  orchestration tag); `Pawl.Resolve` stays the only module casing on `Effect`. No
  `case card of AetherChanneler → …` anywhere.
- **First-order, non-recursive DSL.** The reshape adds no function or control flow to
  the payload; `Modal`/`Mode` remain the structural data M4g built. The parametric
  `card` nests structurally (a mode's `Create` effect holds a Bird token's card),
  never a recursive call.
- **The engine makes no choice it shouldn't.** The ability mode choice is a real
  player choice, prompted through `ChooseModes` — elided only when forced (one legal
  mode, or a non-modal single mode), and removed (never silently defaulted) when no
  mode is legal (CR 603.3c). Self-exclusion narrows a legal set to what the rules
  allow; it invents nothing.
- **Numeric tower / collections.** Indices/counts are `Natural`; the payload's
  collections are `Seq` (modes/effects) and `Set` (chosen modes), per the avoid-lists
  directive — and the reshape *retires* the last `[Effect card]` on the two ability
  types.

## 13. Named deferred expiries and fast-follows

- **A real modal activated ability gate.** The activation path is gated by the
  synthetic fixture (§9) because no clean modal activated ability lives in the
  engine's opcode set (Goblin Cratermaker needs a colour model; Umezawa's Jitte /
  Power Conduit need charge counters and counter-removal costs; Insidious Fungus /
  Cankerbloom carry land-play / proliferate riders). Retire the fixture when one lands.
- **`X` in an activated-ability cost.** M3g's `AbilityCost.mana` + M4a's `ChooseX`
  were built for it; `activateAbility` gains the `ChooseX` step (before `ChooseModes`
  or after, per 601.2b order) when the first `{X}`-cost ability lands. Unwired here
  (no ability carries `{X}`).
- **Text-change reaching an ability's effects (M3d).** `resolveEffects` does not
  rewrite effects; no ability text-changes yet. Wire `rewriteEffect` in when one does.
- **Non-self-excluding `NonlandPermanentTarget`.** Defined self-excluding for "another
  target nonland permanent"; a card reading plain "target nonland permanent" splits it
  (or adds a per-slot "another" flag) — the `WallTarget` specific-then-general posture.
- **`ModeSelection` beyond `ChooseExactly` on abilities.** The M4g deferrals (choose-
  two/commands, escalate CR 700.2h, pawprint 700.2i, same-mode-twice 700.2d, another-
  player-chooses 700.2e) apply equally to abilities; each is a new `ModeSelection`
  constructor due with its first card, spell or ability.
- **Multi-mode slot-name collision on abilities.** Unreachable until a selection picks
  ≥2 modes; the M4g mitigation (qualify a bound slot by its `ModeIndex`) covers both.
- **Modal triggered abilities beyond ETB.** Only `SelfEnters` triggers exist; a modal
  trigger on a leaves-the-battlefield or phase-step condition rides the same
  `placeOne` path when those conditions land.
