# M4a the numeric tower's X + the general binding environment — design

Design for milestone **M4a**, the first letter of M4 (see the split table in
`docs/design.md` §3): **the numeric tower's `X`, built on a general binding
environment.** M4a opens the open half — the vocabulary that grows forever — and
it leads because X is *upstream of every opcode*: §2.12 says decide the numeric
tower "before M4, nearly free; discovering it halfway through the vocabulary is
expensive." Every later M4 quantity (a counter count, a "for each", a modal
back-reference) is born X-aware because M4a lands the shape now.

The letter carries two coupled deliverables:

1. **The general binding environment** — the risk-register's *"named binding
   slots"* (§7, D4). Today a spell's cast-time choices live in two parallel
   `Object` fields (`targets`, `chosenSubtypes`); X would be a third. Instead,
   unify them into one `Object.bindings :: Map SlotName Binding`. X is the
   **second customer** after targets — the classic "two customers, generalize
   now" trigger the risk register anticipated.
2. **`X`** — the numeric tower's variable, implemented on that environment.

The gate card, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-19):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Blaze** | `{X}{R}` | Sorcery | — | "Blaze deals X damage to any target." |

Blaze is chosen because it **falsifies its own naive implementation** on the one
axis this letter owns, and *only* that axis. It reuses `Effect.DealDamage` (M3a)
and `TargetSpec.AnyTarget` (M3a) unchanged; its sole novelty is the `X` quantity.
The falsifiers:

- **X is chosen at cast (CR 601.2b) and re-read at resolution.** A naive engine
  that stored the printed quantity has nothing to store — `X` is not a number
  until the caster names it. An engine that resolved the value at *cast* and
  baked a literal would work here but is the wrong seam; M4a stores the *chosen
  value* and evaluates `Quantity.X` against it at resolution, the same
  late-binding shape M3a used for targets (CR 608.2b).
- **X gates payment.** `{X}{R}` cannot be paid unless the mana covers the chosen
  value; Blaze at X=3 costs `{3}{R}`. Choosing X and paying for it are one
  interlocked step (CR 601.2b→f→h).

Blaze over **Fireball** deliberately: Fireball divides its X damage among multiple
targets (a second axis — multi-target *division*), which belongs to the modal /
multi-target volume work, not here. One axis per letter.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the phased spine

M4a is one axis (X) sitting on one refactor (the binding environment). Neither
adds a rules subsystem the size of M3b's projection or M3f's event pipeline: the
refactor is a behavior-preserving unification of storage the engine already
keeps, and X is a single new `Quantity` arm plus a single new cast-time prompt.

**The phased spine** (land the behavior-preserving refactor first, keep the new
behavior isolated behind it):

1. **The binding environment (Phase 1) — no behavior change.** Replace
   `Object.targets` and `Object.chosenSubtypes` with one
   `Object.bindings :: Map SlotName Binding`. Migrate every reader (`Pawl.Cast`
   writes it; `Pawl.Target`, `Pawl.Resolve`, `Pawl.Projection` read it) and every
   `changeZone` reset. Every existing test passes unchanged — this is pure
   plumbing, and the M3a–M3g fixtures are the regression net. Exit: green suite,
   `Object.targets` / `Object.chosenSubtypes` gone.
2. **X (Phase 2) — Blaze.** `Quantity.X`, `ManaSymbol.Variable`, the
   `ChooseX` prompt and its cost substitution, the `Quantity.evaluate` arm, the
   generalized D4 lint, and Blaze as data. Exit: Blaze at a chosen X deals that
   much damage; an X the caster cannot pay makes the cast a no-op.

M4a stays **one milestone, one spec, one plan**: the phases are ordered commits;
the plan owns the decomposition. Phase 2 depends on Phase 1 (X's chosen value is
stored *in* the binding environment), so they do not interleave.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **X chosen and paid (Phase 2, CR 601.2b/f/h).** A player casts Blaze choosing
  X=3; `{3}{R}` is paid from their mana; the Blaze on the stack carries the chosen
  value 3 in its binding environment. Choosing X=0 is legal (Blaze is castable
  whenever `{R}` alone is affordable) and resolves as 0 damage.
- **X re-read at resolution (Phase 2).** Blaze resolves and deals exactly the
  chosen X to the chosen target (a creature loses 3 toughness worth of marked
  damage / a player's life drops by 3). The falsifier in a comment: an engine that
  ignored the chosen value (treated X as 0, or as the printed `{X}`'s mana value)
  deals the wrong amount.
- **Unaffordable X is a no-op (Phase 2).** Casting Blaze at an X the caster cannot
  pay leaves the game unchanged (reject-not-repair, the `Cast.castSpell` posture);
  Blaze stays in hand.
- **The binding environment preserves behavior (Phase 1).** Every M3a–M3g
  scenario — Lightning Bolt's target re-validation (CR 608.2b), Magical Hack's
  target *and* word-swap on one slot, Mindslaver's player target — passes
  unchanged reading from `Object.bindings` instead of the retired fields.

The `DecisionLog` replays deterministically with the new `ChooseX` response
alongside the existing target and land-type choices.

**Non-goals.**

- **No other numeric-tower variants.** `Star` (`*`, a characteristic-defining
  ability — Tarmogoyf's power, a layer-7a CDA), `Plus` (`1+*`), `Half` (Little
  Girl), `Infinite` (Mox Lotus), and `Count` ("for each") stay **shape-only** —
  the `Quantity` type's comment already reserves them, and M4a implements only
  `X`, exactly M1a's "shape lands, one variant implemented" move. Each is a named
  expiry (§7), due with its first card.
- **No X in a stored continuous effect.** `Quantity.X` is evaluated for
  `Effect.DealDamage` (one-shot, at resolution, while the spell object still
  carries its binding). A `+X/+X` until end of turn (`Effect.ModifyTarget` with
  `X`) must **freeze** X to a `Literal` when the effect is stored, per the standing
  note at `Projection.hs:55` ("When X lands, Resolve must freeze the value into
  the stored effect"): the source is gone by the time the continuous effect is
  read, so a live X read is wrong. Blaze exercises no continuous effect; the freeze
  is a named expiry (§7), due with the first `+X/+X` card.
- **No X in activated-ability costs.** M4a is **spells only**. Activated-ability
  `{X}` (Stroke of Genius, Fireball-as-instant analogues) rides the same
  `ChooseX` mechanism through M3g's `AbilityCost.mana :: Maybe ManaCost`, deferred
  with a named expiry (§7).
- **No multi-target damage division.** Fireball's "divided as you choose among
  any number of targets" is multi-target + division, deferred to the modal /
  multi-target volume work.
- **No new zone verbs, tokens, counters, prevention, counterspells, or modes.**
  Those are M4b–M4g. Blaze needs none of them.
- **No engine-computed maximum X.** `ChooseX` names any `Natural`; an unaffordable
  choice no-ops at payment (reject-not-repair), the same posture as the mana-source
  elision (`Mana.payCost`). Pre-computing the maximum affordable X is a nicety
  deferred (§7); it is not needed for a correct, deterministic engine.

## 1. New and grown types

### Phase 1 — the binding environment

**`Pawl.Type.Binding`** (new) — everything a caster chose for one slot, collected
into a record so a single slot can carry multiple bindings (Magical Hack's
`"target"` slot carries both a target *and* a word-swap):

```haskell
-- CR 601.2: the cast-time choices bound to one named slot of a spell (or
-- ability) on the stack. A record, not a sum, because one slot may carry several
-- kinds of choice at once -- Magical Hack's slot is both TARGETED (a Recipient)
-- and WORD-SWAPPED (a Subtype pair). A field per binding kind; a kind absent for
-- this slot is Nothing. Grows a field per future binding (a mode, a for-each
-- count); today three cover every card.
data Binding = MkBinding
  { -- CR 601.2c: the chosen target for this slot. Re-validated at resolution
    -- (CR 608.2b). Nothing for a non-targeting binding (a bare X).
    target :: Maybe Recipient,
    -- CR 612: the (from, to) basic land types chosen for a text-changing slot.
    subtypes :: Maybe (Subtype, Subtype),
    -- CR 601.2b: the value chosen for a variable in the cost (X). Read by
    -- Quantity.evaluate. Nothing for a slot with no amount.
    amount :: Maybe Natural
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Object`** loses `targets` and `chosenSubtypes`, gains `bindings`:

```haskell
  -- CR 601.2: the choices bound while casting, by slot name. Empty for everything
  -- but a spell or ability on the stack. Per-incarnation state: reset by
  -- changeZone, so CR 400.7 forgets them when the object moves. Replaces the M3a
  -- `targets` and M3d `chosenSubtypes` fields (the risk-register's D4 named
  -- binding slots, unified as X arrives as the second customer).
  bindings :: Map SlotName Binding
```

`Setup` and every object constructor initialize `bindings = Map.empty` where they
initialized the two retired fields. A helper `Object.bindingsTargets ::
Map SlotName Binding -> Map SlotName Recipient` (project the `target` fields,
dropping `Nothing`s) gives the retired `Object.targets` view for readers that want
it whole (`Target`'s CR 608.2b re-validation iterates targets); likewise
`Object.bindingSubtypes` for the M3d readers. These projections keep the migration
mechanical: a reader that said `Object.targets o` says `Object.bindingTargets o`.

**X's slot.** `Quantity.X` needs to find "this object's chosen X" with no slot in
hand (`Quantity.evaluate`'s signature is `GameState -> ObjectId -> Quantity`).
X is stored under a **reserved slot name** — `SlotName.variableX`, a documented
constant that no `targetSpecs` may use — whose `Binding.amount` holds the value.
`Quantity.evaluate` for `X` looks up `SlotName.variableX` in the object's
`bindings` and reads `amount`. (A more general `Quantity.Bound SlotName` reading
any named amount is the shape a *named* count would want — deferred to the first
"for each" card, §7; a single reserved slot is right while X is the only amount.)

### Phase 2 — X

**`Pawl.Type.Quantity`** grows the reserved variant its own comment names:

```haskell
  | -- CR 601.2b: X -- a value the caster chose while casting, read from the
    -- object's binding environment (Object.bindings, slot SlotName.variableX).
    -- One-shot only for now: a continuous effect must FREEZE this to a Literal
    -- when stored (Projection.hs note), which no M4a card exercises.
    X
```

**`Pawl.Type.ManaSymbol`** grows the variable symbol its own comment names:

```haskell
  | -- CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X to the cost once chosen (0 before, for the castability floor).
    Variable
```

**`Pawl.Type.Prompt`** grows the X choice:

```haskell
  -- CR 601.2b: choose the value of X while casting (the ObjectId is the spell).
  -- Any Natural; payment (reject-not-repair) rejects an unaffordable choice, so
  -- the engine computes no maximum. Prompted before targets (CR 601.2b precedes
  -- 601.2c), and only when the cost contains a Variable symbol -- a spell with no
  -- {X} is not asked (where the rules leave nothing to choose, don't prompt).
  ChooseX :: Decider -> PlayerId -> ObjectId -> Prompt Natural
```

**`Pawl.Type.Response`** grows the replay variant `ChoseX Natural`, mirroring
`Searched` / `ChoseBasicLandTypes`.

**`Pawl.Card`** gains **Blaze** (`{X}{R}` = `[Variable, OfType Red]`; Sorcery;
one effect `DealDamage "target" Quantity.X`; one slot `"target" ↦ AnyTarget`).
Blaze joins `redDeck` for sorcery-speed X random-game coverage (the first M4 card
to enter a random deck — burn with a variable cost stresses the payment path
under random play).

## 2. Phase 1 — the binding environment (no behavior change)

**The claim.** Targets (M3a) and land-type word-swaps (M3d) are both *cast-time
choices bound to a slot*, stored today in two parallel `Object` maps. They are one
concept — the risk register's "named binding slots" — and X is about to be a
third. Unify them now, while there are exactly two, so X slots in as a field
rather than a fourth parallel map, and every future binding (a mode, a for-each
count) does the same.

**The migration is mechanical and behavior-preserving.**

- **Write site — `Cast.castSpell`.** Today it stamps `Object.targets = chosen` and
  `Object.chosenSubtypes = bound` on the new stack incarnation. It now builds one
  `Map SlotName Binding` merging the chosen targets, the chosen land-type pairs,
  and (Phase 2) the chosen X, keyed by slot: `mergeBindings chosen bound amount`.
  A slot present in `chosen` gets `target = Just r`; a slot in `bound` gets
  `subtypes = Just p`; the reserved X slot gets `amount = Just x`. Magical Hack's
  `"target"` slot ends with both `target` and `subtypes` set — the exact case the
  record shape exists for.
- **Read sites.** `Target` (CR 608.2b re-validation) reads targets via
  `Object.bindingTargets`; `Resolve` (`DealDamage` recipient, `ChangeText`
  word-swap) and `Projection` (M3d's stored land-type rewrite) read `target` /
  `subtypes` off the relevant slot's `Binding`. Each call site changes from a
  field access to a `Binding` field access; no logic moves.
- **Reset — `Event.changeZone`.** Already resets per-incarnation state; it now
  clears one map (`bindings = Map.empty`) instead of two. The M3d
  Magical-Hack-on-a-spell negative test (the word-swap is forgotten when the spell
  resolves, CR 400.7) rides on this reset unchanged.

**Phase 1 gate:** the whole M3a–M3g suite is green with `Object.targets` and
`Object.chosenSubtypes` deleted and every reader on `Object.bindings`. No new
scenario — the existing fixtures are the regression net, which is the point of
landing the refactor as its own phase.

## 3. Phase 2 — X (choose at cast, pay, re-read at resolution)

**Choosing X, in CR order.** `Cast.castSpell` gains a first choice step, before
target selection (CR 601.2b precedes 601.2c): if the cost contains a `Variable`
symbol, prompt `ChooseX decider pid oid` and bind the answer; otherwise bind
nothing. The chosen value is threaded into both the cost computation and the
stamped `bindings`.

**Substituting X into the cost.** A `ManaCost` with `Variable` is not payable as
printed. `Mana` (or `Cast`) gains:

```haskell
-- CR 601.2f: the total cost with X resolved. Each Variable symbol becomes
-- Generic n; every other symbol is unchanged. Order preserved (ManaCost is a
-- list, never fixed arity).
substituteX :: Natural -> ManaCost -> ManaCost
```

Payment pays `substituteX x cost` through the existing `Mana.payCost`
(reject-not-repair: `Nothing` → the cast is a no-op, unchanged posture). The
**castability floor** — whether Blaze appears as a legal action at all — treats a
`Variable` as contributing 0, because the caster may always choose X=0
(`Cast.castable` / `costOf`'s affordability check uses `substituteX 0`, i.e. pay
`{R}`). So Blaze is offered whenever `{R}` is affordable, and the *actual* X is
gated at payment.

**Evaluating X at resolution.** `Quantity.evaluate` gains the arm:

```haskell
  Quantity.X -> fmap toInteger (bindingAmount SlotName.variableX =<< Game.lookupObject oid gs)
```

reading the source object's `bindings` at the reserved X slot. `DealDamage`
already evaluates its `Quantity` against the source object (`Resolve.hs:207`), so
Blaze's `DealDamage "target" X` deals the chosen X: no new resolution path, just
the new arm. The object still carries its `bindings` at resolution (they are
cleared only when it leaves the stack, after resolution), so the read is live and
correct for a one-shot effect.

**Phase 2 gate:** cast Blaze at X=3 (paying `{3}{R}`); assert 3 damage to the
chosen target; cast at X=0 (paying `{R}`) → 0 damage; attempt X beyond the mana
available → no-op, Blaze in hand.

## 4. The D4 dataflow lint, generalized

M3a's D4 lint (`CardSpec.hs`) asserts *equality*: every slot an effect reads
(`Resolve.slotsOf`) equals every slot the card declares (`targetSpecs`). X adds a
*second* kind of read (a quantity, not a target slot) with a *second* kind of
declaration (a `Variable` in the cost, not a `targetSpec`). The lint generalizes
to keep both honest:

- **Targets — unchanged.** `slotsOf` reads ⊆⊇ `targetSpecs` writes, as today. The
  reserved X slot is **exempt** from this equality: it is never a `targetSpec`.
- **X — new conjunct.** A card whose effects read `Quantity.X` (a new
  `Resolve.readsX :: [Effect] -> Bool`, or `Quantity`-aware `slotsOf`) **iff** its
  cost contains a `Variable` symbol. A `Quantity.X` with no `{X}` in the cost is
  an unbound read (the executor would evaluate it to 0 or `Nothing` silently); a
  `{X}` cost with no `Quantity.X` reader is a variable nothing spends (a cost the
  caster pays for no reason). Both are card-data bugs the lint now catches, the
  same contract M3a drew for targets.

This is the risk register's mitigation cashed for the value half: *"for every
question the core must answer about an effect before executing it, add an explicit
classification."* "Is this quantity bound?" is now lint-checked, not trusted.

## 5. Invariants preserved

- **The two-halves invariant holds.** No closed-half module cases on a card's or
  an effect's *identity*. `Resolve` remains the sole home of `case effect of` and
  `case quantity of` (now including `Quantity.X`); `Cast`/`Mana` case on
  `ManaSymbol` (open-half cost machinery, as they already do for `Generic`/`OfType`).
  The closed half *asks* classifications — does this cost contain a variable? what
  amount did the caster bind? — and never names Blaze.
- **The engine makes the right choices and elides only indistinguishable ones.**
  X is a genuine player choice (CR 601.2b) and is prompted (`ChooseX`); it is
  asked only when the cost has a `Variable` (a spell with no {X} has nothing to
  choose). The mana-source elision (`Mana.payCost`, git-bug `65ce714`) is
  unchanged and unaffected — X substitution happens before source selection.
- **Conventions.** One type per module (`Binding` is new; `X`, `Variable`,
  `ChooseX`, `ChoseX` are new constructors on existing types). `Binding` uses the
  `MkBinding` record constructor (non-punning). `NamedFieldPuns` per the M3b
  amendment. Arbitrary-precision `Natural`/`Integer` throughout (X is a `Natural`;
  `Quantity.evaluate` stays `Maybe Integer`). No fixed arity: `substituteX` maps
  over the `ManaCost` list.

## 6. Setup, decks, and testing

Testing follows the phased spine (§0).

**Phase 1 — the refactor.** No new tests; the exit criterion is the **existing**
M3a–M3g suite green with the retired fields gone. The D4 lint (unchanged in Phase
1) still passes over the whole pool reading `Object.bindings`.

**Phase 2 — X (Blaze), deterministic fixtures.** A red fixture (Blaze is red;
reuse M3a's Lightning Bolt fixtures' mana base):

- **X paid and dealt.** The caster has `{3}{R}` available and a target creature
  (or the opponent). Cast Blaze choosing X=3; assert `{3}{R}` is spent, the Blaze
  on the stack has `amount = Just 3` at `SlotName.variableX`, and after resolution
  the target took 3 (marked damage on a creature / life down 3 on a player). Cite
  CR 601.2b/f/h and 608.2 (resolution).
- **X=0.** Cast Blaze choosing X=0 with only `{R}` available; assert it is
  castable, pays `{R}`, resolves to 0 damage, and (if aimed at a creature) the SBA
  does not destroy it. Falsifier comment: a floor that required `{X}` > 0 would
  make Blaze uncastable here.
- **Unaffordable X.** With only `{R}` available, cast Blaze choosing X=3; assert
  the game is unchanged and Blaze is still in hand (reject-not-repair). Cite the
  `Cast.castSpell` no-op posture.
- **The generalized D4 lint** (a `CardSpec` unit): Blaze's `Quantity.X` read is
  matched by its `{X}` cost; a synthetic card reading `X` with no `Variable` in
  its cost fails the lint (the negative, mirroring M3a's `"ghost"` slot case).

**Random-game coverage.** Blaze joins `redDeck`; the red matchup's property run
exercises the X-cost payment path (`substituteX`, the castability floor, the
`ChooseX` replay round-trip) under random play. The scripted interpreter answers
`ChooseX` with an affordable value (e.g. all available generic mana, or a small
fixed amount) so games stay well-defined.

**Setup and decks.** `emptyGame` unchanged. `Setup` initializes `bindings =
Map.empty` where it set the retired fields. The benchmark stays on `redDeck`;
throughput is unaffected (`substituteX` is a one-pass list map; the binding
environment is the same `Map` lookups under a different value shape).

**Properties** (`runMatch`, all matchups): every M2d/M3a–M3g invariant as it
stands. Life "never increases" is unchanged (Blaze only decreases life).
Conservation, termination, id discipline, no floating mana at end of step. Replay
determinism now covers the `ChooseX` response.

## 7. What M4a preserves, and the expiries it opens

**Preserves:** the two invariants (§5), the mana/timestamp/projection models,
M3a's targeting and CR 608.2b re-validation, M3d's text-change binding (now read
through `Binding`), M3e's activation, M3f's event pipeline, M3g's control and
re-entrancy, and the deterministic-fixture posture (Blaze is the one M4a card in a
random deck).

**Expiries this milestone opens:**

- **The rest of the numeric tower.** `Star` (`*`, a layer-7a CDA — Tarmogoyf),
  `Plus` (`1+*`), `Half` (Little Girl), `Infinite` (Mox Lotus), and `Count`
  ("for each") stay shape-only; each is due with its first card. `Star` in
  particular is its own seam (a characteristic-defining ability read in layer 7a),
  likely a later M4 letter or a volume promotion.
- **X in a stored continuous effect.** `+X/+X` until end of turn must **freeze** X
  to a `Literal` when `Resolve` stores the `ModifyTarget` effect (the standing
  `Projection.hs:55` note). Due with the first such card; Blaze (one-shot) does
  not reach it.
- **X in activated-ability costs.** Deferred; rides M3g's `AbilityCost.mana` and
  the same `ChooseX` mechanism. Due with the first activated `{X}` (Stroke of
  Genius).
- **A general `Quantity.Bound SlotName`.** X uses a single reserved slot
  (`SlotName.variableX`). A *named* amount (a for-each count bound to a specific
  set, a second variable) generalizes to `Quantity.Bound SlotName` reading any
  slot's `amount`. Due with the first card that needs two amounts or a named one.
- **Engine-computed maximum X.** `ChooseX` names any `Natural` and payment rejects
  an unaffordable one. A UI/bot nicety (offer the affordable range) is deferred; it
  is not needed for correctness or determinism.
- **The `Binding` record shape is a "best-for-now" call, not load-bearing.** One
  map with a product record was chosen over a targets-separate sum because it most
  faithfully realizes "one general binding environment." If `Binding` accretes
  many mutually-exclusive `Maybe` fields as modes/for-each/kicker land, revisit
  whether a per-kind sum or a targets-separate cut reads better. The unification is
  the commitment; the exact record is revisitable.
- **git-bug `65ce714`** (mana-source prompt) stays open, unchanged — X substitution
  precedes source selection and does not touch it.

**Explicitly deferred to M4b–M4g and the volume tail:** zone-change verbs
(draw/destroy/bounce/mill/discard/exile, M4b), tokens (M4c), prevention and
regeneration (M4d), counter target spell (M4e), counters (M4f), modal (M4g), and
Fireball-style multi-target division. Blaze needs none of them.
