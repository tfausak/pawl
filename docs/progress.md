# Milestone log

pawl's comprehensive-rules core is built milestone by milestone (M0 → M7; the
full path and the M3a–M3g split table are in `docs/design.md` §3). This file is
the **completion log**: one distilled entry per landed milestone — its gate
card, the load-bearing decision it proved, the opcodes and types it added, and
the corrections worth remembering. It moved out of `CLAUDE.md` to keep that file
to working guidance. The authoritative per-milestone detail is each milestone's
spec and plan under `docs/superpowers/{specs,plans}/`, cited at the end of every
entry.

**This file records what each milestone *established*, not what is left to do.**
Outstanding work lives in GitHub Issues; each entry points at its spec's
deferral section for the full list and cites the issues for those deferrals that
have a live code site. Entries are not edited when an issue closes — a landed
milestone's record is history, and history does not change.

- **M0 is complete** (a full game of 60 Mountains vs. 60 Mountains, replaying
  deterministically). Its spec and plan are kept as reference:
  `docs/superpowers/specs/2026-07-15-m0-core-types-design.md` and
  `docs/superpowers/plans/2026-07-16-m0-engine.md`.
- **M1a is complete** (casting a Goblin Piker: mana, the stack, resolution).
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m1a-casting-design.md` and
  `docs/superpowers/plans/2026-07-16-m1a-casting.md`.
- **M1b is complete** (Pikers attack, block, deal damage simultaneously per CR
  510.2, and die). The design doc's M1 bundled two independent subsystems and was
  split into **M1a** (casting) and **M1b** (combat); see `docs/design.md`.
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m1b-combat-design.md` and
  `docs/superpowers/plans/2026-07-16-m1b-combat.md`.
- **M2a is complete** (the keyword seam plus flying, reach, defender, vigilance
  and haste — blocking/attacking legality through the `keywordsOf` projection).
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m2a-keywords-design.md` and
  `docs/superpowers/plans/2026-07-16-m2a-keywords.md`.
- **M2b is complete** (first strike + double strike and the CR 506.1 conditional
  turn structure: the turn is now data — `GameState.remaining` is the schedule
  `Engine.advance` pops — so the CR 508.8 skip is a drop and the CR 510.4 second
  combat damage step is a splice; `#19 is closed). Spec and plan kept
  as reference:
  `docs/superpowers/specs/2026-07-17-m2b-first-strike-design.md` and
  `docs/superpowers/plans/2026-07-17-m2b-first-strike.md`.
- **M2c is complete** (deathtouch + trample. Deathtouch is the first damage-event
  reader: `Damage.applyCombatDamage` is now a change-and-emit funnel recording
  `GameState.damageEvents`, and the CR 704.5h SBA (`Sba.woundedByDeathtouch`)
  destroys a wounded creature the SBA check then drains. Trample restructures
  assignment: `AssignCombatDamage` carries a keyword-agnostic `Map Recipient
  Natural` of lethal thresholds, `Damage.legalAssignment` is the CR 702.19b
  defender-gating implication, and CR 702.2c collapses a deathtouch source's
  threshold to 1 in one line of `Damage.blockerThreshold`. Zero opcodes). Spec and
  plan kept as reference:
  `docs/superpowers/specs/2026-07-17-m2c-deathtouch-trample-design.md` and
  `docs/superpowers/plans/2026-07-17-m2c-deathtouch-trample.md`.
- **M2d is complete** (M2c's black/green creatures are castable: `Swamp`/`Forest`
  basic lands, a `Deck` multiset (`Map Printing Natural`), and setup taking an
  explicit `NonEmpty (PlayerId, Deck)` matchup. The property suite runs over two
  matchups — red-red (unchanged) and green-black (alice green, bob black) — giving
  the 704.5h deathtouch SBA, trample assignment, and their CR 702.2c interaction
  random-game coverage; a deterministic test casts each card through the stack.
  No new rules, zero opcodes. `#23 is closed). Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-17-m2d-castable-decks-design.md` and
  `docs/superpowers/plans/2026-07-17-m2d-castable-decks.md`.
- **M3a is complete** (the first opcode — Lightning Bolt as data. A first-order,
  non-recursive `Effect` AST (`DealDamage SlotName Quantity`) referenced by named
  slots (`SlotName`), with `Pawl.Resolve` the *sole* module that may `case` on an
  `Effect` — executor plus `slotsOf`, the read half of the D4 dataflow lint that
  equates every printing's slot reads to its declared `targetSpecs`. `Pawl.Target`
  owns targeting legality (CR 115.4 `AnyTarget`), shared by casting and the CR
  608.2b re-validation. Casting prompts `ChooseTargets`, reject-not-repair, and
  stamps `Object.targets` on the new stack incarnation (reset by `changeZone`, CR
  400.7); `Cast` honors CR 117.1a instant speed. `Resolve.resolveSpell` runs the
  executor through the generalized `Damage.applyDamage` funnel, buries to the
  graveyard (CR 608.2n), and fizzles when every target is illegal (CR 608.2b). The
  priority loop checks state-based actions after each resolution (CR 117.5) and
  bails on a result. `Engine.runMatch`/`runMatchPure` derive the player list from
  the matchup — `#24 is closed — and four Lightning Bolts in
  `redDeck` give instant speed random-game coverage. Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-17-m3a-effects-design.md` and
  `docs/superpowers/plans/2026-07-17-m3a-effects.md`.
- **M3b is complete** (continuous effects: `Pawl.Projection` runs the CR 613
  single-effect layer fold over a type family — `Timestamp`, `Layer`,
  `Duration`, `Affected`, `Modification`, `ContinuousEffect`, `StaticAbility`,
  `ProjectedCharacteristics` — and `Effect.ModifyTarget` is ONE opcode driving
  both Giant Growth (layer 7c, +3/+3) and Serpent's Gift (layer 6, a deathtouch
  grant); no per-card opcode was added. Humility contributes two static
  abilities (layer 6 strips keywords, layer 7b sets base power/toughness to
  1/1), and CR 514.2 wears until-end-of-turn effects off at cleanup. The
  `DamageEvent.dealtByDeathtouch` deal-time bit (CR 702.2e) retires M2c's
  synthetic deathtrampler fixture in favor of real cards; `CreatureTarget` gives
  Serpent's Gift and Giant Growth a targeting spec; the green deck carries both
  for random-game coverage. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-18-m3b-continuous-effects-design.md` and
  `docs/superpowers/plans/2026-07-18-m3b-continuous-effects.md`.
- **M3c is complete** (the CR 613.8 *existence* dependency — the go/no-go for the
  whole continuous-effects approach, a genuine YES. `Pawl.Projection` folds a full
  projected **type line** (`cardTypes`, `subtypes`, `rulesTextActive` on
  `ProjectedCharacteristics`) layer by layer, evaluating each effect's affected-set
  against the *partial* projection, so a layer-4 type change is visible to a
  layer-6 grant. Three layer-4 `Modification`s (`SetLandSubtype`, `AddLandSubtype`,
  `AddCardType`), `Quantity.ManaValue` (CR 202.3), and `Supertype.Legendary` land
  with no new opcode. **The dependency is resolved by source-liveness, not trial
  application** (a plan refinement): `Projection.staticAbilitiesLive` gathers a
  permanent's static abilities only if no *live* `SetLandSubtype` applies to it,
  reading base characteristics so nothing recurses into the projection — Blood Moon
  strips Urborg order-**independently**, verified in both timestamp orders (the
  falsifier: a naive timestamp fold gives a Forest Swamp). Opalescence animates
  every *other* non-Aura enchantment; `setPT` now *establishes* P/T on a set (CR
  613.4b), so Humility becomes a 4/4 creature under it, resolved in both 7b orders.
  Mana taps off projected subtypes (CR 305.6/305.7); `Sba`/`Target`/`Combat` read
  projected creature-ness. Blood Moon, Urborg, and Opalescence are deterministic
  fixtures (no random-game entry, per the white/red-fixture posture). The
  topological CR 613.8b *applies-to* reorder is deferred behind a documenting test
  (`#11). Performance: `Projection.projectAll` projects the whole
  board from one `gather` per state-based-action sweep. Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-18-m3c-dependency-design.md` and
  `docs/superpowers/plans/2026-07-18-m3c-dependency.md`.
- **M3d is complete** (layer 3 — the rewritable effect AST, gated by Magical Hack;
  the M3 go/no-go for text-changing, a genuine GO. One new layer-3 `Modification`,
  `ChangeSubtypeWord from to` (CR 612, `Layer.Text`, sorts before layer 4 by the
  derived `Ord`), is read at **three** points, all through the projection so every
  consumer cashes out with no special case: (1) **the type line** — `Projection`'s
  own fold rewrites projected `subtypes` before layer 4, so a hacked basic Mountain
  taps `{U}` (CR 305.6); (2) **a source's static abilities** — `gather` rewrites the
  land-type words inside a live permanent's ability `Modification`s via
  `Projection.textChangesAffecting`/`rewriteModification` **before** they fold onto
  others, so hacking Blood Moon `Mountain→Island` makes nonbasic lands Islands
  **order-independently** (the part XMage cannot do — the go/no-go); (3) **a
  resolving spell's one-shot effects** — `Resolve.effectsOf`/`rewriteEffect` rewrite
  the AST at resolution (delegating the inner `Modification` to
  `Projection.rewriteModification`, so neither module touches the other's
  constructors), so a fixture `Landform` (`{U}` "target land becomes a Swamp",
  labeled synthetic crutch, spec §8) hacked `Swamp→Mountain` on the stack resolves
  as Mountain, and a Blood Moon *spell* hacked on the stack loses the change on
  resolution (CR 400.7 new object). The value choice binds at cast: `ChangeText
  SlotName` opcode + `ChooseBasicLandTypes` prompt + `Object.chosenSubtypes` store
  (reset by `changeZone`; serialized as `Response.ChoseBasicLandTypes` for replay),
  keeping `Resolve` pure — an elision justified by indistinguishability, expiry in
  §8. `Duration.Indefinite` (cleanup never drops it), `ToObject` recipient,
  `SpellOrPermanentTarget`/`LandTarget` specs, and `Subtype.Island`/`Plains` land
  with no rules-core casing on an effect's identity. Magical Hack and the fixture
  are blue deterministic fixtures (no random-game entry). Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-18-m3d-text-changing-design.md` and
  `docs/superpowers/plans/2026-07-18-m3d-text-changing.md`.
- **M3e is complete** (activated abilities as first-class objects on the stack,
  proving the CR 605 mana-ability ABI predicate). An `ActivatedAbility` (an
  `AbilityCost` of `AdditionalCost`s + reused `Effect`s + target slots) rides the
  stack as a `Source.OfAbility srcId ability` incarnation, minted by
  `Action.Activate` carrying the ability **value** (validated by membership in
  `Projection.abilitiesOf`, never an index). `Resolve.resolveAbility` runs it
  through the same executor and CR 608.2b fizzle as a spell — with the *source
  permanent* (not the ability object) as the effect source (CR 608.2g) — then the
  ability **ceases** (removed from stack + objects, CR 608.2n) rather than being
  buried. **The go/no-go — one ABI predicate, `Mana.isManaAbility`** (structure:
  produces mana via `Resolve.manaProduced` AND targets nothing, CR 605.1a) — is
  read at exactly two sites: `Mana.manaTypesOf` counts a mana ability as a source
  (resolved inline at payment, CR 605.3b, never the stack); `Activate.activatable`
  (hence `Action.legalActions`) excludes it from stack activations. Three real
  cards land the two branches: **Prodigal Sorcerer** `{T}: deal 1` (targets → the
  stack), **Llanowar Elves** `{T}: Add {G}` (the mana ability, inline, the
  falsifier an engine that stacked it would deadlock on), **Evolving Wilds** `{T},
  Sacrifice: Search` (fetches but adds no mana → the stack, the CR 605 false
  branch on a mana-adjacent card). Two opcodes: `Effect.AddMana ManaType` (a
  documented no-op in `applyEffect`; executed at payment) and `Effect.Search
  CardCriterion` (CR 701.23 tutor — prompts `SearchLibrary`, puts a
  `CardCriterion.BasicLandCard` onto the battlefield tapped, then shuffles;
  fail-to-find allowed, CR 701.23b), which forced resolution `Game`-monadic
  (`resolveSpell`/`resolveTop`/`applyEffect`, no behavior change) and a
  `Response.Searched` for replay round-trip. `AdditionalCost` = `TapSelf` (CR
  302.6 sickness-gates a *creature*'s `{T}`, never a land's) `| SacrificeSelf` (CR
  701.21). `abilitiesOf` is a projection (the `keywordsOf` move —
  `ProjectedCharacteristics.activatedAbilities`, emptied by layer-6
  `LoseAllAbilities`), the single switch that makes Humility strip a creature's
  activated **and** mana abilities. `Action`/`Source` are now `data` (both derive
  `Ord`). Cards are deterministic fixtures (no random-game entry). #12 (payCost must prompt when mana sources are distinguishable) stays
  **open** by design — the mana-source elision expires at the first dual land, not
  here. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-18-m3e-activated-abilities-design.md` and
  `docs/superpowers/plans/2026-07-18-m3e-activated-abilities.md`.
- **M3f is complete** (the event pipeline — triggered abilities (CR 603) and
  replacement effects (CR 614) on one substrate, the zone-change event. Not the
  M3 go/no-go (that verdict arrived at M3d); it cashes two of the three ABI
  decisions M3 owes beyond the gate cards — the event "atom" pattern and effect ≡
  event. **Rest in Peace** (`{1}{W}` Enchantment, "when this enters, exile all
  graveyards / if a card or token would be put into a graveyard from anywhere,
  exile it instead") is the whole-card gate: every clause is one zone change.
  `Event.changeZone` becomes a first-class change-and-emit funnel (generalizing
  M2c/M3b's `Damage.applyDamage`): it consults active replacement effects and
  rewrites the event's destination *before* moving (CR 614.1a — a graveyard-bound
  object redirected to exile), then emits the *resolved* event into
  `GameState.zoneChanges`; at every CR 117.5 priority boundary, abilities whose
  `TriggerCondition` matches an emitted event are placed on the stack (as a
  `Source.OfTrigger` incarnation), resolve through the M3e executor, and cease (CR
  603). One decision is forced by the gate: Rest in Peace must catch a creature
  killed by a **state-based action**, and `Sba.checkStateBasedActions` is pure —
  so the replacement seam applies **purely** (`changeZone` stays `GameState ->
  GameState`, consulting a purely-computed rewrite). `Pawl.Event` is the sole
  `case`-on-`ReplacementEffect`/`TriggerCondition` home (the `Resolve`-for-`Effect`
  move — the closed half reads a *classification*, never the identity). New leaves:
  `ZoneChange`, `ReplacementEffect.RedirectZoneChange`, `TriggeredAbility` /
  `TriggerCondition.SelfEnters`, `Source.OfTrigger`; one opcode
  `Effect.ExileAllGraveyards` (CR 701.10, Rest in Peace's bulk exile). The CR
  117.5 settle is a cheap fixpoint guard that re-scans only on board changes.
  Headline elision: the general **monadic** replacement path (multiple racing
  replacements, CR 616's affected-player ordering prompt, replacements that
  require a choice) — Rest in Peace is a single deterministic replacement with
  nothing to prompt. Rest in Peace is a white deterministic fixture (no
  random-game entry). Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-18-m3f-event-pipeline-design.md` and
  `docs/superpowers/plans/2026-07-19-m3f-event-pipeline.md`.
- **M3g is complete** (the payoff pair — a player-controlling *Decider* and
  cast-during-resolution *re-entrancy*, the two seams the day-one substrate was
  built for). **Mindslaver** (`{6}` Legendary Artifact, `{4}, {T}, Sacrifice:
  You control target player during that player's next turn`) is CR 723 control as
  a turn-scheduled store: a new `Effect.ControlPlayerNextTurn SlotName` opcode
  installs `GameState.pendingControl :: Map PlayerId Decider` (keyed to the chosen
  `PlayerTarget`, valued by the ability's controller — CR 723.5); `Engine
  .handoffTurn` promotes the new active player's pending entry to
  `GameState.activeControl :: Maybe Decider` and deletes it (CR 723.1b), so
  overwriting `activeControl` every turn start is what expires a prior control (CR
  723.1); and `Decide.deciderFor` reads that store — the *single* switch that
  routes the controlled player's decisions to the controller while every resource
  (mana, cards, life) stays the controlled player's (CR 723.3/723.5a), gated by
  `pid == activePlayer` so one `Maybe` suffices. **Panglacial Wurm** (`{5}{G}{G}`
  9/5 Trample, "while you're searching your library, you may cast this from your
  library") is the re-entrant cast: a `CastingPermission.CastFromLibraryWhileSearching`
  classification on `Card.castingPermissions` (read from the card in the library,
  NOT the projection — CR 613 does not reach the library, CR 113.6), a
  `Cast.castWhileSearching` loop offering `Prompt.CastWhileSearching` and calling
  `Cast.castSpell` mid-resolution. Because `Cast`/`Mana` import `Resolve` (so
  `Resolve` sits *below* them and cannot call `castSpell`), the offer is
  orchestrated one layer up in `Stack.resolveTop` — which asks
  `Resolve.searchesLibrary` before resolving an ability and, since the ability is
  still on the stack, lands the cast *on top* of it (the ruling's sequence). New
  types/fields: `CardType.Artifact` (CR 301, first artifact), `TargetSpec
  .PlayerTarget` (CR 115), `Effect.ControlPlayerNextTurn`, `CastingPermission`,
  `AbilityCost.mana :: Maybe ManaCost` (the mana half of activation costs, CR
  602.1b — Mindslaver's `{4}` is the first `Just`; `Activate` checks and pays it),
  `Prompt.CastWhileSearching` + `Response.CastWhileSearched` (replay round-trip).
  `Resolve` grows `ControlPlayerNextTurn` and `searchesLibrary` arms but stays the
  sole `case`-on-`Effect` home; `Cast` is the sole reader of `CastingPermission`
  (`permitsCastWhileSearching`, a membership test). Named elisions: the legend rule
  CR 704.5j is **elided** (Mindslaver is singleton and sacrificed as a cost; it
  must land suppressible — Mirror Gallery); CR 723.4 information visibility is a
  `PlayerView` concern (no `PlayerView` yet); CR 723.2 limited-duration control is
  future. Cards are deterministic fixtures (no random-game entry). Spec and plan
  kept as reference:
  `docs/superpowers/specs/2026-07-19-m3g-decider-reentrancy-design.md` and
  `docs/superpowers/plans/2026-07-19-m3g-decider-reentrancy.md`.
- **M3.5 is complete** (cards as data files — the interstitial that cashes §2.7,
  "cards are runtime data, never Haskell modules." Zero opcodes, zero rules: a
  representation change and a relocation, sequenced before M4 so every M4 opcode
  is born serializable. **The proof is the honesty round-trip** — `jsonToCard .
  cardToJson ≡ Right` for every card — made progressively load-bearing (P1 value,
  P2 through text, P3 files re-parse and re-render byte-stable), which fails
  loudly the day a closure is smuggled into the card model. A hand-rolled JSON
  layer ported from `_scratch/scrod` and reconciled to pawl's rules:
  `Pawl.Type.Decimal` (mantissa/exponent, no `Double`), `Pawl.Type.Json` (scrod's
  seven-newtype split flattened to one `Value` sum; `Object` an ordered assoc list
  so render is canonical by construction), and `Pawl.Json` (`parse`/`render` via
  `bytestring`/`parsec`, both new **boot-lib** deps, no `aeson`). `Pawl.Codec` is
  the **sole** `Card ⇆ Json` authority — free `xToJson`/`jsonToX` values, no type
  classes — over the transitive closure of `Card`'s fields; casing on an effect's
  identity there is open-half serialization machinery, exactly as `Pawl.Resolve`
  may `case` on `Effect`. The §2.12 **tagged-sum discipline** is the central risk
  it retires: P/T and mana are not `Int`, so every sum serializes as a tagged
  object (`{"type":"Literal","value":5}`) and only genuine cardinals become bare
  JSON numbers. **The relocation makes the invariant a build-level fact**: the ~30
  hand-written card values and the decks left the engine library for the test
  suite's `Pawl.Cards`; `Pawl.Card` keeps only classifications (`isLand`, …),
  `Pawl.Setup` only mechanism — the closed half can no longer *name* a card, so §1
  is enforced by the module graph, not discipline. **Files are the source of
  truth**: every card renders to a committed `data/cards/<slug>.json`, the
  hand-written `MkCard` literals are **deleted**, and both non-engine consumers
  read the files — both by true `IO` loading. The benchmark loads only the cards
  its decks name; the test suite loads the whole pool once in `main` and threads a
  `Pawl.Cards.Cards` record into every `tests` tree (the decks and `allPrintings`
  are functions of it). **The TH shim's named expiry was cashed immediately**: the
  milestone first shipped a compile-time Template Haskell shim (`Pawl.Cards.Load`
  splicing `Lift`-derived `Printing`s) to keep the fixtures pure without churning
  ~15 files, then — before M4 — converted the test suite to `IO` loading like the
  benchmark and deleted the shim, `TemplateHaskell`, and the per-module `DeriveLift`
  (so the **engine library no longer links `template-haskell`** — the closed half
  is back to boot-libs-for-real-reasons). (Two other expiries stay open: `Either
  Text` decode errors may become path-aware for M6 transpiler output, and the
  `Card`-granularity codec extends to `Printing`-granularity when `Printing` grows
  metadata.) The engine library stays pure (all file reading is the benchmark's
  and the test suite's `IO`); the codec is total (`Either`, never a partial
  `head`). Note: the plan's `git grep 'MkCard' == nothing` check is an over-broad
  proxy for "the pool literals are gone" — two pre-existing inline unit-test
  fixtures (synthetic "Some Instant"/"T" cards, no JSON files) correctly remain;
  `Pawl.Cards` itself names no card literally (it loads them all from disk). Spec
  and plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m3.5-cards-as-data-files-design.md` and
  `docs/superpowers/plans/2026-07-19-m3.5-cards-as-data-files.md`.
- **M4a is complete** (the numeric tower's `X` on a general binding environment —
  the first letter of M4, leading because X is upstream of every opcode. **Gate
  card: Blaze** (`{X}{R}` Sorcery, "Blaze deals X damage to any target"), chosen
  because it falsifies its own naive implementation on exactly one axis: X is
  *chosen at cast* (CR 601.2b) and *re-read at resolution* (CR 608.2b), the same
  late-binding shape M3a used for targets. An engine that baked a literal at cast,
  or treated `X` as 0 or as the `{X}` mana value, deals the wrong amount — the
  falsifier is a code comment on the gate test. **Landed in two ordered phases.**
  *Phase 1 (behavior-preserving refactor)*: the two parallel `Object` choice-maps
  (`targets`, M3a; `chosenSubtypes`, M3d) unify into one `Object.bindings ::
  Map SlotName Binding` — the risk-register's D4 "named binding slots," generalized
  when X arrived as the **second customer**. `Pawl.Type.Binding` is a product
  record (`target`/`subtypes`/`amount`, each `Maybe`) so one slot can carry several
  kinds of choice at once (Magical Hack's slot is both targeted and word-swapped);
  `Pawl.Binding` holds the logic — projections `targetsOf`/`subtypesOf`/`amountOf`,
  the write-site `fromChoices`, and the reserved slot `variableX`. Every reader
  migrated to a projection; the M3a–M3g suite (unchanged) is the regression net,
  green before and after. *Phase 2 (X)*: `Quantity.X` (evaluated by
  `Quantity.evaluate` against the source object's `bindings` at `variableX`);
  `ManaSymbol.Variable` (the `{X}` symbol, contributing 0 off the stack per CR
  202.3b); `Mana.substituteX` (CR 601.2f — each `Variable` becomes `Generic n`,
  order preserved); `Prompt.ChooseX`/`Response.ChoseX` (replay-serialized so a
  variable-cost cast replays deterministically); and `CardType.Sorcery` (first
  sorcery printing; not a permanent, so it resolves to the graveyard). `Cast
  .castSpell` prompts X first (CR 601.2b precedes 601.2c) and only when the cost
  carries a `Variable`, pays the substituted cost, and stamps the chosen value into
  `bindings`. The **castability floor** is `substituteX 0` (a caster may always
  choose X=0, so Blaze is offered whenever `{R}` is affordable; the actual X is
  gated at payment, reject-not-repair). The **D4 lint generalizes to the value
  half**: `Resolve.readsX` — a card reads `X` iff it declares `{X}` in its cost —
  with the reserved X slot exempt from the target-slot reads-equal-declares
  equality. **Invariants preserved**: `Pawl.Resolve` stays the sole home of
  `case effect of`/`case quantity of`; X is a genuine player choice and is
  prompted (never elided). **Deck note**: Blaze joins `redDeck` for random X-cost
  coverage by swapping in for four Goblin Pikers, keeping the deck at 60 (so the
  CR 400.7 conservation counts stay 120) — a deliberate deviation from the plan's
  literal "add four Blazes" (which would have grown the deck to 64), taken by user
  direction when the gap surfaced. **Named expiries opened**: spec §7 carries the
  list; those with a live code site are filed under the `elision` label, notably
  #14 (`Quantity.Bound SlotName`). #12 (mana-source prompt) is unchanged — X
  substitution precedes source selection. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m4a-numeric-tower-binding-design.md` and
  `docs/superpowers/plans/2026-07-19-m4a-numeric-tower-binding.md`.
- **M4b is complete** (the targeted zone-change opcode family. **Gate: Murder**
  (`{1}{B}{B}` Instant, "Destroy target creature") vs. **Darksteel Myr** (`{3}`
  0/1 Artifact Creature, Indestructible): the falsifier is that modelling destroy
  as a move-to-graveyard buries the Myr, so `Effect.Destroy` is its own opcode
  that consults CR 700.4 indestructibility before funnelling to the graveyard.
  Five opcodes, all executed only by `Resolve.applyEffect` through M3f's
  `Event.changeZone` funnel — proving it generalizes past Rest in Peace's single
  redirect into card-driven verbs: `Destroy SlotName`; `MoveToZone SlotName Zone`
  (Unsummon bounces to Hand, Angelic Edict exiles); `Draw Quantity` (Divination,
  controller-targetless, empty-library loss preserved via the consolidated
  `Event.drawCard`); `Mill SlotName Quantity` (Tome Scour, target player, short
  library mills fewer with no loss, CR 701.13b); `Discard SlotName Quantity`
  (Mind Rot, reusing `Prompt.ChooseDiscard`, the discarding player choosing per
  CR 701.8a). **`Keyword.Indestructible`** (CR 702.12) is read through the
  projection at two independent sites — the `Destroy` opcode and `Sba.creatureDies`
  (guarding CR 704.5g lethal damage and 704.5h deathtouch but **not** 704.5f
  toughness ≤ 0, which is a put-into-graveyard, not a destruction) — so Humility
  strips it for free. New types: `TargetSpec.CreatureOrEnchantmentTarget` (the
  first spec admitting a non-creature permanent, exercised by exiling an
  enchantment), `Subtype.Myr`. The single-card draw was consolidated into one
  `Event.drawCard` shared by the draw step, opening hands, and the Draw opcode.
  Seven cards (Darksteel Myr, Murder, Unsummon, Angelic Edict, Divination, Tome
  Scour, Mind Rot), Scryfall-verified, deterministic fixtures, with a fast-follow
  random-game matchup (blue/black) carrying the high-value verbs. **Named
  elisions/expiries**: Destroy is a plain check-then-move, not yet an interceptable
  destroy event — regeneration/prevention (CR 615) makes it replaceable at **M4d**;
  the 704.5f toughness-drop test uses a synthetic `-0/-1` continuous effect until
  the first real **−1/−1** ability; a forced full-hand discard is elided (not
  prompted) per the engine-makes-no-choices rule; derived references ("its
  controller"/"its power", Path/Swords) and a lifegain opcode are deferred with the
  first card that needs them; `MoveToZone slot Graveyard` (an unconditional
  put-into-graveyard, distinct from Destroy) awaits its first card. Spec and plan
  kept as reference:
  `docs/superpowers/specs/2026-07-19-m4b-zone-change-verbs-design.md` and
  `docs/superpowers/plans/2026-07-19-m4b-zone-change-verbs.md`.
- **M4c is complete** (tokens — the first card-less game object. **Gate: Dragon
  Fodder** (`{1}{R}` Sorcery, "Create two 1/1 red Goblin creature tokens"):
  casting it puts two distinct 1/1 Goblin tokens on the battlefield, read through
  the ordinary projection/combat/SBA pipeline. The decision it proves is that a
  permanent whose characteristics come from an effect rather than a printing flows
  through the whole engine with **no special case**: a token is a `Card` with no
  `Printing`, carried by **`Source.OfToken Card`** and returned by the single
  `Game.cardOf` chokepoint, so every downstream reader (projection, mana, combat,
  state-based actions) is unchanged. Minting is **`Event.createToken`** — a
  `changeZone` sibling that materializes an object from nothing (owner = creator,
  CR 111.2; summoning-sick, CR 302.6) and emits its enters event through the same
  path a resolved permanent uses; the shared materialize-and-emit tail was
  extracted as `Event.placeObject`. The opcode is **`Effect.Create Quantity card`**,
  executed only by `Resolve.applyEffect` (folding `createToken` per the count) with
  its five classifications (`slotsOf`/`readsX`/`manaProduced`/`searchesLibrary`/
  `rewriteEffect`) and a `Codec` arm serializing the nested token `Card` (the
  `allPrintings` honesty round-trip now covers it). A new state-based action
  (**CR 704.5d**) removes any `OfToken` object found off the battlefield — a direct
  delete, not a zone change, keyed to "not on the battlefield" so exile is caught
  too; a 1/1 token taking lethal combat damage is buried then ceases, never
  lingering in the graveyard (the falsifier). **Rest in Peace composes for free**:
  a dying token funnels through `changeZone`, gets redirected graveyard→exile by
  M3f's existing replacement, and CR 704.5d finishes it there — zero new code.
  **Architectural decision (supersedes the plan's Task 3):** a concrete
  `Effect.Create Card` would make `Effect` and `Card` mutually import each other
  (`Card` embeds `[Effect]`) — a module cycle. Rather than an `.hs-boot` or an
  effect-free `TokenSpec` (which would fail the moment a copy-effect needs a card's
  full characteristics, abilities included), **`Effect`, `ActivatedAbility`, and
  `TriggeredAbility` were made parametric over the card type**, with `Card` tying
  the knot at `Effect Card` / `ActivatedAbility Card` / `TriggeredAbility Card`.
  None of the three import `Card`, so no cycle; the DSL stays first-order and
  non-recursive in control flow (design.md §1 — the recursion is structural data
  nesting, never a recursive call). This generalizes to the future copy-token
  opcode. No `Subtype` change (Goblin already existed). One card (Dragon Fodder,
  Scryfall-verified), swapped 4-for-4 into the red deck (stays 60) for random
  token-churn coverage; the conservation property now counts **card-backed**
  (`OfCard`) objects (still 120) since tokens legitimately create and destroy
  objects. **Named elisions/expiries**: `createToken` does not consult replacements
  on entry (Doubling Season is future); `rewriteEffect` is the identity on `Create`
  (a text-changer does not reach a token's embedded card yet); `OfToken` carries no
  physical-token metadata (`Maybe Printing`) until `Printing` grows any; copy-tokens
  (CR 707) and predefined tokens (CR 111.10) are not modelled — the characteristics
  are given, not derived. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m4c-tokens-design.md` and
  `docs/superpowers/plans/2026-07-19-m4c-tokens.md`.
- **M4d is complete** (the two remaining replacement-shield shapes: damage
  **prevention** and **regeneration**. **Gates: Fog** (`{G}` instant, "Prevent all
  combat damage this turn") for the cancel shape, and **Drudge Skeletons**
  (`{1}{B}` 1/1, "`{B}`: Regenerate this creature") for the destruction-replace
  shape. The decisions proved are two: (1) **prevention is a cancel hooked into the
  head of the damage funnel** — a prevented event never happens (not marked, not
  drained, never emitted), distinct from M3f's zone-change *redirect* shape; and
  (2) **every destruction flows through one `Event.destroy` funnel** that a one-shot
  regeneration shield can replace. **Phase 1 (prevention):** each `DamageEvent`
  gains a **`DamageKind`** (`Combat`/`Noncombat`, a no-boolean-blindness tag set at
  deal time — `Damage` tags Combat, `Resolve`'s `DealDamage` tags Noncombat); a
  floating **`GameState.preventions :: [ActivePrevention]`** store (the event-pipeline
  analog of `continuousEffects`) holds **`ActivePrevention {prevention, duration}`**
  over a leaf **`Prevention`** family (`PreventAllCombatDamage`); **`Event.applyPreventions`**
  — the sole caser on `Prevention` — drops each combat event a shield cancels at the
  head of `Damage.applyDamage`, and **`dropEndOfTurnPreventions`** is the CR 514.2
  wear-off wired into cleanup. The opcode is **`Effect.Prevent Duration Prevention`**
  (targetless), with the five `Resolve` classifications, a `Codec` `Prevention`
  arm, and Fog's honesty round-trip. The **`DamageKind` falsifier**: after Fog
  resolves, a Combat event is cancelled while a Noncombat (spell) event still lands.
  **Phase 2 (regeneration):** a per-object **`GameState.regenerationShields ::
  Map ObjectId Natural`** (activating twice stacks two; each destruction consumes
  one; cleared at cleanup) is installed by **`Effect.RegenerateSelf`** (targetless,
  self-referential — the shield fires later, NOT the act of regenerating).
  **`Event.destroy`** is the single destruction chokepoint: CR 700.4 indestructible
  → no-op; CR 701.19a shield → consume one, heal damage, tap, remove from combat,
  stay on the battlefield (same id); else `changeZone Graveyard` (so Rest in Peace's
  redirect and CR 704.5d cease-to-exist still compose). The `Destroy` opcode is
  rewired to it, and the creature-death SBA **splits**: `Sba.zeroToughness` (CR
  704.5f, a put-into-graveyard, ungated and un-saveable) routes through `changeZone`,
  while `Sba.destroyedBySba` (CR 704.5g/h, a destruction) routes through
  `Event.destroy` — so a shield saves a creature from lethal combat damage but never
  from toughness ≤ 0. **Module-cycle discipline:** `Event.destroy` removes a
  permanent from combat by editing `GameState.combat` through the *type* module
  `Pawl.Type.Combat` (never `Pawl.Combat`, which imports `Pawl.Sba` → `Pawl.Event`
  and would cycle); `Pawl.Damage` importing `Pawl.Event` is acyclic. New subtype
  **`Subtype.Skeleton`**. Two cards (Fog, Drudge Skeletons), Scryfall-verified,
  swapped 4-for-4 into the green and black decks (each stays 60; card-backed
  conservation stays 120 — regeneration keeps the same object, no mint). The
  negative test: a shielded creature bounced by Unsummon still leaves (regeneration
  intercepts destruction, not every leave-the-battlefield). **Named
  elisions/expiries**: spec §7 carries the list — CR 701.19c "can't be
  regenerated" (#42), CR 615.7 amount-shields and the multi-source choice plus CR
  615.13 prevented-event triggers (#58), CR 615.10 static prevention, a general
  "Regenerate target creature", CR 701.19b static regeneration, and a distinct
  "was destroyed" event — each due with the first card that needs it. Spec and
  plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m4d-prevention-regeneration.md` and
  `docs/superpowers/plans/2026-07-19-m4d-prevention-regeneration.md`.
- **M4e is complete** (counter target spell — the first effect that removes a spell
  from the stack. **Gate: Cancel** (`{1}{U}{U}` Instant, "Counter target spell"):
  the falsifier is a Cancel whose target left the stack before it resolves, which
  must **fizzle** (CR 608.2b) — a path M3a's resolution-time re-validation already
  builds, so the milestone proves the seam rather than rebuilding it. **One opcode**
  `Effect.Counter SlotName` — a distinct keyword action (the M4b `Destroy`
  precedent; **Counter is CR 701.6**, not 701.5 which is "Cast") — executed by
  `Resolve.applyEffect` through a **new `Event.counter` funnel** (CR 701.6a: remove
  from the stack, put into the owner's graveyard via `changeZone` — so Rest in
  Peace's redirect and CR 400.7's new incarnation compose for free, verified by a
  RiP-exile test; ungated, mirroring `Event.destroy`). **One target spec**
  `TargetSpec.SpellTarget` (CR 115 "target spell" — stack objects that are spells
  only), read via the new **`Game.isSpell`** classification: an object is a spell
  iff it is **on the stack** (`Object.zone == Stack`) **and** `Source.OfCard`
  (**CR 112.1**, not 111.1 which is Tokens) — a classification of the object's kind,
  not a card's identity, like `Card.isPermanent`. The zone check keeps the function
  honestly named rather than merely correct because `Target` pre-filters to the
  stack (a review catch). Cancel is a **blue deterministic fixture** (no random-game
  deck, the M3d posture); `cancel.json` joins `allPrintings` for the honesty
  round-trip. Gate + falsifier are gameplay-level tests through the cast/resolve
  pipeline (the racing-counters scenario proves a spell can target a spell and that
  the second Cancel fizzles when its target is gone, moved exactly once).
  `Pawl.Resolve` stays the sole `case effect of` home; `Event` the sole funnel home;
  `Target` the sole targeting-legality home. **No new prompt/response** (Cancel
  targets through the existing `ChooseTargets`; countering is unprompted). **Named
  elisions/expiries**: spec §7 carries the list — `Event.counter` is ungated for
  "can't be countered" (CR 701.6) and emits no distinct "was countered" event
  (#43); conditional counters, countering **abilities** (Stifle), alternative
  counter destinations (Remand), and restricted counters are each due with the
  first card that needs them. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m4e-counter-spell-design.md` and
  `docs/superpowers/plans/2026-07-19-m4e-counter-spell.md`.
- **M4f is complete** (counters — +1/+1 and −1/−1 as **persistent permanent
  state**, the first P/T source that is neither a printed value nor a durational
  continuous effect. **Gates: Battlegrowth** (`{G}` Instant, "Put a +1/+1 counter
  on target creature") and **Instill Infection** (`{3}{B}` Instant, "Put a −1/−1
  counter on target creature. Draw a card"). The decision proved is that counters
  live as **typed per-kind counts**, forced by the CR 704.5q / 122.3 falsifier: a
  permanent with both a +1/+1 and a −1/−1 counter has N of each removed by a
  state-based action, which a net-`Integer` P/T model cannot represent — so the
  data model must keep counts per kind. **A rules correction:** design.md's M4f row
  said "layer 7d"; **CR 613.4c** puts counters in **layer 7c** (the same sublayer
  as Giant Growth — 7d is P/T *switching*), so **no new `Layer` constructor** was
  added and, 7c being purely additive, pre-combining a permanent's counters into
  one net delta and using the object's own timestamp are both unobservable (a
  theorem, not an elision). New type **`Pawl.Type.CounterKind`** (`PlusOnePlusOne |
  MinusOneMinusOne`, `Ord` load-bearing as a `Map` key); new **`Object.counters ::
  Map CounterKind Natural`** field — per-incarnation state reset by `changeZone`
  (CR 122.2: counters "cease to exist" on a zone change) and, unlike `damage`, NOT
  cleared at cleanup (a counter is not "until end of turn"). One opcode
  **`Effect.PutCounters CounterKind Quantity SlotName`** (CR 122.6), executed only
  by `Resolve.applyEffect` as an in-place `Map.insertWith (+)` — NOT a zone change,
  so it never routes through `Event.changeZone` — with its five `Resolve`
  classifications (the `Quantity` evaluated against the resolving `source`, like
  `DealDamage`, so a future `X`-counter card works unchanged) and a `Codec` arm.
  The projection reads counters through a new **`Projection.counterGathered`** that
  emits each battlefield object's net counter delta as one synthetic layer-7c
  `ModifyPowerToughness`, appended as `gather`'s third source (`stored ++ static_ ++
  counterGathered`) so it folds through the existing path alongside Giant Growth;
  `addPT`'s `(Nothing, _) → Nothing` means counters on a non-creature yield no P/T
  (CR 122.1a). The **CR 704.5q / 122.3 annihilation SBA** is a new arm in
  `Sba.performStateBasedActions` (candidates read from the incoming state for CR
  704.4 simultaneity, N = `min`, the edit folded onto the threaded final state;
  `removeN` deletes a key at zero so a balanced permanent produces no candidate and
  the CR 704.4 settle loop terminates; net P/T is preserved, so annihilation can
  neither cause nor prevent a death). **The falsifiers land as gameplay tests:**
  Battlegrowth's +1/+1 **persists through cleanup** where an equal Giant Growth
  wears off (the counter/continuous-effect line); enough −1/−1 counters drop a
  creature to toughness ≤ 0 and it dies via CR 704.5f — which **retires the
  synthetic −0/−1 continuous-effect fixture** M4b/M4d used as a −1/−1 stand-in
  (their named expiry, cashed); and both kinds on one creature annihilate to the
  correct remainder. Battlegrowth (green) and Instill Infection (black) swap
  4-for-4 into the green/black decks (each stays 60; card-backed conservation stays
  120 — a counter mints no object), giving the `greenBlack` matchup random counter
  coverage; both cards' JSON joins `allPrintings` for the honesty round-trip.
  `Pawl.Resolve` stays the sole `case effect of` home; `Pawl.Projection` the sole
  `case … Modification` home. **No new prompt/response** (both cards target through
  the existing `ChooseTargets`; putting counters is unprompted, CR 122.6). **Named
  deferred expiries:** spec §9 carries the list — non-P/T counter kinds, a
  "counter placed" event, replacements that alter counter placement, counters
  entering *with* a permanent, "move a counter", the "can't have more than N
  counters" SBA, and `Quantity.X`-many counters — each due with the first card
  that needs it; those with a live code site are filed under the `elision` label.
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-20-m4f-counters-design.md` and
  `docs/superpowers/plans/2026-07-20-m4f-counters.md`.
- **M4g is complete** (modal — the seventh and final M4 letter: a choice at cast
  binds which effects and targets apply. **Gate: Chaos Charm** (`{R}` Instant,
  "Choose one — Destroy target Wall. / Chaos Charm deals 1 damage to target
  creature. / Target creature gains haste until end of turn."), three modes under
  a single `ChooseExactly 1` (CR 700.2). **Falsifier: CR 700.2c/601.2c** — the
  Wall-destroy mode's target namespace must stay isolated from the other two
  modes': with no Wall on the board Chaos Charm is still castable via its
  damage/haste modes, and casting the damage mode must bind only the `creature`
  slot, never `wall`. The decision proved is that **modality is a shared,
  Card-free parametric payload** — four new types, `Pawl.Type.ModeIndex`
  (a `Natural` ordinal; CR 608.2c resolution order and CR 700.2d/g's "chosen
  twice"/copy semantics make the ordinal load-bearing, not incidental),
  `Pawl.Type.ModeSelection` (`ChooseExactly Natural` today, a sum so `ChooseAtLeast`/
  escalate/pawprint can join later without primitive blindness), `Pawl.Type.Mode`
  (one option's own `effects :: Seq (Effect card)` and `targetSpecs :: Map SlotName
  TargetSpec`), and `Pawl.Type.Modal` (`modes :: Seq (Mode card)` plus the
  `selection`) — parametric in `card` exactly like `Effect`/`ActivatedAbility`
  (M4c), so activated and triggered abilities can adopt the same payload as a
  **named fast-follow** without a module cycle; only `Card.spell :: Modal Card`
  was wired this milestone (a non-modal card, i.e. every card before M4g, is one
  mode with `ChooseExactly 1`, forced and unprompted — the two-phase,
  behavior-preserving reshape of `Card`'s flat `effects`/`targetSpecs` into
  `spell`, verified against the existing suite before Chaos Charm's own tests were
  added, plus a structural-then-canonical migration of the 44 pre-existing
  `data/cards/*.json` files through the codec). New reader surface on
  `Pawl.Card`: `allEffects`/`allTargetSpecs` (whole-card views, e.g. the
  X-declaration lint) and the mode-scoped `modeTargetSpecs`/`modesEffects`/
  `modesTargetSpecs`; the **D4 dataflow lint itself moved per-mode** (equality of
  each mode's own read slots against its own declared slots, `Mode.effects`
  against `Mode.targetSpecs`, not a flattened whole-card view). **One new binding slot**: `Binding.modes :: Maybe (Set
  ModeIndex)`, stored only under the reserved `Pawl.Binding.chosenModes` slot and
  read back by `modesOf` (CR 700.2/601.2b — a `Set` because CR 700.2d's "same mode
  twice" is future work, not yet a multiset). **One new prompt/response pair**:
  `Prompt.ChooseModes`/`Response.ChoseModes`, wired through `Replay` (deterministic
  replay picks the lowest-indexed legal modes) and every test-suite prompt
  answerer, including `randomAnswer` (so random games now exercise the modal
  choice). **Castability**: `Pawl.Cast.fillableModes` (CR 700.2a — a mode is
  fillable only if every one of its slots has a legal recipient; illegal targets
  exclude the whole mode, never a partial choice) and `targetable` generalized to
  "at least as many modes fillable as the selection demands" (identical to "every
  slot fillable" for a non-modal card, so M3b's Giant Growth falsifier is
  unchanged). **Resolution**: `Pawl.Resolve.effectsOf`/`resolveSpell` re-scoped to
  read and re-validate only the *chosen* modes' effects and slots (an unchosen
  mode's effects never resolve, CR 608.2c; its fizzle check per CR 608.2b is
  likewise chosen-modes-only). **New Wall machinery**, the falsifier's own
  prerequisite: `Subtype.Wall` (CR 205.3m, a creature type) and
  `TargetSpec.WallTarget` (CR 115.1a/700.2c — a creature whose *projected*
  subtypes include Wall), with Wall of Stone (0/8 Defender) as a **deterministic
  fixture only** (not in any random-game deck) giving `WallTarget` a legal
  recipient to destroy. Chaos Charm swaps 4-for-4 into the red deck for four Bird
  Maidens (deck stays 60; card-backed conservation stays 120), with Pikers and the
  remaining Bird Maidens on board so its damage/haste modes have legal targets in
  random games — `randomAnswer`'s `ChooseModes` arm now drives real modal choices
  in the random `redRed` matchup (mode 0, "destroy target Wall," is never legal —
  no Wall in any deck — so it offers `{1,2}`).
  `Pawl.Resolve` stays the sole `case effect of` home; `Pawl.Cast` cases on
  `ModeSelection` as an orchestration tag, the same posture it already has for
  timing, never on a card's identity. **Named deferred expiries:** spec §13
  carries the list; those with a live code site are filed under the `elision`
  label. Modality on **activated and triggered abilities** was the design's own
  stated intent (§0 of the spec) and the immediate next step rather than a
  deferral — it landed as M4h. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-20-m4g-modal-design.md` and
  `docs/superpowers/plans/2026-07-20-m4g-modal.md`.
- **M4h is complete** (the M4g fast-follow: modality on activated and triggered
  abilities). **Gate: Aether Channeler** (`{2}{U}` Creature — Human Wizard, "When
  this creature enters, choose one — Create a 1/1 white Bird creature token with
  flying. / Return another target nonland permanent to its owner's hand. / Draw a
  card."), a `ChooseExactly 1` `Modal` on its `SelfEnters` (M3f) trigger — the
  first *targeted* triggered ability the engine has (no triggered ability targeted
  before this). The decision proved is that **modality is a payload-level property,
  and M4g built it Card-free and parametric precisely so both ability types could
  adopt it with no new module cycle**: `ActivatedAbility`/`TriggeredAbility`
  reshaped to `{cost | condition, modal :: Modal card}`, retiring M4g's documented
  interim divergence (abilities' `effects :: [Effect card]`) now that `Mode.effects
  :: Seq` is the one shape everywhere. **Zero opcodes** — Aether Channeler's three
  modes are `Create` (M4c), `MoveToZone Hand` (M4b), and `Draw` (M4b), the same
  "compose existing verbs under a modal wrapper" posture Chaos Charm set. The
  trigger-only novelty a spell has no analog for is **CR 603.3c/700.2b**: a modal
  triggered ability is *placed on the stack first*, and only removed after if no
  mode can be legally chosen — where an uncastable modal spell is simply never
  offered. New machinery: `Pawl.Modal`, a shared Card-free logic module lifting
  M4g's mode-scoped readers (`allEffects`/`allTargetSpecs`/`modesEffects`/
  `modesTargetSpecs`/`selectionCount`) off `Card.spell` so `Activate`/`Engine`/
  `Resolve`/`Mana` read any `Modal card` directly, with `Pawl.Card` reduced to
  one-line delegations; `TargetSpec.NonlandPermanentTarget` (CR 109.2/110.4 — a
  battlefield permanent whose projected types exclude Land) plus
  `Target.selfExcludes`/`legalSetsExcluding` (CR "another" — drops the targeting
  source from a self-excluding spec's legal set at choice time only, re-validation
  stays source-blind) and `Target.fillableModes` generalized out of `Cast` (now the
  one home for mode-fillability shared by spells and abilities); `Subtype.Wizard`
  (CR 205.3m) and a Bird token (1/1 Flying) for the gate card itself.
  `Resolve.resolveEffects` is now payload-and-binding-aware — both
  `resolveAbility` and `Stack`'s trigger arm read the ability object's chosen modes
  via `Binding.modesOf` and resolve/re-validate (CR 608.2b/608.2c) only those.
  Wired into **both** choice points: the activation path (CR 602.2b, mirroring
  `Cast.castSpell`'s mode-then-target order) and trigger placement (CR
  700.2b/603.3d, with the CR 603.3c removal check preceding the mode prompt so a
  never-legal trigger is dropped from stack and objects before ever asking).
  **Two labeled synthetic fixtures**, the `tests-prefer-real-cards` crutch
  discipline: `synthetic-modal-activator` (a `{cost}: choose one — DealDamage /
  PutCounters` ability) covers CR 602.2b activation-path mode choice and the
  mode-scoped CR 608.2b fizzle, expiring when a real modal activated ability lands
  in the opcode set (none surveyed clean — Goblin Cratermaker needs colour,
  Umezawa's Jitte needs charge counters, Insidious Fungus/Cankerbloom carry
  land-play/proliferate riders); `synthetic-modal-trigger` (two self-excluding
  `NonlandPermanentTarget` bounce modes, source as the board's only nonland
  permanent) covers CR 603.3c removal, expiring when a real all-targeted modal ETB
  trigger lands (none found in the opcode set — every candidate needs a new
  opcode or an unmodelled characteristic). **Named deferred expiries:** spec §13
  carries the list, including the M4g deferrals that apply equally here; those
  with a live code site are filed under the `elision` label. Spec and
  plan kept as reference:
  `docs/superpowers/specs/2026-07-20-m4h-modal-abilities-design.md` and
  `docs/superpowers/plans/2026-07-20-m4h-modal-abilities.md`.

## M4.5 (phased)

M4 (M4a–M4h) closed the effect-DSL sequencing; M4.5 is a phased umbrella
closing the remaining **closed-half** gaps the gap census turned up (a
permanent's controller, layer-1 copy, color, and the rest) — each phase gets
its own gate card and spec, landed as it completes. Umbrella:
`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`.

- **M4.5 P1 is complete** (permanent control, GAP-L2). **Gate: Act of Treason**
  (`{2}{R}` Sorcery — "Gain control of target creature until end of turn. Untap
  that creature. It gains haste until end of turn."), chosen over Control Magic
  because Control Magic is an Aura and would drag in the out-of-scope
  Attach/Aura subsystem. The decision proved is that **a permanent's controller
  is a projected layer-2 characteristic, not a base `Object` field**: control is
  base `Object.owner` overridden by layer-2 `Modification.SetController`
  continuous effects (CR 613.1b), timestamp last-wins (CR 613.7), folded by a
  new `Projection.controllerOf :: ObjectId -> GameState -> Maybe PlayerId` and
  enumerated by `Projection.controls :: PlayerId -> GameState -> [ObjectId]` —
  the same "remove the effect and recompute" story as Giant Growth's P/T, so
  `Object` grows no `controller` field and nothing new needs reverting at
  cleanup. `Game.controllerOf` (the M1b owner stand-in) is deleted; every
  caller now reads `Projection.controllerOf`. Two new opcodes: **`GainControl
  Duration SlotName`** (`Resolve.applyEffect` bakes the *source's* controller —
  CR 611.2c fixes the affected set at creation, never chosen — into a stored
  `SetController` continuous effect, and re-Sicks the target, CR 302.6) and
  **`Untap SlotName`** (CR 701.26b — corrected from the plan's draft citation
  of 701.20, which is Reveal). Act of Treason's haste clause reuses the
  existing `ModifyTarget UntilEndOfTurn (GainKeyword Haste)` path (M3b), no new
  opcode. The "you control" call sites — `Combat.legalAttackers`/
  `legalBlockers`, `Engine.untapAll`/`settleAll`, `Action`'s activation
  enumeration — switched from the owner-based `Game.zoneMembers Battlefield` to
  `Projection.controls`; `Resolve.resolveSpell`/`resolveAbility`'s effect
  controller switched from raw `Object.owner` to `Projection.controllerOf`
  (CR 613/608.2c — corrected from the plan's draft citation of 608.2g, which is
  the resolution-time-cast-a-spell rule). A latent bug the earlier funnel
  missed: `Mana.tapForMana` was routing produced mana to `Object.owner`
  regardless of a control change; it now routes to `Projection.controllerOf`
  (CR 109.4a — a mana ability's controller is determined as though it were on
  the stack — and CR 110.2, the permanent's controller; corrected from the
  plan's draft citation of 106.4/108.4, which only establish that mana lands in
  *a* player's pool and the general owner-fallback, not which player). One
  **labeled synthetic** (spec §4, the `tests-prefer-real-cards` crutch): a
  "steal until end of turn, no haste" scenario isolating CR 302.6's
  control-change re-sickening, since Act of Treason's own haste rider would
  mask it; **documented expiry** — retires when Control Magic / the Auras phase
  can test control-change sickness with a real indefinite-control card across
  two turns. Act of Treason itself is a **red deterministic fixture** (the M3d
  posture), not in any random-game deck, so CR 400.7 conservation counts are
  undisturbed. **Named deferred expiries:** spec §7 carries the list — Auras /
  indefinite control, instant-speed control change, CR 613.8 control dependency,
  multiplayer leaves-the-game reversion, mass/conditional untap, control-at-base
  — and those with a live code site are filed under the `elision` label (#62 for
  the cross-turn settle, #33 for the sickness synthetic). Tracking: #26's GAP-L2
  facet is addressed by this phase; #11 stays open. Spec and plan kept as
  reference:
  `docs/superpowers/specs/2026-07-20-p1-permanent-control-design.md` and
  `docs/superpowers/plans/2026-07-20-p1-permanent-control.md`.
- **M4.5 P2 is complete** (copy — layer 1, GAP-L1; the **M4.5 go/no-go**, a genuine
  GO. design.md §5 names the layer system as the architecture's canary, and layer 1
  (copy) was the one blank layer left after P1. **Gate: Clone** (`{3}{U}` Creature —
  Shapeshifter, 0/0, "You may have Clone enter the battlefield as a copy of any
  creature on the battlefield"). The decision it proves: **a permanent's copy is a
  projected layer-1 characteristic — the copied object's *copiable* values (CR
  707.2), snapshotted as the object enters (CR 707.9a) and used to SEED the layer
  fold**, so layers 2–7 (control, ability grants, counters, pumps) fold on top and
  are excluded from a copied object's own copiable value. **The falsifier is
  structural, not a special case**: `Projection.copiableCharacteristics` returns
  base-or-snapshot only, so a +1/+1 counter (layer 7c) on the copied creature makes
  it project 3/2 while the Clone copies the base 2/1 (asserted both-sides). Copy is
  the **fold seed, not a `Modification`** — a deliberate refinement over the spec's
  first draft (a synthesized `Modification.BecomeCopy` would have forced a dead
  `ProjectedCharacteristics` JSON codec, since it never appears in a card): layer 1
  is the fold's starting value, so `projectFrom` seeds from `copiableCharacteristics`
  instead of `baseCharacteristics` (one line; `affectsBase`/source-liveness still
  reads base, so nothing recurses). **Zero new opcodes.** The snapshot rides
  `Object.bindings` (`Binding.copy :: Maybe ProjectedCharacteristics`, forgotten on
  a zone change for free), so it is per-incarnation and locked at entry — a copy
  **survives its source leaving the battlefield** (Murder the copied creature, the
  Clone stays a 2/1), which a live-ObjectId model gets wrong. **The as-enters choice
  is CR-faithful, not a resolution-time stopgap**: "enters as a copy" is a static
  ability whose effect happens as part of the entering event on ANY path (CR
  614.1c/603.6d/113.6h), with the choice made before the object enters (CR 614.12a).
  So it is a **pure mark** on the universal battlefield-entry funnel
  (`Event.placeObject` stamps a `copyOnEnter` object `asEntersPending`) plus a
  **monadic drain** at the CR 117.5 settle boundary (`Engine.drainAsEntersChoices`,
  run **before** state-based actions and triggers — observably equivalent to "before
  it enters": no player, trigger, or SBA sees the interim 0/0; a copied 0/0 becomes
  its real P/T before any SBA, a declined 0/0 dies to that same sweep, CR 704.5f).
  The drain is the **narrow single-choice first version of P5's monadic replacement
  engine**, not throwaway — the `copyOnEnter` classification and entry-funnel hook
  are reused, the bespoke settle pass folds into CR 614.12/616 at P5. New
  types/fields: `Binding.copy`; `Pawl.Binding` reserved slots `copySource`/
  `asEntersPending` + `copyOf`/`setCopy`/`pendingCopy`/`markPending`/`clearPending`;
  `Card.copyOnEnter :: Bool` (a classification, read by the funnel/drain, never a
  card identity); `Subtype.Shapeshifter`; `Prompt.ChooseCopyTarget` /
  `Response.ChoseCopyTarget (Maybe ObjectId)` (replay-serialized); `Target
  .legalCopyTargets`; `Projection.copiableCharacteristics`; `ProjectedCharacteristics`
  gained `Ord` (to ride a `Binding`). `Card.copyOnEnter` serializes **only when
  True**, so the 44 pre-existing `data/cards/*.json` stay byte-identical; Clone is a
  **blue deterministic fixture** (`data/cards/clone.json`, in `allPrintings` for the
  honesty round-trip, no random-game deck). `Pawl.Resolve` stays the sole
  `case`-on-`Effect` home; `Pawl.Projection` the sole `case`-on-`Modification` home;
  the copy target is a genuine prompt, never elided. **Named deferred expiries:**
  spec §7 carries the list — static-ability copying, unprojected copiable values,
  ongoing "becomes a copy", copy-spell and copy-token, simultaneous entry of
  multiple copy-choosers, face-down — and those with a live code site are filed
  under the `elision` label; the general monadic as-enters replacement engine
  (CR 614.12/616), which the drain folds into, is #1. #26 (GAP-L1) is closed by
  this phase (its GAP-L2 sibling was addressed by P1); #11 stays open (P2 adds no
  within-layer dependency — a copy's affected set is always the object itself). Spec
  and plan kept as reference:
  `docs/superpowers/specs/2026-07-20-p2-copy-layer-1-design.md` and
  `docs/superpowers/plans/2026-07-20-p2-copy-layer-1.md`.
- **M4.5 P3a is complete** (color — layer 5, GAP-L5). **Gates: Doom Blade**
  (`{1}{B}` Instant — "Destroy target nonblack creature."), **Crimson Wisps**
  (`{R}` Instant — "Target creature becomes red and gains haste until end of
  turn. Draw a card."), **Aphotic Wisps** (`{B}` Instant, the mirror — "...black
  and gains fear...") and **Bad Moon** (`{1}{B}` Enchantment — "Black creatures
  get +1/+1."). The decision proved: **an object's colour is a projected CR 613
  layer-5 characteristic, folded like `keywordsOf`/`controllerOf`/
  `copiableCharacteristics` before it, and never read off a printed card** —
  `ProjectedCharacteristics.colors :: Set Color` (a `Set`, not a sixth
  `Colorless` constructor: CR 105.2c says a colourless object has no colour at
  all), seeded from CR 202.2's coloured mana-cost symbols union CR 204.2's
  colour indicator (`Card.colorIndicator`, new, serialized only when
  non-empty so every pre-existing `data/cards/*.json` stayed byte-identical),
  and overwritten by one layer-5 `Modification.SetColor (Set Color)` applied
  as a **replace** per CR 105.3 ("a new colour replaces all previous
  colours") — deliberately no `AddColor` constructor, since no card in the
  pool says "in addition to its other colors" (a named deferral, below).
  `Projection.colorsOf` is the sole read point. **CR 702.114a devoid is
  applied at the projection seed, not as a layer-5 CDA pass**:
  `Keyword.Devoid` empties the colour set in `baseColorsOf` instead of
  installing a CR 613.3 CDA-first precedence key on `Gathered`. The code
  comment argues the equivalence from four cases rather than asserting it:
  every layer-5 effect in the vocabulary is `SetColor`, which replaces, so
  "CDA first, then replacers" and "CDA at the seed, then replacers" agree on
  the final set always; a copy of a devoid object snapshots the printed
  keyword (CR 613.2c, P2) and recomputes colourless from its own seed;
  Humility's `LoseAllAbilities` is layer 6, after layer 5, and CR 613.8a
  scopes dependency to same-layer effects, so a Humility'd devoid object
  stays colourless either way; and CR 604.3 ("CDAs function in all zones")
  comes free from a card-derived, zone-independent seed. **A fifth case names
  the channel the first four don't cover:** all four reason about what
  *writes* colour, none about what *reads* it — seeding devoid also moves it
  earlier than CR 613.3's "start of layer 5" relative to a colour *reader*, so
  a layer-2/3/4 effect whose affected set is colour-keyed (expressible today
  via `Affected.CreaturesOfColor`, though no card in the pool pairs it that
  way) would see the wrong answer. So the honest claim is indistinguishable
  **for the pool**, not for everything the engine can reach. **Named expiry:**
  the first card needing a genuine CDA-vs-timestamp interleave within
  layers 2–6 (which would build the `Gathered` precedence key), **or** the
  first layer-2/3/4 effect whose affected set is colour-keyed, whichever comes
  first. **P3b does *not* reopen this question**: devoid is a *constant* CDA
  (safe to seed, since a copy snapshot recomputes the same constant), but
  Tarmogoyf's characteristic-defining P/T is a *dynamic* one — seeding it would
  freeze a Clone's P/T into `Binding.copy` at entry instead of recomputing (CR
  707.2 violation) — so **P3b must fold in-place at the existing
  `Layer.CharacteristicPT` (7a)**, not at the seed; the `*`-P/T seed precedent
  is harmless only because no card in the pool has one, not a licence to seed
  a dynamic CDA. **Three readers span three closed-half
  subsystems, and two of the three expire**: `TargetSpec
  .NonblackCreatureTarget` (CR 115.1a, Doom Blade — the `WallTarget`
  hand-carved-variant posture) and `Affected.CreaturesOfColor Color` (Bad
  Moon — `affects` already receives the partial projection, so a layer-7c
  effect's affected set reads the layer-5 result for free, a genuine
  cross-layer read with no new machinery) both **expire into P9's
  criterion/filter language**; `Keyword.Fear` (CR 702.36b, Aphotic Wisps)
  **does not expire** — it is permanent closed-half machinery, conjoined
  with `evasionAllows` (CR 509.1b: evasion restrictions are cumulative) and
  asymmetric like flying (asked of the attacker first, CR 702.9b), reading
  projected colour *and* projected card type together, so Darksteel Myr
  blocks as an artifact and Typhoid Rats as black. **A live correctness bug
  closed as a data fix**: Dragon Fodder's Goblin tokens had projected
  colourless against their own oracle text ("two 1/1 red Goblin tokens")
  since M4c introduced tokens, because a token carries a `Card` with no mana
  cost; CR 111.3 makes a token's effect-defined characteristics equivalent
  to printed ones, so `colorIndicator` on the nested token card is the fix —
  data only, zero library changes, made observable by Bad Moon (colourless
  would also read as nonblack, so only red proves it). **The gate card that
  is a labeled synthetic**: `synthetic-devoid-drone.json` (`{1}{B}` 2/2,
  Devoid, no other text) — all 26 black-costed devoid creatures on Scryfall
  carry a rider pawl cannot express (a self-effect such as Slaughter Drone's
  `{C}: gains deathtouch`, needing a `ModifySelf` opcode pawl has no self
  slot for; an unbuilt `TriggerCondition` beyond `SelfEnters`, needed by
  Culling Drone/Silent Skimmer/Sky Scourer/Reaver Drone; or menace), and the
  one french-vanilla devoid creature, **Vestige of Emrakul** (`{3}{R}` 3/4,
  Devoid + Trample), needs nothing pawl lacks but is red-costed, so its
  colour conflict is unobservable through this phase's black-keyed readers.
  **Expiry:** a `ModifySelf` opcode (or a self slot) makes Slaughter Drone
  encodable, a new `TriggerCondition` makes Culling Drone or Silent Skimmer
  encodable — either retires the crutch — and a red-keyed reader would make
  Vestige of Emrakul usable with nothing new. **Rulings discipline** (design.md
  §4): Doom Blade and Bad Moon carry **no Gatherer rulings at all** —
  confirmed empty against Scryfall's `rulings_uri`, an empty yield rather
  than a skipped step. Crimson Wisps and Aphotic Wisps each carry the same
  three templating rulings (WotC, 2008-05-01): "colourless is not a colour"
  restates CR 105.4, already exercised by the devoid/Bad Moon tests; "changing
  colour won't change text" **is exercisable after all**: Bad Moon is itself
  a card whose own text is keyed to its printed colour ("Black creatures get
  +1/+1"), and is now transcribed as `ColorSpec`'s "CR 613.1c/613.1e
  2008-05-01 changing a permanent's colour doesn't change its text"; the one
  Q&A-shaped ruling — a colour change overwrites
  *all* previous colours, "even if... blue and black" — is transcribed as
  `ColorSpec`'s "2008-05-01 a colour change overwrites ALL previous colours,
  even a multicoloured one" (two stacked `SetColor` effects rather than a
  card, since no card in the pool is printed multicoloured), and passes
  against the already-landed replace semantics. **A plan bug found and
  fixed in its own task's execution** (design.md's "a test failing against
  correct code is a plan bug" discipline): the token-colour task's draft
  fixture used `S.spellOnStack` + `Stack.resolveTop`, which can never resolve
  a **modal** spell — `spellOnStack` leaves `Object.bindings` empty, so
  `Binding.modesOf` is empty and `Modal.modesEffects` returns `[]`, and every
  spell has been modal since M4g; the fix substitutes a real cast (mirroring
  `ResolveSpec`'s own Dragon Fodder test) and preserves every assertion — a
  fixture caveat worth recording for future test authors reusing
  `spellOnStack` against a card with effects. **Named deferred expiries:** spec §7
  carries the list — `AddColor`, hybrid/Phyrexian mana symbols, the CR 613.3
  CDA-precedence question above, colour outside the battlefield and the general
  filter language (both **P9**), colour indicators needing suspend, devoid acquired
  by copy or text-change, the synthetic Devoid Drone and Vestige of Emrakul,
  "choose a colour" as a prompt, protection, and colour as a copiable value under a
  non-devoid layer-5 CDA. Those with a live code site are filed under the `elision`
  label (#35 for the CDA-precedence shortcut, #40 for the target-spec family).
  **Tracking:** no issue is closed by this phase; #11 (topological CR
  613.8b applies-to reorder) stays untouched, since every layer-5 effect this
  phase adds replaces rather than depends, so within-layer ordering is
  last-wins by timestamp; #14 (`Quantity.Bound`) belongs to **P3b**.
  The umbrella's §3 table splits its single P3 row into **P3a** (this phase)
  and **P3b** (characteristic-defined P/T, next), per the umbrella's own §7
  authorization for a phase spec to depart from the map and update it. Spec
  and plan kept as reference:
  `docs/superpowers/specs/2026-07-20-p3a-color-design.md` and
  `docs/superpowers/plans/2026-07-20-p3a-color.md`.
- **M4.5 P3b is complete** (characteristic-defined P/T, GAP-L7cda — layer 7's
  last blank sublayer). The umbrella scoped P3b to layer 7a alone; the phase
  spec widened it to **the rest of layer 7**: 7a, the CR 608.2h/611.2d freeze
  7b owed, and 7d. **Gates: Tarmogoyf** (`{1}{G}` Creature — Lhurgoyf `*`/`1+*`,
  "Tarmogoyf's power is equal to the number of card types among cards in all
  graveyards and its toughness is equal to that number plus 1") falsifies
  "evaluate `*` once, at entry" — empty graveyards make it 0/1, Fog resolving
  into a graveyard makes it 1/2 with no re-entry and no effect touching it
  (Fog, not Lightning Bolt: Bolt targets, and the identity answerer would aim
  it at the only creature on the board — the 0/1 Goyf being measured — and
  kill it) — and falsifies "a copy snapshots the number" — a Clone of
  Tarmogoyf must keep recomputing, not freeze at 2/3. **Inner Calm, Outer
  Strength** (`{2}{G}` Instant — Arcane, "Target creature gets +X/+X until end
  of turn, where X is the number of cards in your hand") falsifies "a stored
  continuous effect re-evaluates its quantity" — the pump must not shrink when
  the caster's hand does (CR 608.2h) — and falsifies "the frozen value reads
  the target" — it must read the **caster's** hand, not the target
  controller's. **Twisted Image** (`{U}` Instant, "Switch target creature's
  power and toughness until end of turn. Draw a card.") falsifies "7d switches
  the printed/base box" — a 2/3 Tarmogoyf must become 3/2 via the *projected*
  values, not 0/0 printed ones — and, via its own three 2021-03-19 Gatherer
  rulings, falsifies "the switch is a display concern": a creature at 2/3 that
  survived 2 marked damage dies after becoming 3/2 (CR 704.5g), because damage
  stays marked while toughness moves. **The phase thesis, in one line**: one
  counting quantity, and the two rules that say when it is re-read — CR 613.4a
  says a characteristic-defining ability is **recomputed on every projection**
  (7a, Tarmogoyf), CR 608.2h says a resolution-created continuous effect's
  quantity is **frozen once**, at resolution (7b, Inner Calm). The *same*
  `Quantity.Count` machinery answers both, pointed at opposite re-read rules.
  New numeric-tower arms: `Quantity.Star` (CR 208.2's printed `*`, evaluates to
  `Nothing` — notation, not a value), `Quantity.Plus Quantity Quantity`
  (composes `1+*`), `Quantity.Count CountSpec` (new sibling module
  `Pawl.Type.CountSpec`: `CardTypesInAllGraveyards`, `CardsInYourHand` —
  quarantined, **expires at P9**'s criterion language, same posture as
  `WallTarget`); `Quantity.evaluate` gained a "you" `PlayerId` for
  player-scoped counts; a new `substituteStar` helper replaces `Star` with the
  card's own CDA (`Card.characteristicPT :: Maybe Quantity`), recursing through
  `Plus` — Tarmogoyf's printed `1+*` toughness becomes
  `Plus (Literal 1) (Count CardTypesInAllGraveyards)`. **Layer 7a folds in
  place in `projectFrom`, not through a synthesized `Gathered` the way
  layer-7c counters do — three reasons, recorded in the code comment**: (1)
  Humility's `LoseAllAbilities` must be able to strip the CDA at layer 6, but
  `gather` runs *before* the fold and has no partial projection to strip from;
  (2) CR 604.3 says a CDA "functions in all zones" (CR 208.2a repeats it for
  P/T specifically), and `gather` is battlefield-only, while `projectFrom` is
  not zone-scoped — proved by Tarmogoyf's own ruling that it counts itself in
  a graveyard; (3) a CDA has no source object and no timestamp, so it has
  nothing to sort on under CR 613.7 and doesn't belong in the candidate list
  at all. `PC.characteristicPT :: Maybe (Quantity, Quantity)` seeds via
  `substituteStar` — **unevaluated quantities, not numbers** — riding
  `Projection.copiableCharacteristics` (P2's layer-1 seed), so a Clone
  acquires the *ability* and keeps recomputing (CR 707.2a) rather than
  freezing the number (CR 707.2b's converse). **This pays P2's deferred
  bill**, named there verbatim: *"7b/CDA P/T-setting in copiable values (rides
  P3b, Tarmogoyf)."* **A latent bug closed, and its citation corrected**:
  `Pawl.Resolve`'s `ModifyTarget` arm had carried a comment since M4a citing
  **CR 611.2b** for a freeze it never performed, claiming it was a no-op
  "until X exists" — but 611.2b is the *"for as long as"* duration rule, not
  the freeze rule, and X had existed since M4a, so the stated precondition had
  already expired; the right rules are **CR 608.2h** (an effect needing
  outside information is determined only once, when applied) and **CR
  611.2d** (variables such as X). The bug had a second, wrong-object half:
  `Projection.applyModification` evaluates a stored quantity against the
  **affected object**, so a stored X would have read the target's bindings
  and a stored "cards in your hand" would have counted the wrong player's
  hand. New `Projection.freezeQuantities` fixes both halves at once: `Resolve`
  calls it at store time, against the **source spell and the source's
  controller** — never the target. Static abilities are deliberately exempt
  (CR 611.2 scopes the whole freeze family to *"a continuous effect generated
  by the resolution of a spell or ability"*; a static ability's continuous
  effect, CR 604.2, is regenerated from the permanent every projection), so
  Opalescence's `ManaValue` still recomputes per affected object — the
  existing M3c Humility+Opalescence gate is the regression guard, unchanged.
  **What the freeze does NOT close**: `freezeQuantities`'s `Nothing` fallback
  (an unevaluable quantity) leaves the quantity un-frozen in the store, and
  `Projection.applyModification` still evaluates it against the affected
  object on every projection read — the same wrong-object shape the fix was
  meant to retire, just unreachable, since no card in today's pool stores an
  unevaluable quantity in a continuous effect. Recorded in the code as a named
  expiry rather than overstated as fixed. **Twisted Image carries functional
  errata that shrank the phase**: the printed New Phyrexia wording was "target
  **artifact or** creature's power and toughness," but Scryfall's Oracle text
  (verified 2026-07-21) drops the artifact clause — CR 208.3 says why, *"a
  noncreature permanent has no power or toughness,"* so switching a
  noncreature artifact's P/T never did anything. WotC's silent errata deleted
  a whole `TargetSpec` this phase would otherwise have added; the existing
  `CreatureTarget` (CR 115.1a) sufficed instead. This is design.md §4's
  functional-errata category (*"a round-trip failure after a data refresh may
  be an errata event, not a regression"*) actually biting for the first time,
  caught before it reached code. `Modification.SwitchPowerToughness` (layer
  7d, CR 613.4d) swaps `PC.power`/`PC.toughness` outright; two switches return
  to normal for free, since each is a separate fold application. **One honest
  non-distinguishing test, labeled as such**: clearing `PC.characteristicPT`
  on `Modification.LoseAllAbilities` is required by CR 604.3 (a CDA is a
  static ability; Humility removes abilities) but is **unobservable in the
  current pool** — every `LoseAllAbilities` source, Humility included, also
  sets base P/T at layer 7b, which overwrites layer 7a's result regardless of
  whether the clear is correct; a Humility'd Tarmogoyf is 1/1 either way. The
  clearing and its test are implemented anyway (the CR says so; "Humility'd
  Tarmogoyf is 1/1" is a genuine ruling worth transcribing), but both are
  labeled non-distinguishing with a concrete **named expiry**: **Dress Down**
  (an enchantment whose "Creatures lose all abilities" clause sets no P/T, but
  needs Flash, a beginning-of-end-step trigger (**P4**), and a Sacrifice
  effect) or **Soul Sculptor** (needs layer-4 card-type *replacement* —
  becoming an enchantment and ceasing to be a creature — which `Modification`
  doesn't have yet), whichever lands first; the Aura family (Darksteel
  Mutation and kin) is the bulk of the category but is blocked on Attach,
  outside M4.5 entirely. **Named deferred expiries:** spec §8's table carries
  every one with its expiry — the two P/T axes counting different things,
  `Count` over projected state (Strength of Cedars / Wirewood Pride), projected
  graveyard card types, `CountSpec` as a whole (**P9**), CR 208.2b's as-enters
  P/T choice (**P5**), CR 208.5 and CR 208.2a's undeterminable-number
  substitution, a one-axis 7b set, CR 208.4b base-P/T readers, a dynamic CDA
  defining colour or subtype, CR 613.3 precedence (unchanged from P3a),
  `Quantity.Half`/`Infinite`, and static-ability copying (unchanged from P2).
  Those with a live code site are filed under the `elision` label: #41, #65,
  #36, #35, #39. **Tracking**: no issue is closed by this phase. #14
  (`Quantity.Bound SlotName`) was related to P3b by the umbrella and is
  answered **in the negative**: neither `Count CardTypesInAllGraveyards` nor
  `Count CardsInYourHand` needed a binding slot, so it stays open, unretired.
  #11 (topological CR 613.8b applies-to reorder) is untouched: 7a
  applies at most one CDA per object with nothing to order against it, and
  7d's switches order last-wins by timestamp with no same-layer dependency.
  **Rulings yield honestly** (Scryfall, 2026-07-21): Tarmogoyf carries one
  relevant ruling (2007-10-01, the all-zones/counts-itself ruling, transcribed
  as a test and cited above as the reason for the in-place fold); Twisted
  Image carries three, all dated 2021-03-19 (switch applies after all other
  effects regardless of when they began; nonlethal damage may become lethal;
  an even number of switches is a no-op); **Inner Calm, Outer Strength has
  zero Gatherer rulings**. New subtypes: `Subtype.Lhurgoyf` (CR 205.3m,
  Tarmogoyf's creature type) and `Subtype.Arcane` (CR 205.3k, Inner Calm's
  spell type). Three new deterministic fixtures (`tarmogoyf.json`,
  `inner-calm-outer-strength.json`, `twisted-image.json`), in `allPrintings`
  for the round-trip and in no random-game deck, so CR 400.7 conservation
  counts stay undisturbed. Zero new opcodes, zero new prompts, no change to
  `Object`, `GameState`, or the event pipeline. **After P3b, Cluster 1
  (layer-system completion) is done**: layers 1 (P2), 2 (P1), 3 (M3d), 4
  (M3c), 5 (P3a), 6 (M3b) and 7a/7b/7c/7d (P3b) all have producers — every
  layer CR 613 names now has a producer in every sublayer. The umbrella's
  next phase is **P4** (event history + state/delayed triggers, gating P6 and
  P7). Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-21-p3b-characteristic-defined-pt-design.md`
  and `docs/superpowers/plans/2026-07-21-p3b-characteristic-defined-pt.md`.
- **M4.5 P4 is complete** (event history + state/delayed triggers, GAP-T).
  Cluster 1 closed at P3b; P4 opens **Cluster 2, the event substrate
  generalized**, and discharges the umbrella's only remaining hard dependency
  edge, `P4 → {P6, P7}`. **The decision proved, in one line: events are
  recorded, not consumed; and a trigger's condition is a classification over
  game *state*, not only over the event stream.** The two halves are one phase
  because each is the other's falsifier — a log that is merely a longer queue is
  not history, and a condition language that only matches events cannot express
  a state trigger at all. **Gates, four cards, each one or two clauses:**
  **Barbarian Outcast** (`{1}{R}` Creature — Human Barbarian Beast, 2/2, "When
  you control no Swamps, sacrifice this creature.") is CR 603.8, and its
  falsifier is **flooding**: exactly **one** trigger reaches the stack, not one
  per priority boundary — which is the failure CR 603.8's second sentence exists
  to prevent, and it is CR 603.8's own illustrative shape ("a player controlling
  no permanents of a particular card type"). Armed-ness is **derived, not
  stored**: an instance is suppressed while a matching `Source.OfTrigger` object
  sits on the stack, so 603.8's three exits ("resolved, countered, or otherwise
  left the stack") are all just "no longer on the stack" and there is no
  bookkeeping field to leak; suppression is structural on **both** source id and
  ability, so two Outcasts give two instances. Both `StateCondition` arms read
  the **projection**, pinned by two tests that would otherwise have been
  assertion-only: give a Mountain the Swamp subtype (layer 4) and the trigger
  stops; hand Alice control of Bob's Swamp (P1's layer 2) and it stops.
  **Khabál Ghoul** (`{2}{B}` Creature — Zombie, 1/1, "At the beginning of each
  end step, put a +1/+1 counter on this creature for each creature that died
  this turn.") is CR 608.2i turn history, read from the log's
  `ProjectedCharacteristics` **snapshots**: the deaths it counts happened at
  boundaries the trigger scan already passed (a drained queue reads zero), the
  thing counted is a token with **no printed card** (CR 111.1/111.3 — re-deriving
  types from print reads zero), and the count resets at **turn handoff**, not at
  the scan. Zero new opcodes: it is `PutCounters PlusOnePlusOne (Quantity.Count
  …)`, cashing P3b's numeric tower against one new `CountSpec` arm. **Tidal
  Wave** (`{2}{U}` Instant — "Create a 5/5 blue Wall creature token with
  defender. Sacrifice it at the beginning of the next end step.") is CR 603.7,
  and its shape is forced by the closed half: `Effect` is **first-order and
  non-recursive**, so the delayed ability is **card data** (`Card
  .delayedAbilities`, keyed by a new `AbilityName`) and the opcode only **arms**
  it, capturing the resolving object's binding environment — which is how "it"
  (CR 603.7c) is remembered. It fires exactly once (603.7b), does nothing if the
  token has already left the battlefield (603.7c) and is consumed anyway, and
  603.7a's "no event from before it was created" falls out for free from the
  watermark. The card-text-in-two-fields join is policed by the lint family
  `SlotName` already documents: an `ArmDelayedTrigger` naming an undeclared
  ability is a **failing test**, not a trigger that never fires. **Sarcomancy**
  (`{B}` Enchantment — ETB 2/2 black Zombie token; "At the beginning of your
  upkeep, if there are no Zombies on the battlefield, this enchantment deals 1
  damage to you.") is the intervening "if" at **both** check sites, CR 603.4
  (it does not trigger at all) and CR 608.2a (it is removed from the stack if
  the condition has become false). The distinguishing case is a Zombie created
  **in response**, and CR 608.2a was proven load-bearing rather than asserted:
  deleting the resolution-time re-check makes that test fail (1 damage instead
  of 0), while a single trigger-time check passes the other two. It is also
  `NoPermanentsOfSubtype`'s first behavioural customer, and the test
  **discriminates it from `YouControlNo`** — the responding Zombie is under
  *Bob's* control, so a "you control no" implementation reads the wrong answer.
  **The centerpiece scenario is where the four falsifiers interlock**: Tidal
  Wave's delayed sacrifice and Khabál Ghoul's count trigger at the beginning of
  the *same* end step under one controller, so the controller must **order**
  them (CR 603.3b), and the order **changes the answer** — CR 608.2h determines
  the count when the effect is applied, so sacrifice-first gives **1** counter
  and count-first gives **0**, from one board on two answers. The thing counted
  is a token with no printed card, and the death happened at a boundary the scan
  already passed; an engine that picks the order silently, re-derives from print,
  or reads a drained queue fails one of the four. **Types and opcodes**: new
  `Pawl.Type.GameEvent` (`Moved ZoneChange !ProjectedCharacteristics` /
  `DamageDealt DamageEvent` / `StepBegan Phase PlayerId` — `ZoneChange` and
  `DamageEvent` survive as the payloads they already were, a re-homing, not a
  redesign), `TurnScope` (`EachTurn`/`ControllersTurn`), `PendingTrigger`,
  `StateCondition` (`YouControlNo Subtype`/`NoPermanentsOfSubtype Subtype` —
  hand-carved, **expires at P9**'s criterion language, the posture `CountSpec`
  and `WallTarget` already carry), `AbilityName` (`SlotName`'s shape, named
  never positional) and `DelayedTrigger`. Two new opcodes: **`Effect.Sacrifice
  SlotName`** (CR 701.21/701.21a — sacrificing is **not** destroying, so it
  consults neither regeneration shields nor indestructible; the reviewer
  confirmed that by construction, `Event.sacrifice`'s body naming none of them)
  and **`Effect.ArmDelayedTrigger AbilityName`**. `TriggerCondition` gains
  `StepBegins Phase TurnScope` and `StateIs StateCondition`; `TriggeredAbility`
  gains `intervening :: Maybe StateCondition` (one predicate vocabulary, two
  customers — that reuse is why 603.4 was cheap to include); `Card` gains
  `delayedAbilities`; `Effect.Create` grows a `Maybe SlotName`; `CountSpec`
  gains `CreaturesDiedThisTurn` (folds the log and reads `PC.cardTypes`, never
  `Game.cardOf`; `Pawl.Quantity` still does **not** import `Pawl.Projection`, so
  the layer-fold recursion hazard stays impossible);
  `Prompt.OrderTriggers`/`Response.OrderedTriggers`. `GameState` gains
  `events`, `scannedThrough`, `damageScannedThrough` and `delayedTriggers`, and
  **loses `zoneChanges` and `damageEvents`** — consumption becomes an index
  bump and the record stays, with clearing a **turn-handoff** act rather than a
  cleanup-step one (cleanup is still part of *this* turn, and CR 514.1's discard
  is an event of it). Two reserved binding slots: **`self`** (CR 113.7, a
  triggered ability's own source, stamped at placement, which is what makes
  "this creature" expressible without a second `SacrificeSelf` opcode) and
  **`you`** (the ability's controller, making Sarcomancy's "deals 1 damage to
  you" a slot read rather than a new opcode). The trigger scan gathers from
  three sources — event-matched over **all** battlefield permanents (CR 603.6a
  widens M3f's newcomer-only scan), state-matched, and delayed — with the CR
  603.4 filter applied at the gather, which is what keeps
  `placePendingTriggers`' re-run flag honest. **An elision is RETIRED, not
  added.** M3f's CR 603.3b ordering elision ("at most one trigger controlled by
  one player … elided until a second simultaneous trigger exists") falls due in
  a single line of card text, and `Prompt.OrderTriggers` replaces it — asked
  **only** when a player controls two or more pending triggers, since with one
  there is nothing to choose. The payload is that player's pending triggers as
  an indexed list of source `ObjectId`s in the engine's canonical order and the
  answer is a permutation of the indices, validated **reject-not-repair** (short,
  duplicate and out-of-range answers all fall back to canonical, each pinned by
  a test). `Engine.apnapOrder` was replaced by `apnapPlayers`, total on a
  degenerate turn order and strictly more correct — the old one never consulted
  turn order at all. The source-only payload is **contingent, not a rules
  property**, and carries its own named expiry in `Prompt`: it holds only while
  no single source can have two *distinct* abilities triggered in one batch —
  Sarcomancy already has two triggered abilities, and they fail to co-trigger
  only by settle-schedule accident — expiring at the first card where they can,
  which needs an ability discriminator on the wire. **Deferred, with named
  expiries:** spec §8 carries all sixteen with their triggers — reflexive
  triggers (CR 603.12), leaves-the-battlefield triggers and the look-back list,
  the enters-then-dies-same-settle timing gap, stated-duration delayed triggers,
  CR 603.2d/603.2h/603.7h/603.9, CR 603.3b's second part, CR 400.7e, scanned
  zones beyond the battlefield, a `Create` binding several tokens, event kinds
  with no reader (VOCAB), the unenforced CR 701.21a control clause, trigger
  control read at the scan boundary, state-trigger non-termination, two
  identical state triggers conflated by `Source` equality, partial CR 514.3
  handling, and a delayed ability's intervening "if" pruned before it is
  checked. Those with a live code site are filed under the `elision` label:
  #45, #46, #47, #48, #49, #51, #52, #53, #54, #55, and #44. **Corrections
  worth remembering.** A **systemic CR 701.x renumbering**, found only because
  half-fixing Destroy left one file citing **701.8 for two different actions**:
  the codebase's keyword-action citations predated **`Create`'s insertion at
  701.7**, so everything from Destroy onward was cited low — Destroy 701.7→
  **701.8**, Discard 701.8/8a/8b→**701.9/701.9b/CR 609.3**, Exile 701.10→
  **701.13**, Mill 701.13/13b→**701.17/701.17b**, Mill by four. Sub-letters do
  not remap mechanically (the old `701.8a`, "the discarding player chooses",
  is **701.9b**, not 701.9a), and the old `701.8b` has no 701.9 counterpart at
  all — it is **CR 609.3**, "does only as much as possible", researched rather
  than invented. Counter (701.6), Regenerate (701.19), Sacrifice (701.21),
  Search (701.23) and Tap/Untap (701.26) were already right and were left alone.
  This was a **pre-existing** defect in the project's most binding constraint,
  invisible until a partial fix made one file self-contradictory.
  `GameEvent.Moved`'s snapshot field had to be made **strict**: left lazy, no
  production reader forced it, so the snapshot was a thunk retaining the entire
  pre-move `GameState` for a whole turn — defeating the `Map.delete` two lines
  below it. Fixed with a plain Haskell 2010 bang (no pragma) and **measured**
  against the pre-log baseline `3cc3ecd` in a scratch worktree rather than
  asserted: goldfish 10.1→10.6ms, casting 9.30→9.73ms, fighting 9.31→9.72ms —
  ~4-5%, inside the ~850µs run-to-run stddev, and the numbers are in the code
  comment where an unquantified claim used to be. **The delayed-trigger binding
  merge was backwards**: `placeOne` merged the captured environment over the
  placement-time bindings with a left-biased `Map.union`, and the captured side
  was on the left, so a spell's own reserved `modes` slot (stamped by
  `Binding.fromChoices` on every cast) **overrode the delayed ability's own** —
  invisible only because both were mode 0; a modal spell arming a delayed
  ability would have resolved the wrong mode or none. `variableX` collided
  identically. Argument order flipped so placement-time wins; verified by revert
  (`expected {0}, got {7}`). **Nine rules-citation errors were caught in total**,
  several by implementers and reviewers who read `rules.txt` rather than copying
  the brief: CR 608.2g→**113.7** for the source of a triggered ability (found
  twice, independently); CR 700.4→**702.12b** for indestructible (700.4 really
  is the definition of *dies*, and stayed where it belonged); CR 108.4→**109.5**
  for what "you control" means; CR 503.1→**502.4 + 503.1a** for held triggers at
  untap; and CR 111.3→**111.1** at five sites for "a token isn't represented by
  a card" (two genuine 111.3 uses left alone). Each was corrected **at source in
  the plan** so it could not propagate to a later task. Two process failures are
  recorded rather than buried. A reviewer finding that the widened CR 603.6a scan
  had dropped an enters trigger whose source had already left the battlefield was
  **adjudicated false** — `Event.changeZone` deletes the id from `GameState
  .objects` on the very next line after removing it from the battlefield, so
  `∉ battlefield ⟹ ∉ objects` by construction and the "fix" was dead code with a
  test building a state no library path can produce; it was reverted (the
  independently valuable projection hoist, one `projectAll` per settle instead of
  one `gather` per event × permanent pair, was kept). The lesson recorded: **a
  finding that asserts what existing, unchanged code does is a claim to verify
  before acting, not after.** And the centerpiece test shipped in the plan with
  its **two arguments transposed** — `orderLast` puts a trigger last on the
  stack, `placeOne` conses, so last-placed resolves *first*, making
  `orderLast ghoul` the counting-first case, not the sacrificing-first one the
  plan paired it with. The implementer proved it with a temporary trace and
  fixed the **test, not the engine**, per CLAUDE.md; had it shipped as written,
  the phase's thesis test would have asserted the opposite of what it claimed
  and passed. **Tracking:** no issue is closed. #13 (OfAbility LKI)
  stays **open and unretired** — §2.2's last-known-information snapshot is the
  mechanism its fix will use, not the fix; #1 (M3f replacement seam) is
  untouched and belongs to **P5** — note the family resemblance, P4 retires the
  *trigger* ordering elision (603.3b) and P5 retires the *replacement* ordering
  elision (616), different rules and different mechanisms; #14
  (`Quantity.Bound → SlotName`) stays open, since like P3b's two arms
  `CreaturesDiedThisTurn` needs no binding slot — it folds the log. Four new
  deterministic fixtures (`barbarian-outcast.json`, `khabál-ghoul.json`,
  `tidal-wave.json` — carrying a nested Wall token card — and `sarcomancy.json`);
  **no pre-existing card file was modified** — verified by diff, so `Create`'s
  new arity and `Card`'s new field are invisible to all 59 of them.
  Final suite **670/670**, warning-clean under `-Werror` on a from-scratch
  build, hlint clean, `cabal bench` statistically identical to baseline; the
  whole-branch review found **zero Critical and zero Important** code defects and
  confirmed both invariants branch-wide (`TriggerCondition.`/`StateCondition.`
  appear outside `Pawl.Event`/`Pawl.Codec` nowhere). The umbrella's next phase is
  **P5** (replacement event coverage + CR 616, subsuming #1);
  **P6 and P7 are now unblocked**, each adding a *reader* of this log rather than
  a mechanism, which is the whole reason P4 preceded them. Spec and plan kept as
  reference:
  `docs/superpowers/specs/2026-07-21-p4-event-history-triggers-design.md` and
  `docs/superpowers/plans/2026-07-21-p4-event-history-triggers.md`.
- **M4.5 P5 is complete** (replacement event coverage + CR 616, GAP-R and the
  git-bug `6afb561` M3f seam). P4 opened Cluster 2; P5 closes its second phase
  — **exactly one replacement path exists in the engine, and it is monadic**,
  replacing the pure single left-to-right fold M3f shipped as a placeholder.
  **Three gate cards, each falsifying a different part of the pure-fold
  assumption.** **Hardened Scales + Corpsejack Menace** falsify
  determinism-by-list-order — and, unlike the two-Hardened-Scales case this
  phase also tests, the order here changes the *count*, not just the
  audit trail. Hardened Scales reads "If one or more +1/+1 counters would
  be put on a creature you control, that many plus one +1/+1 counters are
  put on it instead"; Corpsejack Menace reads "...twice that many...
  instead." CR 616.1 says the affected permanent's controller **chooses**
  which applies first, and a pure fold has no chooser to ask — it has to
  invent an order, silently making a choice the rules assign to a player.
  On the same board resolving the same spell, Scales-then-Corpsejack
  computes (1 + 1) × 2 = **4**; Corpsejack-then-Scales computes
  (1 × 2) + 1 = **3**. Same input, different outcome, decided solely by
  which effect the player picks first — a pure fold cannot even fall back
  on "the answer doesn't matter" here.
  **Doubling Season** is one card with two replacement abilities in two
  different event classes (`TokenR` for "twice that many tokens," `CounterR`
  for "twice that many counters"), which falsifies a design that gives one
  source only one replacement opportunity, and its Voice-of-All-token
  interaction is CR 616.1g's own worked example: creating a token **contains**
  that token's own entry, so Doubling Season's count must be chosen before
  either token's own entry replacement can be chosen. **Clone + Primal
  Plasma** is the centerpiece: a Clone's `EntryR AsCopy` replaces its copiable
  snapshot with Primal Plasma's, and Primal Plasma's own `EntryR (ChoiceOf …)`
  — the CR 208.2b "choose the power and toughness" ability — did not exist as
  an applicable candidate until that snapshot swap happened. CR 616.2 ("A
  replacement or prevention effect can become applicable to an event as the
  result of another replacement or prevention effect that modifies the event")
  is exactly this, and **no single-pass implementation can produce it**: the
  loop must re-collect candidates against the CURRENT state on every
  iteration, not decide once from the original candidate list, and it did not
  appear anywhere in the umbrella's own gate-card prediction — this spec added
  it after the umbrella was written. **Deleted:** `Pawl.Type.Prevention`,
  `ActivePrevention` (the old shield-only record), `GameState.preventions`,
  `regenerationShields`, `Card.copyOnEnter`, `Engine.drainAsEntersChoices` and
  the `Binding` pending marker it drained, `Event.applyReplacements` /
  `applyPreventions` / `cancels` / `regenerate` / `markCopyOnEnter`,
  `Target.legalCopyTargets`, `Effect.Prevent` / `RegenerateSelf`. **Added:**
  `Pawl.Replacement` (the CR 616.1 loop, collection, bucketing, `choose`,
  `chooserOf`, the entry loop, `legalCopyTargets`'s replacement); six
  `ProposedEvent` arms (`WouldChangeZone` / `WouldEnter` / `WouldDealDamage` /
  `WouldBeDestroyed` / `WouldPutCounters` / `WouldCreateTokens`); six
  `ReplacementEffect` arms (`ZoneChangeR` / `EntryR` / `DamageR` /
  `DestructionR` / `CounterR` / `TokenR`); `ActiveReplacement` (the floating
  store's record, now carrying `source` and `timestamp`) and `Uses`;
  `Effect.Replace`; `Event.putCounters` / `createTokens`;
  `Prompt.ChooseReplacement` / `ChooseEntryOption`; the monadic funnels
  threading `Game` through what used to be pure, and
  `Sba.performStateBasedActions :: Game Bool`. **Three deliberate departures
  from the phase's own spec**, all forced by review, not drift: (1) **CR
  614.5's identity is `(source, effect VALUE)`, not `(source, index)`** — index
  identity makes the Clone/Primal Plasma centerpiece unreachable, because
  Clone's `AsCopy` and the newly-acquired `ChoiceOf` are both index 0 of their
  respective one-element lists, so the already-applied set would swallow the
  new ability and CR 616.2 would never fire; value identity's only cost is
  that a single source with two textually identical replacement abilities gets
  one CR 614.5 opportunity instead of two, which no card in the pool
  exercises (#75). (2) **The data-file migrations landed with the opcode that
  needed them, not all in one task** — `rest-in-peace.json` in the task that
  reshaped `ReplacementEffect`, `fog.json`/`drudge-skeletons.json`/
  `clone.json` each with the task introducing `Effect.Replace` /
  `WouldBeDestroyed` / `EntryR AsCopy` — so every task's commit left the suite
  green, never a multi-task migration in flight. (3) **Four internal types the
  spec's own module inventory did not list** — `Pawl.Type.CandidateId`,
  `Pawl.Type.ReplacementCandidate` and `Pawl.Type.ReplacementBucket` joined
  `ProposedEvent` under the one-type-per-module rule rather than living as
  anonymous tuples inside `Pawl.Replacement`. **Two things this note must be
  honest about, because reviews forced their retraction mid-phase.** CR
  616.1c's dedicated `CopyOnEntry` bucket split is correct but is **not** what
  makes the centerpiece work — on the entering Clone's first iteration,
  `AsCopy` is the *only* applicable candidate, so it is picked because it is
  the sole candidate, not because of its bucket; what actually makes CR 616.2
  work is CR 616.1f's re-collection each iteration together with departure
  (1)'s value identity, and the bucket split itself is exercised by no test
  (#73). **CR 616.1g's nesting is implemented but exercised by no test**:
  every token card in the pool has empty `replacementEffects`, so
  `Event.createTokens`'s nested per-token entry loop finds no candidates and
  returns immediately — deleting the nesting call would leave all 694 tests
  passing. This was verified empirically, not inferred from the pool's
  shape: `7b191b2` actually deleted the `Monad.mapM_ (Replacement.runEntry
  …)` line from `Event.createTokens`, ran the suite, watched all 694 tests
  still pass, and restored the line (#73). **Eight
  wrong CR citations were caught during the phase**, worth recording as the
  phase's most transferable lesson: one originated in the design spec itself;
  one was copied from already-landed code on the assumption a landed citation
  had been checked and had not; four were introduced by later fix passes,
  each while correcting a *different* citation nearby; and one was
  re-introduced by a task's brief after an earlier task had already fixed it
  in code, so the brief's own text regressed a correction the implementation
  already carried. Three separate implementers, across three separate tasks,
  refused a brief-specified citation on the strength of `docs/rules.txt` and
  were right every time — CLAUDE.md's "never trust recalled Magic rules"
  applies as much to a task brief's citations as to memory. **Tracking:**
  `48b17cb`/issue #1 (the M3f replacement seam — both GAP-R, the event-coverage
  facet, **and** the CR 616 ordering/choice facet `6afb561` tracked) is
  **closed**; #58 (CR 615.7's shared-shield allocation, CR 615.13's
  prevented-triggers) is **updated, not closed** — `source` and `timestamp`
  retire the "no way to report either" blocker, but CR 615.7 needs a
  cross-event allocation the per-event loop shape cannot express, and CR
  615.13 needs `Pawl.Damage.resolveDamage`'s return type widened to report
  which candidate applied, two different remaining blockers now named instead
  of one unstated one; eleven new issues filed for this phase's own
  deferrals, #68–#78 (ten from the plan's own list plus one — #78 — found
  while sweeping placeholders: a third, wholly unimplemented channel by which
  a simultaneously-entering sibling's static abilities could leak into a
  later token's entry-loop projection, CR 614.12 does not sanction it, and it
  is unreached only because every token card in the pool has empty
  `staticAbilities`). Note the family resemblance P4's spec already drew: P4
  retired the *trigger* ordering elision (CR 603.3b); P5 retires the
  *replacement* ordering elision (CR 616.1) — different rules, different
  mechanisms, same shape of mistake, one phase apart. **Final suite 694/694**,
  warning-clean on a from-scratch `cabal clean` build, `hooky run` clean.
  `cabal bench`: goldfish 11.3ms, casting 12.3ms, fighting 12.3ms — but issue
  #66 (pre-existing, not introduced here) means all three benchmarks execute
  the **identical** pass-pass-draw-until-decked game, so the per-scenario
  split is not meaningful; the only honest reading is the aggregate, ~12ms,
  consistent with the pre-P5 baseline. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-21-p5-replacement-events-design.md` and
  `docs/superpowers/plans/2026-07-21-p5-replacement-events.md`.
- **M4.5 P6 is complete** (conditional and event durations, GAP-D). A stored
  effect's duration now has a **beginning** as well as an end. `Duration` — the
  printed card-data vocabulary — split from a new runtime-only `Expiry`, and
  the whole life cycle moved into one module: `Pawl.Expiry` is the **sole home
  of `case … Expiry`**, the standing `Pawl.Resolve` has over `Effect` and
  `Pawl.Projection` over `Modification`. **Two gate cards, each falsifying a
  different fixed point.** **Master Thief** is CR 611.2b's own printed example
  — the rules text names the card — and its three Gatherer rulings *are* three
  of the four tests, verbatim. The load-bearing one is **the latch**: "If
  another player gains control of Master Thief, its control-change effect ends.
  Regaining control of Master Thief won't cause you to regain control of the
  artifact." An implementation that *masks* a conditional effect out of the
  projection while its condition is false — the obvious cheap design — passes
  every other assertion and fails exactly here, because masking un-masks when
  the source comes home. `sweepConditional` therefore **deletes**, and deletion
  is irreversible in the right way. Its sibling ruling ("if Master Thief ceases
  to be under your control before its ability resolves, you won't gain control
  of the targeted artifact at all") is CR 611.2b's "if the duration never
  starts, the effect does nothing": `arm` returns `Nothing` and nothing is
  stored at all. **Hag of Inner Weakness** ("target creature an opponent
  controls gets -2/-1 until your next turn") falsifies two designs at once: an
  `UntilEndOfTurn` stand-in dies at its own CR 514.2 cleanup, and an expiry
  implemented by scanning P4's event log for a matching `StepBegan` dies *at
  birth*, because the effect is created during an upkeep whose untap step has
  already happened this turn. **Added:** `Pawl.Type.Expiry`
  (`AtCleanup | Never | While PlayerId StateCondition | AtTurnOf PlayerId`) and
  `Pawl.Expiry` (`arm`, `dropAtCleanup`, `dropAtHandoff`, `sweepConditional`);
  `Duration.ForAsLongAs`/`UntilYourNextTurn`; `StateCondition.YouControlSource`
  (that vocabulary's third customer, after CR 603.8 state triggers and CR 603.4
  intervening "if"s); `TargetSpec.ArtifactTarget`/`OpponentCreatureTarget`;
  `Subtype.Rogue`/`Hag`/`Warlock`; `data/cards/master-thief.json` and
  `data/cards/hag-of-inner-weakness.json`. **Deleted:**
  `Projection.dropEndOfTurnEffects`, `Event.dropEndOfTurnReplacements` — two
  sweeps that differed only in which module their list lived in, now one
  `dropAtCleanup` over both carriers — and the `duration` field on
  `ContinuousEffect` and `ActiveReplacement`. **`Pawl.Target` became
  source-relative**: `legalRecipients`, `stillLegal` and `legalSets` all take
  the targeting source, because `OpponentCreatureTarget` is the first spec
  whose legal set depends on *who is choosing* (CR 109.5 with CR 613.1b —
  projected control, not ownership). **The design's one departure from the
  umbrella spec: event-relative durations are decided at the turn handoff, not
  by reading P4's event log.** The handoff transition *is* the event, known
  exactly and for free at the one site that performs it; reading
  `GameEvent.StepBegan` back out of a turn-scoped log would need a per-effect
  watermark to distinguish "your untap already happened this turn" from "your
  next turn began" — the exact trap Hag sets, and the log is cleared at the
  handoff anyway. Dropping at the handoff is *observably* identical to dropping
  "as the turn begins" (CR 500.12: no game events occur between turns; CR
  502.4: no player receives priority during the untap step; CR 704.3: SBAs and
  the projection are only consulted when a player would get priority), so no
  observer can tell the two apart. P6 still reads P4's work — `stateHolds` is
  the shared predicate evaluator. **Four departures from P6's own spec**, all
  corrections rather than drift: (1) `Expiry.arm` grew its parameters across
  three tasks (`Duration ->`, then `PlayerId -> Duration ->`, then the spec's
  final `PlayerId -> ObjectId -> Duration -> GameState ->`) because arriving
  four-parameter in task 1 leaves three unused parameters and `-Weverything` +
  `-Werror` fails the build; (2) `Target.legalSets` took the source parameter
  too, though the spec named only `legalRecipients` and `stillLegal` — it is a
  wrapper over the former; (3) a controller-relative spec whose source has left
  the battlefield yields an **empty** legal set, a rules deviation against CR
  608.2b's own last-known-information clause, kept because the narrower fix
  changes a signature four call sites use for a case no card in the pool
  reaches (#85); (4) the spec's single "codec round-trips" test is distributed
  — each new arm's round-trip landed with the arm, and each card file's is
  enforced for free by `CardsSpec.checkFile`. **No prompt was added and none
  elided.** Every value this phase computes is derived: CR 109.5's "you" off
  the effect's controller, the condition off the board. There is nothing to ask
  a player, so there is no elision and no expiry to name. **A pre-existing bug
  was found by a gate card and fixed inside the phase.**
  `Resolve.resolveEffects` re-read a triggered ability's controller from the
  source permanent's **live** projected controller; **CR 113.8** fixes it when
  the ability goes on the stack ("the controller of a triggered ability on the
  stack … is the player who controlled the ability's source when it
  triggered"). Stealing Master Thief while its ETB trigger was on the stack
  transferred the ability's "you" to the thief, contradicting the card's own
  ruling. Both producers already stamped the frozen value; only the reader was
  wrong. Filed as #81 and **closed** (`fb0f18d`, `11a9b20`). **The phase's
  transferable lessons.** (a) *Both gate cards' falsifiers passed on the first
  run* — the latch and Hag's cross-turn survival both worked immediately,
  because tasks 1–4 built the machinery before either card existed. That is
  what substrate-before-consumers buys, and it is the strongest evidence yet
  for the ordering. (b) *A single CR citation was wrong three times over*: the
  task brief said CR 113.7a, the implementer corrected it to CR 602.2a, and
  review found **CR 113.8** is the rule that actually states both halves and
  scopes them to the ability *on the stack*. P5 recorded citation drift as its
  lesson; this is the sharper form of it — every party did check `rules.txt`,
  and the failure was in *which rule to look up*, not in whether to look.
  (c) *Two review findings were defects in the plan's own prescribed tests*,
  not in the implementations: an assertion that could not distinguish "never
  stored" from "stored then swept" (fixed by asserting the CR 302.6 re-Sick
  residue, which the sweep cannot launder away), and a card-shape test titled
  "2/2" that never asserted P/T. Both fixes were then verified by deliberately
  injecting the fault they were supposed to catch. (d) *An honest negative
  result was kept*: one activated-ability regression passes with **and**
  without the #81 fix, because when control moves *before* activation the
  frozen and live controllers are definitionally equal — no test of that shape
  can discriminate. It was reported rather than quietly dropped, and the
  discrimination is carried by the two tests that do fail under a revert.
  (e) Writing a test surfaced a real fixture trap: a regeneration-shield
  payload intercepts the very `Event.destroy` used to remove its own source, so
  the fixture removes the source with `Event.changeZone` instead. **Tracking:**
  **#81 is closed.** #38 (`StateCondition` retired wholesale by P9's filter
  language) and #40 (the hand-carved `TargetSpec` family) are cited at the new
  arms, not retired. Three deferrals filed and cited in code: #82 (a synthetic
  `GainControl` activated ability stands in for a controller-sensitive printed
  one — none is in the pool), #83 (`resolveSpell` re-reads a spell's projected
  controller, the same shape as #81 and benign today), #84 (no card produces a
  conditional or turn-relative *floating replacement*, so `Expiry.While` on
  that carrier is covered only by a hand-built unit fixture and `AtTurnOf` on
  it has no test at all), plus #85 from departure (3). #62's cross-turn settle
  is untouched — no test this phase holds a permanent under conditional control
  across an untap step. **Two files were touched beyond the plan's
  expectations**, worth knowing for the next phase: `Pawl.Mana.subtypeMana`
  twice, because every new `Subtype` constructor breaks its exhaustive `case`;
  and `Pawl.Support`, which gained a shared `continuousEffectAffects` fixture.
  **Final suite 732/732**, warning-clean on a from-scratch `cabal clean` build,
  `hooky run` clean. `cabal bench`: goldfish 11.6ms, casting 12.6ms, fighting
  12.6ms — issue #66 (pre-existing) still makes all three benchmarks execute
  the identical game, so the per-scenario split is meaningless and the
  aggregate, ~12ms, is the only honest reading; unchanged from P5. Spec and
  plan kept as reference:
  `docs/superpowers/specs/2026-07-22-p6-conditional-event-durations-design.md`
  and `docs/superpowers/plans/2026-07-22-p6-conditional-event-durations.md`.
- **M4.5 P7 is complete** (player and rules-modifying continuous effects,
  GAP-P plus the *modification* half of GAP-Co, and with them the whole of
  Cluster 3). CR 611.1's third clause finally has a carrier: a continuous effect
  that "affects players or the rules of the game" rather than the
  characteristics of an object. **The structural fact the whole phase rests on
  is that this axis is *outside* the layer system.** CR 613.1 makes the seven
  layers a machine for computing an *object's* characteristics; CR 613.10 and CR
  613.11 apply their effects *after* that machine has run. So there is no new
  `Layer`, no new `Modification`, no `Affected` constructor, and
  `Pawl.Projection` was **read from and never edited** — mechanically proven,
  not asserted: `git diff --stat 1b5d24a -- Pawl/Projection.hs
  Pawl/Type/{Layer,Modification,Affected,ContinuousEffect,StaticAbility,Player}.hs`
  is empty across the phase's twelve commits. The first invariant has the same
  kind of audit from the other side: a tree-wide grep for any `PlayerEffect.`,
  `PlayerScope.` or `SpellCriterion.` constructor outside `Pawl.PlayerEffect`,
  `Pawl.Codec` and `Pawl.Type.*` returns nothing, so **`Pawl.PlayerEffect` is
  the sole rules home of casing on this axis** — the standing that `Pawl.Resolve`
  has over `Effect`, `Pawl.Projection` over `Modification` and `Pawl.Expiry`
  over `Expiry`. `Pawl.Resolve` names `AffectPlayers` as an opcode and passes its
  payload through *without importing the payload's type at all*: the invariant
  is enforced by the import list, not by discipline. **Five gate cards, each
  falsifying a different fixed point.** **Rule of Law** ("each player can't cast
  more than one spell each turn") makes the count a fold over P4's *whole* turn
  log rather than anything watermark-bounded, because the spell that used up the
  allowance is Rule of Law itself — cast before the effect existed — and the
  counted event is the **cast** (CR 601.2i), so a countered spell still counts;
  its own ruling demands exactly this ("looks at the entire turn … even if Rule
  of Law wasn't on the battlefield when that spell was cast"). **Thalia,
  Guardian of Thraben** proves the tax has to land on *both* castability and
  payment: taxing only the payment underpays the offer, and taxing only the
  offer wedges a game that has no rewind (#56). **Sapphire Medallion** is CR
  118.7a — a reduction takes only the *generic* component, so `{1}` off `{U}`
  leaves `{U}` — and, crossed with Thalia on a cost with **no** generic
  component at all, is the one board shape that can tell CR 601.2f's order
  (every increase, *then* every reduction) from its reverse. **Reliquary Tower**
  makes "no maximum hand size" a `Nothing` threaded end to end with no sentinel,
  and splits CR 402.2 into its own rule with its own seven, separate from CR
  103.5's opening hand: `defaultMaximumHandSize` is an independent literal, and
  `Setup.openingHand` now serves only the opening draw — the two sevens the
  rules keep apart are finally apart in the engine. **Silence** is CR 611.2c's
  third sentence, and the card does *literally nothing* without it: 611.2c's
  first sentence freezes a stored effect's object set, but its third carves out
  exactly this axis — such an effect "modifies the rules of the game, so it can
  affect objects that weren't affected when that continuous effect began" — so a
  stored `ActivePlayerEffect`'s `scope` is recomputed fresh on every `applying`
  call and never frozen, while its `controller` *is* baked in at creation
  (Silence is an instant, so by the time its effect is live the source is in a
  graveyard with no controller left to project). **A census correction, filed so
  a later phase does not inherit it.** `docs/mtgish-gap-census.md` §3.2 lists
  `SkipsUntapStep`/`SkipsDrawStep`/`SkipsMainPhase` under `PlayerEffect`. They do
  not belong on this axis: **CR 614.1b** is explicit that "effects that use the
  word 'skip' are replacement effects", so they are P5's carrier, not P7's.
  Filed as #98. `Engine.skipsDraw`'s CR 103.7a first-turn skip is a *turn-based
  rule* rather than an effect and correctly stays where it is. **Added:**
  `Pawl.Type.PlayerEffect` (`CantCastSpells | CantCastMoreThan |
  IncreaseSpellCost | ReduceSpellCost | NoMaximumHandSize` — increase and reduce
  are two constructors, never one signed delta, because CR 601.2f orders them
  and CR 118.7a restricts only the reduction), `Pawl.Type.PlayerScope`,
  `Pawl.Type.SpellCriterion`, `Pawl.Type.PlayerStaticAbility` (the printed
  carrier) and `Pawl.Type.ActivePlayerEffect` (the stored one); `Pawl.PlayerEffect`
  (`applying`, `inScope`, `prohibitsCasting`, `castsThisTurn`, `costAdjustments`,
  `matchesSpell`, `maximumHandSize`, `defaultMaximumHandSize`) and `Pawl.Cost`
  (`total`, `applyAdjustments`); `Card.playerAbilities`,
  `GameState.playerEffects`, `Effect.AffectPlayers`, `GameEvent.SpellCast`,
  `Event.castOf`, `Subtype.Soldier`; six read sites (`Cast.castable`,
  `Cast.castableWhileSearching`, `Cast.castSpell`, `Engine.discardToHandSize`,
  `Pawl.Expiry`'s three sweeps, `Pawl.Resolve`); and five card files. **Four
  deliberate departures from P7's own spec**, refinements rather than drift:
  (1) `Pawl.Cost` is factored into a stateful `total` and a **pure**
  `applyAdjustments :: ([Natural], [Natural]) -> ManaCost -> ManaCost`, which is
  what lets the CR 601.2f order and the CR 118.7a floor be unit-tested without a
  board; (2) the total cost is **canonicalized** — one leading `Generic` symbol,
  then the printed typed symbols in order, a zero generic component dropped
  entirely — because the spec's own order test demands the answer be *exactly*
  `{U}`, and `Mana.spend` sums every generic symbol anyway, so this is
  presentation and not semantics; (3) the printed gather honours **CR 305.7**,
  which the spec does not mention: Reliquary Tower is a nonbasic land and Blood
  Moon is in the pool, so an ability read straight off the card would survive
  having its land's subtype set to a basic type — the gather reuses
  `Projection.liveGiven`, exactly as `Projection.gather` does, and the
  differential test (Tower alone → `Nothing`; Tower under Blood Moon → `Just 7`)
  proves the strip really happens; (4) `applying` grew its **second** carrier in
  the seventh task rather than arriving with both, so no task left a
  `GameState` field that nothing writes. **No prompt was added, and one is asked
  less often.** `Prompt.ChooseDiscard` is skipped *entirely* for a player with no
  maximum hand size — the absence of a choice, not the making of one — rather
  than prompting for zero cards. The single elision is CR 601.2f's "if multiple
  cost reductions apply, the player may apply them in any order" (#88):
  `applyAdjustments` sums, which is equivalent not to *some* order but to
  **every** order, because CR 118.7a routes every reduction P7 can express to the
  same generic component. **The phase's transferable lesson is about
  citations, and it is sharper than P5's or P6's.** The *plan itself* shipped
  three classes of rule-citation error, and review caught all three — though
  the CR 102.1 fix initially landed only in `Pawl.PlayerEffect.inScope` and
  missed its sibling comment in `Pawl.Type.PlayerScope`, which the final
  whole-branch review caught and corrected on its own pass: CR 102.1
  where 102.2 was meant (102.1 defines *player*; 102.2 is the two-player
  opponent rule, and 102.3's teams are exactly why the implementation's
  `pid /= controller` carries a documented two-player assumption); CR 118.7e for
  601.2f's "reductions in any order" (118.7e is about *hybrid* symbols, and the
  sentence lives in 601.2f); and CR 613.11 attached at **four** sites to claims
  it does not support — it governs the *application order* of rules-modifying
  effects and explicitly defers cost order to 601.2f, so it substantiates the
  tier, `costAdjustments` and `maximumHandSize`, but says nothing about which
  spells a criterion admits (CR 613.1d/613.1e do). **Every one of the three was a
  wrong justification attached to correct behaviour**, which is the failure mode
  worth naming: a citation error never showed up as a failing test, only as a
  reader being misled, and the only thing that caught any of them was somebody
  opening `rules.txt`. Two implementers refused a brief-specified citation on
  that evidence and were right both times. **Two of the plan's own tests were not
  discriminating**, and strengthening them was not weakening the plan: the
  turn-handoff sweep for the new stored carrier stored one entry and asserted
  the list empties, which a `\_ -> False` keep-predicate would also satisfy; and
  the conditional sweep used a stand-in source that was never on the battlefield,
  so its condition was false from construction and no case proved an effect
  *survives* while its condition holds. Both were fixed and then verified by
  **sabotage** — break the keep-predicate, watch the new case fail with real
  output, revert, watch it pass — and the first sabotage attempt landed on
  `dropAtCleanup`'s identically shaped line and was caught by the *wrong* test
  failing, which is itself evidence the cases now discriminate. Separately,
  `Cast.castableWhileSearching`'s prohibition gate was implemented but **untested**
  — a stated spec requirement running unproven — and is now covered, with a
  positive control so the negative assertion cannot pass because the Panglacial
  Wurm was never castable at all. **Tracking:** **#3 is closed**, and with it
  git-bug `c5a985d` (GAP-P) and the *modification* half of GAP-Co; **#4** (P8,
  the cost *payment* half) stays open, and `Pawl.Cost.total` cites it where
  additional and alternative costs would land. #38 (`StateCondition`), #39
  (`CountSpec`) and #40 (the `TargetSpec` family) are cited and **not** retired —
  `Pawl.Type.SpellCriterion` joins that list as a **fourth** member of the family
  P9's filter language replaces, and says so in its own header. Eleven deferrals
  filed and cited at their code sites: #88 (CR 601.2f's reduction order) and #89
  (`castSpell` computes the total cost against an object still in *hand*, CR
  601.2a having already moved it to the stack) were filed during the phase rather
  than batched at close-out; #90 (activated-ability cost modification has no
  producer — `Pawl.Activate` hands `AbilityCost.mana` straight to `Pawl.Mana` and
  never reaches `Cost.total`), #91 (CR 118.7b–g's colored, colorless, hybrid,
  Phyrexian and snow reductions are unrepresentable behind `ReduceSpellCost`'s
  bare `Natural`), #92 (CR 613.10's *player*-affecting tier — protection from red
  for a player — is a genuinely distinct tier from 613.11's rules tier and has no
  constructor), #93 (CR 613.10/613.11 both order by timestamp; `applying` returns
  its effects unsorted, unobservably so because none of the five constructors
  conflicts with another, and the field is stored so the fix is a sort rather
  than a migration — expires on Null Profusion + Reliquary Tower, the pair
  Reliquary Tower's own ruling names), #94 (CR 601.2f's "locked in" total cost is
  recomputed on demand rather than stored), #95 (CR 601.3a's quality-bearing
  prohibitions are unrepresentable — `prohibitsCasting` takes no `ObjectId`
  because both of P7's prohibitions are quality-free; Void Winnower is 601.3a's
  own worked example), #96 (player-scoped casting and land-play permissions have
  no carrier: `Card.castingPermissions` is object-scoped for Panglacial Wurm, and
  `Pawl.Action`'s land gate is a `Set PlayerId` where CR 305.2a wants a count),
  #97 (no card arms a conditional or turn-relative expiry on the stored player
  carrier, the sibling of #84 on the third and last carrier) and #98 (the CR
  614.1b skips correction above). **Final suite 788/788**, warning-clean on a
  from-scratch `cabal clean` build, `hooky run` clean. `cabal bench`: goldfish
  11.7ms, casting 12.9ms, fighting 12.9ms against P6's 11.6/12.6/12.6 — issue
  #66 (pre-existing) still makes all three benchmarks execute the identical game,
  so the per-scenario split is meaningless and the aggregate is the only honest
  reading. The +0.3ms is roughly a third of the suite's own ±0.9ms stddev and
  reproduced across two runs, so it is **inside the noise, not a clean
  no-change**: `Cast.castable` now calls `PlayerEffect.applying` per card in hand
  per `legalActions`, each call walking the battlefield, which is real work the
  benchmark cannot resolve. If `legalActions` ever gets hot, hoisting the
  spell-independent `prohibitsCasting` out of the per-card loop is the first
  move. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-22-p7-player-effects-design.md` and
  `docs/superpowers/plans/2026-07-22-p7-player-effects.md`.
- **M4.5 P8 is complete** (cost generalization and alternative costs, the
  *payment* half of GAP-Co — and with P7's modification half, the whole of
  GAP-Co is now closed). pawl has **one `Cost` type for spells and activated
  abilities**: `{mana :: Maybe ManaCost, components :: [CostComponent]}`, where
  the `Maybe` carries CR 118.6's own distinction — `Nothing` is an *unpayable*
  cost ("this card has no mana cost"), `Just (MkManaCost [])` is a payable `{0}`
  (Ornithopter) — in the type rather than inferred, and every existing ability
  migrated `Nothing → Just (MkManaCost [])` across the card files and the
  fixtures so the two are never conflated again. **Three gate cards, each
  falsifying a different fixed point.** **Greed** ("{B}, Pay 2 life: Draw a
  card") is CR 118.3's own worked example: a payability check that ignores the
  *amount* passes at 20 life and at 1 alike, but the real check fails at 1 and —
  the sharp case — at **2** life the payment is legal and *loses the game* (CR
  704.5a), which is what proves paying life is a real life-total change and not
  a flag. **Village Rites** ("As an additional cost to cast this spell,
  sacrifice a creature") is CR 601.2f: the additional cost is folded **inside**
  the total cost, so with no creature the spell is *not castable at all* — the
  castability gate, not resolution, refuses it — and the payment runs through
  `Event.sacrifice`, so Khabál Ghoul counts the creature that died. **Fireblast**
  ("You may sacrifice two Mountains rather than pay this spell's mana cost")
  casts a `{4}{R}{R}` spell from two **tapped** Mountains and an empty pool:
  "castability is mana affordability" and "an alternative cost is a different
  `ManaCost`" both die at once, because Fireblast's alternative is `Just []` — a
  real, taxable `{0}`, not `Nothing`. **Two cross-checks cash the structural
  bet**, and both passed on the first run with no engine change — which is what
  validates the two feature tasks: **Blood Moon** proves
  `PermanentCriterion.PermanentOfSubtype` reads the *projection* — a nonbasic
  land that Blood Moon has made a Mountain may be sacrificed to Fireblast as one
  — and **Thalia** proves the alternative's `Just []` is a genuine `{0}` a cost
  increase can tax (CR 118.9d): Fireblast's free alternative still costs `{1}`
  under Thalia. **The correction to the spec.** §2.9 claimed "ability costs
  route through `Cost.total` too, nothing changes observably." That is false, and
  the falsifier is already in the pool: `PlayerEffect.matchesSpell` classifies an
  **object**, not a spell (`SpellCriterion.NoncreatureSpell` is "no creature card
  type on the projection"), so a noncreature *permanent* matches it, and Thalia
  would tax Mindslaver's `{4}` activation to `{5}` — Thalia taxes noncreature
  **spells**. So `Pawl.Activate` pays the ability's **printed** cost, **#90 was
  commented, not closed** (routing an ability cost through `total` is a
  regression, not a no-op), and a Thalia × Mindslaver regression test in
  `Pawl.CostSpec` pins it. **Added:** `Pawl.Type.Cost`, `Pawl.Type.CostComponent`
  (`TapThis | SacrificeThis | PayLife Natural | Sacrifice Natural
  PermanentCriterion` — `SacrificeThis` is deliberately not `Sacrifice 1 this`,
  because CR 602.1a's self-referential cost offers no choice) and
  `Pawl.Type.Payment` (`Paid | Unpaid`, the no-boolean-blindness door);
  `Pawl.Cost` grew from one arithmetic step into the axis's **sole casing home**
  — `costsFor`, `total` (now `Cost -> Cost`), `canPay`, `canPayComponent`, `pay`,
  `payComponents`, `payComponent`, `requiresTapSymbol`, `substituteX`,
  `hasVariable`, `matchesCriterion`, `sacrificeCandidates`, `unpayable`,
  `firstOffered`; `Card.additionalCosts` and `Card.alternativeCosts`;
  `PermanentCriterion.PermanentOfSubtype`; `Prompt.ChooseCost`/`ChooseSacrifices`
  and their responses; `Pawl.Cast.payableCost`; three card files (Greed, Village
  Rites, Fireblast); and `Pawl.CostSpec`. **Retired:** `Pawl.Type.AbilityCost`,
  `Pawl.Type.AdditionalCost`, `Pawl.Cast.costOf`,
  `Pawl.Activate.canPayAdditional`/`payAdditional`. **Six deliberate departures
  from the spec:** (1) the correction above — ability costs are paid at printed
  value, #90 commented; (2) `Cost.pay` is **transactional** — it captures the
  entry state and restores it on `Unpaid`, because it spends mana and pays
  earlier components before it reaches a `ChooseSacrifices` prompt, so a rejected
  answer would otherwise leave lands tapped and a creature dead with no spell
  cast; (3) `Cost` gained `substituteX`/`hasVariable`/`unpayable`/`firstOffered`,
  which the spec did not name, to state the CR 601.2b X=0 floor once and give the
  nine `ChooseCost` fallback sites one answer instead of nine copies; (4)
  `PermanentCriterion` is matched at **two** sites — `Cost.matchesCriterion`
  alongside `Replacement.matchesPermanent` — because sharing would make
  `Pawl.Cost` import `Pawl.Replacement` and collide with #72's queued CR 614.12b
  fix (filed as #111); (5) `Cost.costsFor` arrives whole and grows twice rather
  than surviving as a discarded `Cast.costOf` intermediate; (6)
  `Cast.payableCost` is a named predicate, not three copies of the same
  `canPay ∘ total ∘ substituteX 0` chain. **Two prompts, each elided only when
  forced:** `ChooseCost` is skipped when exactly one candidate is payable,
  `ChooseSacrifices` when the candidates exactly equal the count — but three
  payable Mountains and a count of two is a *real* choice and is asked. The one
  genuine elision is CR 601.2h's payment order (#105), unobservable because no
  component in this vocabulary changes another's payability. **A plan-test
  correction, faithful not weakening:** Village Rites' "resolves" test asserted
  the pre-sacrifice `ObjectId`s appear in the graveyard, but `Event.changeZone`
  mints a **new** `ObjectId` per CR 400.7, so the assertion was corrected to a
  count (`length pikers + 1`) — the same fact, checked the way the rules make it
  checkable. **Tracking:** **closes #4** (M4.5 P8) and the *payment* half of
  GAP-Co, which P7's spec left explicitly open; **#90 commented, not closed**;
  #38/#39/#40 cited by #111 and **not** retired; **fourteen deferrals filed and
  cited at their code sites** — #99 (a variable-`Quantity` life payment, filed
  during the phase) plus #100 (flashback has no `CastFromGraveyard` carrier),
  #101 (which cost was paid is not recorded), #102 (optional additional costs /
  kicker), #103 (effect-granted alternative costs), #104 (CR 118.10 across two
  components), #105 (CR 601.2h payment order), #106 (CR 118.13
  hybrid/Phyrexian), #107 (CR 118.12 costs paid at resolution), #108 (discard-
  and exile-as-cost components), #109 (CR 118.6a alternative on an unpayable
  cost), #110 (CR 118.8c's "if able" exception), #111 (`PermanentCriterion`
  matched at two sites) and #112 (`ChooseSacrifices`'s per-component scoping).
  **Final suite 834/834**, warning-clean on a from-scratch `cabal clean` build,
  `hooky run` clean. `cabal bench`: goldfish 11.4ms, casting 12.9ms, fighting
  12.9ms against P7's 11.7/12.9/12.9 — #66 (pre-existing) still makes all three
  benchmarks execute the identical game, so the aggregate is the only honest
  reading, and it is **flat**: the phase's added work is a per-card `costsFor`
  list build and a `null`-components `all`, inside the suite's own noise. Spec
  and plan kept as reference:
  `docs/superpowers/specs/2026-07-22-p8-cost-generalization-design.md` and
  `docs/superpowers/plans/2026-07-22-p8-cost-generalization.md`.
- **M4.5 P9 is complete** (the target-filter predicate language, GAP-F). The
  decision it proves: **one first-order, non-recursive predicate language,
  `Pawl.Type.Filter`, subsumes the whole hand-carved classification family** —
  `Pawl.Type.TargetSpec`'s per-card variants, `CardCriterion`,
  `PermanentCriterion`, `SpellCriterion`, and `Affected`'s dynamic sets — and
  does it across BOTH projected (battlefield/stack) and printed (off-battlefield)
  subjects through one identity-blind evaluator, `Pawl.Filter.matches ::
  Context -> View -> Filter -> Bool`. `Filter` (`HasCardType`, `HasSupertype`,
  `HasColor`, `HasSubtype`, `PowerAtLeast Integer`, `ControlledBy
  PlayerRelation`, plus `And`/`Or`/`Not`) cases only on characteristics, never
  on an effect's identity — the same legitimate act as casing on a `CardType`.
  **Three gate cards, each a different pool of `TargetSpec`.** **Doom Blade**
  (`{1}{B}` "Destroy target nonblack creature") — `MkTargetSpec Creatures (Just
  (Not (HasColor Black))) IncludesSource` replaces the old
  `NonblackCreatureTarget`. **Terror** (`{1}{B}` "Destroy target nonartifact,
  nonblack creature. It can't be regenerated.") — `And [Not (HasCardType
  Artifact), Not (HasColor Black)]`, the first two-atom `And` a real card needs.
  **Reprisal** (`{1}{W}` "Destroy target creature with power 4 or greater. It
  can't be regenerated.") — `PowerAtLeast 4` against the projection, not the
  printed card, so a pumped 2/2 is a legal target and an unpumped one is not.
  **Two non-target cross-checks**, proving the same evaluator covers printed-card
  search and the cost-generalization phase's `PermanentCriterion`: the basic-land
  search (`Effect.Search` now carries a `Filter`, matched via the new
  `Projection.viewOfCard :: Card -> Filter.View` builder for off-battlefield
  cards, whose `power`/`controller` are vacuously `Nothing`) and Fireblast's
  sacrifice-two-Mountains alternative cost (P8's `PermanentCriterion`, now a
  `Filter`, matched against the live projection so a Blood-Moon'd nonbasic land
  still qualifies as a Mountain). **`TargetSpec` is now `MkTargetSpec Pool
  (Maybe Filter) Exclusion`** — a closed `Pool` (CR 115's candidate-kind enum,
  unchanged), an optional `Filter` (`Nothing` = the whole pool, e.g. bare
  "target creature"), and `Exclusion` (`IncludesSource`/`ExcludesSource`, CR
  601.2c's "another") carrying self-exclusion as a slot property rather than a
  `Filter` atom — closing #40. `Affected`'s dynamic sets are now `Matching
  Exclusion Filter`, re-derived each projection against the partial
  characteristics accumulated through the layers already applied; **self-
  exclusion is PRESERVED**, not dropped — a plan correction made during
  execution, since `opalescence.json` is a live producer and its "each other"
  self-exclusion test (`ProjectionSpec.hs`, "Opalescence is not itself a
  creature") would have regressed had the exclusion been elided. **Added:**
  `Pawl.Type.Filter`, `Pawl.Type.Pool`, `Pawl.Type.Exclusion`,
  `Pawl.Type.PlayerRelation`; the `Pawl.Filter` evaluator (`View`, `Context`,
  `matches`); `Projection.viewOfObject`/`viewOfCard`. **Retired outright** (no
  compat shims): `Pawl.Type.CardCriterion`, `Pawl.Type.PermanentCriterion`,
  `Pawl.Type.SpellCriterion`, and the old per-card `TargetSpec` variants
  (`NonblackCreatureTarget`, `WallTarget`, `ArtifactTarget`,
  `OpponentCreatureTarget`, …) and `Affected` constructors (`AllCreatures`,
  `AllLands`, `AllNonbasicLands`, `CreaturesOfColor`, `OtherNonAuraEnchantments`).
  **Regeneration** (Terror's/Reprisal's "It can't be regenerated" clause) is out
  of scope — pawl has no regeneration shield yet — filed and cited at the gate
  test (#113), the same precedent P8's cross-checks set. **#38/#39
  re-scoped, not closed**: `StateCondition` and `CountSpec` keep a second
  concept, scope + aggregation + threshold comparison, that `Filter` does not
  reach; their expiry moves off "milestone P9" onto the deferred count/compare
  phase. **One new deferral filed during close-out**: Opalescence's Oracle text
  is "each other NON-AURA enchantment", but `Aura` is not a modelled `Subtype`
  and the retired `OtherNonAuraEnchantments` arm never enforced it either, so
  the qualifier stays unenforced and unreachable until Aura exists (#114).
  **Tracking:** closes #5 (M4.5 P9) and #40 (`TargetSpec` family retired); #111
  (`PermanentCriterion` matched at two sites) closes as a side effect of the
  merge into one `Filter`; #38/#39 commented, not closed; #113/#114 are the
  phase's two live deferrals. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-22-p9-target-filter-predicate-language-design.md`
  and
  `docs/superpowers/plans/2026-07-22-p9-target-filter-predicate-language.md`.

- **M4.5 P10 is complete** (player-counter substrate + poison + energy,
  GAP-C). **Gates: Glistener Elf** (`{G}` Creature — Phyrexian Elf Warrior,
  "Infect.") and **Longtusk Cub** (`{1}{G}` Creature — Cat, "Whenever Longtusk
  Cub deals combat damage to a player, you get {E}{E}. Pay {E}{E}: Put a
  +1/+1 counter on Longtusk Cub."). The decision it
  proves: **a player-counter substrate disjoint from object counters** —
  poison and energy live on `Player`, not on any `Object`, so a wholesale
  player loss (CR 704.5c's ten-or-more-poison SBA) and a spendable player
  resource are both representable without a fictional "player object"; **infect
  is a deal-time classification bit, not a rewritten damage event** —
  `DamageEvent.dealtByInfect :: Bool` mirrors the established
  `dealtByDeathtouch :: Bool` idiom, and `Damage.applyDamage`'s `markOne`
  fold — already branching on `Recipient` (creature vs. player) — grows one
  more branch per recipient (mark damage vs. `-1/-1` counters to a creature;
  life loss vs. poison counters to a player) reading the bit; the CR 616
  replacement loop and everything upstream of it is untouched; and **energy
  is a bidirectional player counter** — the same
  `PlayerCounterKind` that poison uses is gained by an effect and spent by a
  cost, so no separate "energy pool" type was needed. **Added:**
  `Pawl.Type.PlayerCounterKind` (`Poison | Energy`, distinct from
  `Pawl.Type.CounterKind` — CR 122.1's marker sits on either "an object or
  player", and pawl keeps those two placements as separate types rather than
  one shared vocabulary); `Player.counters :: Map PlayerCounterKind Natural`;
  `Keyword.Infect` (702.90, ordered after
  `Fear`/before `Devoid`); `DamageEvent.dealtByInfect`; `Effect.GainPlayerCounters
  PlayerCounterKind Quantity` (targetless, CR 107.14's "you" reading, subsumes
  energy/experience/rad without a new opcode per kind);
  `CostComponent.PayEnergy Natural`; `TriggerCondition.SelfDealsCombatDamageToPlayer`
  (CR 510.1b/510.2, filtered from the existing `DamageDealt` event log, no new
  recording); and the poison-at-ten SBA (CR 704.5c) beside the life-total-loss
  SBA in `Pawl.Sba`. **One incidental fix surfaced by Longtusk Cub's own
  activated ability:** `Activate.activateAbility` now binds the source
  permanent under the reserved self slot before resolving (CR 113.7), so "put
  a +1/+1 counter on Longtusk Cub" resolves as a slot read exactly as
  `Engine.placeOne` already does for a triggered ability's source — additive,
  since no existing activated ability read the self slot before. **Nine
  deferrals filed at close-out**, each `expires:card-driven`: the counter→
  layer-6 ability-granting path (#116, rest of GAP-C); toxic, CR 702.164
  (#117); poisonous, CR 702.70 (#118); proliferate, CR 701.27 (#119); a
  targeted player-counter effect (#120, cited at `Effect.GainPlayerCounters`);
  a variable energy cost (#121, cited at `CostComponent.PayEnergy`); the CR
  614 player-counter replacement funnel (#122, cited at `Damage.applyDamage`'s
  infect arm — energy and poison are both added directly today, with no
  replacement-effect opportunity for a doubler); experience/rad counters
  (#123); and mana of any color (#124, the mechanism that blocked Aether Hub
  as the energy gate card, unrelated to counters). Two-Headed Giant poison
  sharing (CR 704.6b/810) stays out of scope, no issue filed. **Tracking:**
  closes #6 (M4.5 P10); #116–#124 are the phase's nine live deferrals. Spec:
  `docs/superpowers/specs/2026-07-23-p10-player-counters-design.md`.

- **M4.5 P11 is complete** (the command zone, emblems, and the monarch — GAP-Z
  and the monarch customer of GAP-S — **closing M4.5**). **Gates: Palace
  Jailer** (`{2}{W}{W}` Creature — Human Soldier, "When Palace Jailer enters
  the battlefield, you become the monarch. When Palace Jailer enters the
  battlefield, exile target creature an opponent controls until an opponent
  becomes the monarch.") and a **labeled synthetic emblem source**
  (`S.anthemEmblemCard`, Support.hs — pawl models no planeswalker or the Ring
  to mint a real one; #125). The decisions it proves: **the command zone is a
  seventh zone whose residents' static abilities function *from* the zone,
  off the battlefield** — the projection's uniform static-ability walk grows
  one more gather pass over `GameState.command`, the same walk it already
  runs over permanents, so an emblem's anthem folds into layer 7c without the
  layer system or the projection's battlefield-only assumptions changing;
  **the monarch is a single game-wide designation, not a per-player
  counter** — `GameState.monarch :: Maybe PlayerId` sits beside (not inside)
  `Player`, because CR 725.3 makes at most one player the monarch at a time,
  the opposite shape from P10's per-player counter map; **the monarch's two
  CR 725.2 triggers are genuinely sourceless** — `Source.OfInherentTrigger
  PlayerId (TriggeredAbility Card)` parallels the existing `DelayedTrigger`
  pattern (a controller baked in, no live object), and the trigger scanner
  synthesizes both abilities purely from `monarch = Just p`'s *presence*,
  never from casing on a card; and **exile-until-a-designation-changes needs
  a dedicated carrier, not an `Expiry`** — P6's `Expiry` sweeps are
  delete-and-recompute only (dropping a stored effect so the next projection
  reverts it), and no sweep performs a zone change, so a physically-exiled
  permanent cannot be recomputed back onto the battlefield; Palace Jailer's
  return instead uses `GameState.exiledUntilMonarch` + `Effect.ExileUntilMonarch`
  + `Monarch.returnExiledForMonarch`, wired into `settleForPriority`, with the
  observable duration ("until an opponent becomes the monarch") matching the
  spec even though the mechanism is a dedicated carrier rather than the
  spec's proposed `Expiry` condition. **Added:** `Zone.Command`;
  `GameState.command :: Set ObjectId`, `GameState.monarch :: Maybe PlayerId`,
  `GameState.exiledUntilMonarch`; `Source.OfEmblem Card`,
  `Source.OfInherentTrigger PlayerId (TriggeredAbility Card)`;
  `Effect.CreateEmblem Card`, `Effect.BecomeMonarch MonarchTarget`,
  `Effect.ExileUntilMonarch`; `Pawl.Type.MonarchTarget` (`TheController` |
  `ControllerOfSource`, CR 725.2's two distinct "who becomes the monarch"
  readings — pawl has no general player-spec for effects yet, so this is the
  minimal two-constructor shape the phase needs); `GameEvent.BecameMonarch
  PlayerId`; `TriggerCondition.CreatureDealtCombatDamageToMonarch` (not
  bearer-scoped, unlike P10's `SelfDealsCombatDamageToPlayer` — it matches
  any creature whose combat damage recipient is the current monarch, riding
  P4's existing `DamageDealt` event history); `Pawl.Monarch` (the inherent
  trigger scanner and placement, and the exile-return sweep); and the
  projection's command-zone gather pass plus a source's-controller
  perspective for `Projection.affects` (the emblem's anthem is the first
  affected-set filter to read a player's control, so `affects` gained a real
  perspective argument where it previously hard-coded `Nothing`). **Eight
  deferrals filed at close-out:** the synthetic emblem source itself (#125,
  cited at its fixture); `GameState`/`Object`/`Source` serialization (#126 —
  no codec exists for any of the three today, the same wall P10 hit for
  `Player.counters`); command-zone casting and the CR 903.8 Commander tax
  (#127, no gate card); CR 725.4 monarch reassignment when a player leaves
  the game (#128, related to #87, both blocked on real multiplayer support
  and both leaning on the project-wide two-player `Opponent` assumption, CR
  102.2); CR 725.5's no-monarch no-op (#129, unexercised — Palace Jailer
  always establishes a monarch before its own duration is checked); the rest
  of the GAP-S backlog — day/night, the Ring/Ring-bearer, initiative,
  venture, speed, experience/rad counters (#130, census §3.4); the other
  command-zone residents CR 309–315 name — dungeon, plane, scheme, vanguard,
  conspiracy, and their format variants under CR 408.3 (#131, each its own
  subsystem or format); and, found during the phase's review, an APNAP
  ordering gap between the monarch's inherent triggers and normal pending
  triggers (#132 — `Engine.placePendingTriggers` places `inherent` triggers
  unconditionally after the APNAP-`orderPending`-ed normal batch, so CR
  603.3b's own-order choice cannot interleave the two when a player controls
  one of each in the same batch; no pool card produces the collision yet).
  **Tracking:** closes #7 (M4.5 P11, the umbrella) — **M4.5 is complete**.
  Spec: `docs/superpowers/specs/2026-07-23-p11-command-zone-emblems-monarch-design.md`.

## M5 (phased)

- **M5a is complete** (Controlling Another Player — CR 723 — the first M5 phase;
  a *close-out* of the `Decider` bet placed on day one and wired through M4a, not
  a new axis). **Gate: Mindslaver** at gameplay level — alice activates a real
  Mindslaver through the driver loop targeting bob, the engine installs pending
  control (CR 723.1), promotes it on bob's turn (`Engine.handoffTurn`), alice
  makes bob's action/mode/target choices (and, in a sibling test, bob's combat
  attackers, CR 723.5) for bob's turn, and control lapses at the following turn
  boundary. The decisions it proves: **723 is an indirection, already built** —
  the substrate (`Pawl.Type.Decider`, `Decide.deciderFor`, `pendingControl`/
  `activeControl`, `Effect.ControlPlayerNextTurn`, the `handoffTurn` promotion)
  needed no change; M5a adds proof and edges, not machinery. The three edges:
  **723.1a** — player-controlling effects overwrite, last created wins, because a
  resolution-created continuous effect's creation time is its resolution time, so
  last-created = last-`Map.insert` on `pendingControl` (asserted with two effects
  at the same target, distinct controllers); **723.5a** — cost payment debits only
  the controlled player's resources (already true by construction: `Cost.pay pid`/
  `Mana.payCost pid`; the negative half — the controller's own Mountain and hand
  untouched — is now asserted); **723.6** — the controller cannot make the
  controlled player concede: concede is unbuilt, deferred as a new player-action
  axis (#133), with a durable guard comment at `Engine.priorityLoop`'s
  `ChooseAction` site stating concede must read the true player, never the
  `Decider`. **Added:** nothing in the library — zero new types, opcodes, or
  prompts; four gameplay/resolution tests (`GameSpec`, `ResolveSpec`) and one
  guard comment. **Deferred:** concede (#133); 723.2 limited-duration control
  (Word of Command, Opposition Agent; card-driven).

- **M5b is complete** (Restarting the Game — CR 727 — the second M5 phase;
  the lower-risk of the two game-lifecycle phases, and the one that introduces
  the primitive M5c reuses). **Gate: a labeled-synthetic "Restart" artifact**
  (the `Landform` crutch pattern; documented expiry → **Karn Liberated**, #135)
  whose activated ability's only effect is the new nullary `Effect.RestartGame`
  opcode. At gameplay level, bob activates it through the real priority loop; it
  resolves to `Setup.restartGame controller` and rebuilds the single `GameState`
  slot. The decision it proves: **restart is replace-in-place, not fresh setup**
  — `startGameFromCards` rebuilds every player's library from the *actual object
  pool* (each owner's `Source.OfCard` objects, wherever they sat; non-cards
  cease), ownership preserved (CR 727.2), and the starting player is the
  *restart's controller* (CR 727.1a), rotated to the head of the turn order (CR
  103) — both of which `Setup.emptyGame` + `Setup.newGame` would get wrong. The
  edges: **727.4** — the effect settles just before the first untap step (phase
  `Beginning Untap`, no priority, turn 1); **727.3** — a player owning fewer than
  seven cards draws from an empty library and loses at the next SBA check, reusing
  the existing draw-from-empty path. **Added:** `Setup.startGameFromCards`
  (reused verbatim by M5c) and `Setup.restartGame`/`rotateTo`; one nullary
  `Effect.RestartGame` opcode (six `Resolve` dispatch arms + two `Codec` arms +
  the `applyEffect` executor calling `Setup.restartGame`); the `synthetic-restart`
  card. **Deferred:** live `playGame` re-entry after an in-game restart (#134);
  full Karn Liberated's CR 727.5/727.5a exemption + put-onto-battlefield rider
  (#135, card-driven), which retires the synthetic gate; CR 727.6 subgame-restart
  (rides M5c) and CR 727.2's outside-the-game cards (subsystem-blocked), noted in
  #135.

- **M5c is complete** (Subgames — CR 729 — the M5 go/no-go, and the M5 exit).
  **Gate: a labeled-synthetic "Synthetic Subgame" sorcery** (the `Landform` crutch;
  documented expiry → **Shahrazad**, #139) whose one mode is
  `[PlaySubgame "loser", DealDamage "loser" (Literal 3)]`. The decision it proves,
  the day-one suspended-continuation bet (design.md §2.1/§3): **a subgame is a
  function call.** `Engine.playSubgame` runs
  `Trans.lift (runStateT (startGameFromCards >> playGame) sub0)` — the nested game
  sequenced into the parent's `StateT GameState (Program Prompt)`, so its prompts
  flow through the **same** `Program`/`Replay` fold the main game uses (untagged;
  scripted interpreters and deterministic replay work unchanged — a
  `Prompt.PlaySubgame` was **rejected** precisely because it would bypass
  `Replay.record` and break determinism). The parent state sits untouched in the
  outer frame while the subgame runs (CR 729.1a); **nesting (CR 729.6) is free
  recursion** — each level's `priorityLoop` re-supplies the runner, no `GameState`
  stack field. Its gate is a 2-level nested run plus a termination guard
  (`runGamePure` would simply not return if the recursion looped). CR 729.1a's
  isolation means a subgame's *internal* choices leave no trace in the parent
  `GameState` — but the interpreter **transcript** (`Pawl.Replay.record`'s
  `[Response]` log) *is* a top-level observable, and it discriminates nesting
  depth: each level's setup (`subgameStateFrom` → `startGameFromCards`) and
  `playSubgame`'s CR 729.5 funnel-back each shuffle every player's library once,
  so a flat (single-level) subgame gate contributes 4 `Response.Shuffled`
  entries (2 setup + 2 funnel-back) and this 2-level gate contributes 8. The
  gate asserts the measured count, so — like the other CR 729 gates — it **is**
  a self-verifying regression: a level-2-only breakage collapses the count to 4
  and the assertion catches it.
  **Runner injection:** `playSubgame` lives in `Engine` (it needs
  `playGame`) and is threaded **down** the spell path as a `Game Result` through
  new `Stack.resolveTopWith` → `Resolve.resolveSpellWith` → `Resolve.applyEffectWith`
  (bare names kept as `…With Resolve.noSubgame` wrappers, so **none** of the 105
  `resolveTop` / 9 `applyEffect` existing call sites changed) — the inversion that
  lets the bottom-layer resolver reach the top-layer loop without a cycle.
  **Outcome plumbing (CR 729.1b):** `Effect.PlaySubgame SlotName` **defines** its
  slot (the `Create` pattern, via `Resolve.definedSlots`), binding the derived
  2-player loser (`ToPlayer`); a later `DealDamage` reads it — enabled by
  `resolveSpellWith` **re-reading** the resolving object's bindings per effect
  (target legality still fixed at resolution start; only a newly-*defined* reserved
  slot, always vacuously legal, becomes visible), generalizing the mid-resolution
  binding `Create` writes but nothing yet read. **Construction/teardown:**
  `Setup.subgameStateFrom` (CR 729.2 — library cards only, players reset, id supply
  **inherited** so subgame ids never collide at return) and `Setup.funnelBack`
  (CR 729.5 — each owner's `OfCard` objects return to their main library, the
  parent's non-library board untouched, ids merged collision-free, supplies advanced);
  `playSubgame` reshuffles (`Prompt.Shuffle`). **Falls out for free:** CR 729.3
  (a <7-card library decks in the subgame's opening draw → loses at the first SBA,
  reusing the draw-from-empty path) and CR 729.4b (main-game player counters are
  outside the subgame — `funnelBack` never touches the parent's players; subgame
  counters cease when the subgame state is dropped). **Added:** `Setup.subgameStateFrom`/
  `funnelBack`; `Engine.playSubgame`; one opcode `Effect.PlaySubgame SlotName`
  (five classifier arms + a `definedSlots` arm + two `Codec` arms + `applyEffectWith`
  executor + `noSubgame` + `bindLoserSlot`); the `resolveSpellWith`/`resolveTopWith`
  runner-carrying variants; the `synthetic-subgame` card. **Deferred:** subgame
  first-player RNG (#136, elision), ability-path subgames (#137), Result-widening for
  multi-player non-winners (#138), full Shahrazad's half-life rider (#139,
  card-driven), and the subsystem-blocked slices — nontraditional/Vanguard/Commander
  movement, cards brought into a subgame, and subgame prompt tagging (#140).
  **M5 exits here:** control (M5a), restart (M5b), and subgames (M5c) close
  design.md §3's "nightmares"; the closed half is functionally complete for its
  flagged surface. Spec (umbrella) and plan kept as reference:
  `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`
  and `docs/superpowers/plans/2026-07-23-m5c-subgames.md`.

- **Mulligans (CR 103.5) are implemented** (issue-driven gap closure #141, not an
  M5 phase; surfaced by M5c's whole-branch review, which found `Setup.newGame` and
  `Setup.startGameFromCards` both drew an unconditional seven-card opening hand with
  no mulligan step). **What it establishes:** a new `Pawl.Mulligan` module owns the
  whole CR 103.5 process behind one entry point, `openingHands :: [PlayerId] ->
  Game ()`, which `Setup.newGame` (main game) and `Setup.startGameFromCards`
  (restart CR 727 + subgames CR 729) both call — so the London mulligan lands once
  for all three game-start paths. The loop: draw seven each (CR 103.5 sentence 1),
  then a **declare-all-then-take-all** round (the starting player declares first,
  then each other in turn order; all who chose to mulligan then do so), where taking
  a mulligan shuffles the hand back, redraws seven, and bottoms N cards (N =
  mulligans that player has now taken) in the player's chosen order. **Keeping is
  terminal** (CR 103.5): the loop threads a still-deciding pool and a kept player
  drops out permanently — it does NOT re-ask everyone each round, and the fix cannot
  be delegated to the interpreter (the prompt carries only the count, and count == 0
  cannot distinguish "never decided" from "kept immediately"). Bottoming reuses the
  existing `changeZone Hand → Library` bottom-append and `Event.drawCard` top-take —
  no new zone primitive — so the answered order becomes the library-bottom order.
  The per-player mulligan count is a **setup-local `Map`**, never a `GameState`
  field: no in-game effect asks how many mulligans a player took. **Falls out for
  free:** the CR 727.3 / 729.3 short-deck loss still fires "regardless of any
  mulligans," because a short library sets `drewFromEmpty` on the initial draw and
  that flag survives the whole loop. **Added:** `Pawl.Type.MulliganDecision`
  (`Mulligan | Keep`, a sum type — no boolean blindness); two prompts
  `Prompt.DeclareMulligan` and `Prompt.Bottom` (each carrying a `Decider`, so CR 723
  is satisfied for free — at setup `activeControl` is `Nothing`) with their
  `Response` mirrors and `Replay` encode/decode/defaultAnswer arms; `Pawl.Mulligan`
  (`openingHand`, `shuffleLibrary` — both moved out of `Setup` to break the import
  cycle one-way — and the `openingHands`/`mulliganRounds`/`takeMulligan` loop);
  `Pawl.MulliganSpec` (nine cases, including teeth-verified proofs that a chosen
  bottom *order* is honored and that a kept player is not re-asked). **Deferred:**
  CR 103.5c's multiplayer/Brawl free first mulligan (#148, `area:multiplayer`);
  CR 103.6 opening-hand actions — Leyline, Gemstone Caverns (#149, card-driven);
  CR 103.2a/b sideboards and companions (#150, card-driven). Spec and plan:
  `docs/superpowers/specs/2026-07-24-mulligans-cr-103-5-design.md` and
  `docs/superpowers/plans/2026-07-24-mulligans-cr-103-5.md`.
