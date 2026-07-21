# Milestone log

pawl's comprehensive-rules core is built milestone by milestone (M0 → M7; the
full path and the M3a–M3g split table are in `docs/design.md` §3). This file is
the **completion log**: one distilled entry per landed milestone — its gate
card, the load-bearing decision it proved, the opcodes and types it added, and
every **elision with its named expiry**. It moved out of `CLAUDE.md` to keep
that file to working guidance. The authoritative per-milestone detail is each
milestone's spec and plan under `docs/superpowers/{specs,plans}/`, cited at the
end of every entry.

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
  combat damage step is a splice; `git-bug 5f50eec` is closed). Spec and plan kept
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
  No new rules, zero opcodes. `git-bug 14138aa` is closed). Spec and plan kept as
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
  the matchup — `git-bug 15de615` is closed — and four Lightning Bolts in
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
  (`git-bug f90e0c4`). Performance: `Projection.projectAll` projects the whole
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
  `Ord`). Cards are deterministic fixtures (no random-game entry). `git-bug
  65ce714` (payCost must prompt when mana sources are distinguishable) stays
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
  direction when the gap surfaced. **Named expiries opened**: the rest of the
  numeric tower (`Star`/`Plus`/`Half`/`Infinite`/`Count`) stays shape-only, each
  due with its first card; X frozen to a `Literal` in a stored continuous effect
  (the `Projection.hs` note) is due with the first `+X/+X` card; X in
  activated-ability costs rides M3g's `AbilityCost.mana` and the same `ChooseX`;
  a general `Quantity.Bound SlotName` (git-bug `c7a0077`) generalizes the single
  reserved slot when a named or second amount lands; an engine-computed maximum X
  is a deferred UI nicety. git-bug `65ce714` (mana-source prompt) is unchanged — X
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
  elisions/expiries**: **CR 701.19c "can't be regenerated" is deferred to Wrath of
  God** (`Event.destroy` is ungated — no `Regenerability` argument, no mass-destroy
  opcode); CR 615.7 amount-shields and the multi-source choice, CR 615.10 static
  prevention, retaining prevented events for CR 615.13/615.5, a general "Regenerate
  target creature" (`Regenerate SlotName`), CR 701.19b static regeneration, and a
  distinct "was destroyed" event are all deferred to the first card that needs them;
  the 704.5f toughness-drop test still uses the synthetic `−0/−1` continuous effect
  until the first real **−N/−N** ability. Spec and plan kept as reference:
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
  elisions/expiries**: `Event.counter` is ungated — "can't be countered" (CR 701.6),
  conditional counters ("counter unless pay", Mana Leak/Daze), a distinct "was
  countered" event and its trigger, countering **abilities** (Stifle — needs an
  `AbilityTarget`), alternative counter destinations (counter-and-exile, Remand),
  and restricted counters ("counter target spell with mana value N") are each
  deferred to the first card that needs them. Spec and plan kept as reference:
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
  deferred expiries:** non-P/T counter kinds (keyword/charge/loyalty/poison/shield/
  stun, CR 122.1b–i); a "counter placed" event and its triggers (CR 122.7,
  proliferate); replacements that alter counter placement (Doubling Season/Hardened
  Scales); counters entering *with* a permanent (CR 122.6a, a replacement); "move a
  counter" (CR 122.5); the "can't have more than N counters" SBA (CR 122.4 /
  704.5r); and `Quantity.X`-many counters (rides M4a's `ChooseX` — `PutCounters`
  already carries a `Quantity`) — each due with the first card that needs it. Spec
  and plan kept as reference:
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
  timing, never on a card's identity. **Named deferred expiries:** modality on
  **activated and triggered abilities** is the design's own stated intent (§0 of
  the spec) and the *immediate* next step, not merely deferred; `ModeSelection`
  stays **`ChooseExactly`-only** until a card needs "choose two"/commands (CR
  700.2d — the same-mode-twice case, which turns `Binding.modes` from a `Set` into
  a multiset), escalate/additional-cost-per-mode (CR 700.2h), the pawprint "worth
  of modes" count (CR 700.2i), or another-player-chooses (CR 700.2e); **copy
  copies modes** (CR 700.2g — a future copy effect must carry the source's chosen
  `Set ModeIndex` rather than re-prompt) is ungated, no copy effect exists yet;
  **multi-mode slot-name collision** is unreachable until a selection picks ≥2
  modes, mitigation (qualify a bound slot by its `ModeIndex`) due with that card;
  and **a general subtype-restricted target** — `WallTarget` is specific to Wall,
  generalizing to a subtype-parameterized spec ("target Goblin"/"target Zombie")
  when the first such card needs it. Spec and plan kept as reference:
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
  opcode or an unmodelled characteristic). **Named deferred expiries** (spec §13):
  a real modal-activated-ability gate card; `X` in an activated-ability cost
  (M3g's `AbilityCost.mana` + M4a's `ChooseX`, unwired — no `{X}`-cost ability
  exists); text-change (M3d) reaching an ability's effects (`resolveEffects` still
  doesn't rewrite); a non-self-excluding `NonlandPermanentTarget` (splits when a
  card wants plain "target nonland permanent" without "another"); `ModeSelection`
  beyond `ChooseExactly` on abilities (the M4g deferrals — choose-two/escalate CR
  700.2h/pawprint CR 700.2i/same-mode-twice CR 700.2d/another-player-chooses CR
  700.2e — apply equally here); multi-mode slot-name collision (unreachable until
  a selection picks ≥2 modes); and modal triggers beyond `SelfEnters` (rides
  `placeOne` unchanged when a non-ETB modal trigger condition lands). Spec and
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
  undisturbed. **Named deferred expiries** (spec §7): Auras / indefinite
  control (Control Magic) — the whole Attach subsystem, retiring the sickness
  synthetic and adding the cross-turn settle path; instant-speed / mid-combat
  control change (Ray of Command) — combat-membership edits when control moves
  mid-step; **CR 613.8 control dependency** beyond timestamp last-wins
  (multiple simultaneous control effects that depend on one another) — tracked
  as open git-bug `f90e0c4`; multiplayer leaves-the-game reversion (CR 800.4);
  mass/conditional untap (`Untap` stays single-target until a card needs
  more); and control-at-base (a permanent entering under a non-owner's
  control). Tracking: git-bug `83f1a55` (which also carries the still-open
  GAP-L1/copy facet for P2) is annotated that its GAP-L2 facet is addressed by
  this phase; `f90e0c4` stays open. Spec and plan kept as reference:
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
  the copy target is a genuine prompt, never elided. **Named deferred expiries** (spec
  §7): copying a permanent's **static abilities** (a Clone of Humility/Opalescence —
  `gather` reads static abilities from the printed card, not copiable values; the
  activated/triggered/replacement abilities ARE copied); **name/mana cost/color/
  supertypes** (not projected — color rides P3); **7b/CDA P/T-setting** in copiable
  values (rides P3, Tarmogoyf); **ongoing "becomes a copy"** (Vesuvan Doppelganger,
  re-reads the source); **copy-spell** (CR 707.10) and **copy-token** effects;
  **simultaneous entry** of multiple copy-choosers (CR 614.12b/616); the **general
  monadic as-enters replacement engine** (CR 614.12/616 — the drain folds into it at
  **P5**); **face-down** (backlog). git-bug `83f1a55` (GAP-L1) is closed by this
  phase (its GAP-L2 sibling was addressed by P1); `f90e0c4` stays open (P2 adds no
  within-layer dependency — a copy's affected set is always the object itself). Spec
  and plan kept as reference:
  `docs/superpowers/specs/2026-07-20-p2-copy-layer-1-design.md` and
  `docs/superpowers/plans/2026-07-20-p2-copy-layer-1.md`.
