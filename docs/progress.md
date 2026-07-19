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
