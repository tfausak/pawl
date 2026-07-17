# pawl — lessons from prior art

*Synthesis of a code-level study of three engines against the bets in `design.md`.*
*Studied: **Argentum** (Kotlin, the sibling — MIT), **Forge** (Java, 33k-card DSL — GPLv3), **XMage** (Java, class-per-card — varies).*
*Method: one agent per repo, each interrogating the actual source (not just docs) against pawl's specific architectural decisions. Every claim below traces to `path:line` in the study transcripts.*

*§8 adds a **second wave**: a survey of the closed-source engines (Arena, MTGO, Duels, Manalink) and the wider open-source field. Method there is weaker — public writeups plus, where possible, source verification. Sourcing is graded inline: **[source]** = verified against code/text read directly, **[public]** = a dev statement on the page, **[weak]** = forum/wiki only.*

*§10 adds a **third wave** (2026-07-17): code-level studies of the two Haskell engines — **mtg-pure** and **MedeaMelana's Magic**, both BSD-3 — plus deeper reads of Magarena's layer/AI internals and ygopro-core's suspension protocol. Method matches the first wave: agents interrogating source, claims traced to `path:line`.*

---

## 0. The seven findings that change what you do

*(1–5 from the first wave — Argentum/Forge/XMage at code level. 6 from the second wave — §8. 7 from the third wave — the Haskell engines, §10.)*

1. **Trial-application layer resolution is the bet the best prior art gave up on.** Argentum *documented* trial application (`docs/continuous-effect-dependency-system.md`) in loving detail and then shipped a hardcoded whitelist (`EffectSorter.dependsOn` pattern-matches ~2 `Modification` subtypes; else → timestamp). It couldn't even *represent* "Blood Moon removes Urborg's ability." Its `CLAUDE.md` still claims trial application it doesn't do. **This is your M3 go/no-go.** Budget real engineering, or scope it down deliberately — do not assume "we'll just do trial application." (§Decisions, §M3)

2. **You may not need randomness as a suspension.** Argentum hits *every* goal you cite for suspensions (replay, seeded repro, MCTS determinization, WASM) with a **seeded RNG threaded in state** (`GameRng`, SplitMix64) — the seed is recorded once as "the one sanctioned non-determinism boundary," then the engine is pure. That's strictly *simpler* than modeling shuffle/flip/die as prompts and gets bit-exact replay anyway. Your §2.2 deserves an explicit debate: **player choices as suspensions, chance as a threaded seed.** (§Decisions)

3. **The free monad is validated by counting what it eliminates.** Argentum reached the suspension *goals* without a free monad — via a hand-written defunctionalized continuation stack: **~145 `ContinuationFrame` subtypes + ~30 resumer modules**, stored as serializable game data. That boilerplate *is* what your `Free`/coroutine buys for free. Quantified argument for your §2.1. But steal the property it preserves: **the suspension must serialize** (persist across process restart, resume on another host). (§Steal)

4. **Two design-doc claims about XMage are wrong; fix them before they mislead you.** (a) **Mutate is fully implemented** in XMage (36 cards, `PermanentImpl.mutate():737`, list-based merged permanent) — issue #6390 was resolved; remove it from your impossibility list. Ironically XMage solved it with the *exact* list-based arity your §2.11 preaches. (b) **104.4b loop detection is not deep-copy** — it's a heuristic counter + "draw game?" prompt (`GameImpl.java:1904`). Your AI-search deep-copy claim is correct; the loop-detection one is overstated. (§Corrections)

5. **The opcode build order is now empirical, not guessed.** Forge's 33k cards yield **190 distinct opcodes; the top ~20 cover 77%, top ~50 cover 92%** — *steeper* Zipfian than your "150–200 for 80%." `ChangeZone, Pump, Draw, Token, PutCounter, DealDamage, Mana, GainLife, Destroy, Tap` are the first ten. This is your M4 sequencing, derived from data. (§M4)

6. **~80% is proven; the last 20% is the whole bet — and nobody has held it but Argentum.** Measured across corpora (§8.8): Magarena is **84.3%** declarative (2,057/13,071 need `requires_groovy_code`, *plus* 5,293 cards never attempted at all), Arena's parser gets **~80%** automatically, Forge ~95%. Three unrelated systems converged on ~80/20 — so "a first-order DSL expresses most cards" is **settled, not a risk**. The risk is elsewhere, and it's sharper than "everyone leaks": the leak ranges **0% → 94%**, and that range *is* the architecture. ygopro looks like your design (15 `EFFECT_TYPE_*`, 75 `EVENT_*`) but **94.4% of its 13,415 cards call `SetOperation`** — an arbitrary Lua closure, even for "take control of 1 monster." Its open half is not data. **Only Argentum is 100% declarative, and only at ~1/10th Forge's corpus.** State the bet honestly: *can pawl hold the last 20% with no hatch, at a scale where the sole precedent is untested?* Decide the answer **now**, before card #400 forces it — every other project answered under duress, and answered the same way. (§8.8)

7. **The two Haskell engines bracket pawl's design, and each stalled one decision short of it.** mtg-pure took the opposite fork on *both* central bets — cards as a type-indexed EDSL in modules (5,199 LOC of object-union plumbing plus a mandatory codegen), choices as IO callbacks (so no snapshot/resume: subgames absent, Mindslaver a literal `-- TODO`) — and died in six months at exactly the layers/replacement/subgames boundary. MedeaMelana's Magic has pawl's free-monad interaction and full `Layer1..7e` as data, but its card effects are monadic closures — unserializable, unanalyzable. Together they isolate pawl's delta to precisely two choices: **suspensions, not callbacks; data leaves, not closures.** Both are BSD-3 — portable. (§10)

---

## 1. Corrections to `design.md`

| Doc claim | Reality (evidence) | Fix |
|---|---|---|
| XMage can't do Mutate (§5, cites #6390) | Fully implemented — 36 cards, `MutateAbility`, `PermanentImpl.mutate():737-804`, list-based merged permanent | **Delete the claim.** Cite instead as *proof list-based arity works* (§2.11). |
| 104.4b via deep-copy hash/compare; "Forge and XMage must deep-copy to even attempt it" (§2.5) | XMage uses a heuristic loop counter + player prompt (`GameImpl.java:1904-1930`), not state copy | Keep the *AI-search* deep-copy point (that's real: `createSimulationForAI()→copy()`); **soften the 104.4b line.** |
| XMage can't nest a game → restart (726) impossible (§M5 lumps 726/728) | *Subgames (728, Shahrazad) confirmed impossible* — `Game` is a mutated singleton, no Shahrazad class. But *restart (726, Karn)* works by mutating the singleton in place (`KarnLiberated.java:95-132`, code comment: "dirty hack… can cause bugs") | **Scope the nesting claim to true subgames (728),** not all of 726–728. |
| Layer 3 "very unlikely" in XMage because "nothing to rewrite" (§5) | Subtler: XMage *can* rewrite structured subtype/color enums via layer 3 (`NewBlood.java:147`). What it can't rewrite is words baked into **compiled ability logic** | Restate precisely: the blocker is *compiled abilities*, not the absence of a mutation mechanism. Sharpens your M3 Magical Hack test rationale. |
| "Argentum: same base/projected split, same trial-application dependency resolution" (§8) | Base/projected split **confirmed**. Trial application **doc-only, never built** | **Correct the reference:** Argentum validates base/projected; it is a *cautionary* datapoint on trial application, not a positive one. |

---

## 2. Bets that are now validated (proceed with confidence)

| Bet | Verdict | Key evidence + what to steal |
|---|---|---|
| **§2.5 Immutable base vs. projected** | ✅ Confirmed (Argentum) | Separate `StateProjector.project()`; "wear off" = delete effect from `floatingEffects` list + recompute, no rollback anywhere. Projection **memoized per immutable instance** (`lazy`) — you get this free from Haskell laziness. Nuance: "for as long as" durations need a **two-phase model** — reversible per-frame gate *plus* a permanent latch SBA (611.2b) so a later projection can't resurrect the effect. |
| **§2.7 Cards as runtime data (deep AST)** | ✅ Confirmed *stronger than the doc* (Argentum + Forge) | Argentum: `CardDefinition` is `@Serializable`; **no escape hatch exists** — zero lambda/`CustomLogicRegistry` hits across the whole card corpus even though its own docs proposed them. Tarmogoyf-style P/T is `DynamicAmount.Count` (first-order data arithmetic). Forge: 33k cards, closed 201-entry `ApiType` enum, bounded iteration only → first-order thesis holds at full corpus scale. **Resist the doc-temptation to add code escape hatches; a first-order `DynamicAmount` covers CDAs.** |
| **§1 ABI invariant (no `case effect of` in core)** | ✅ Confirmed, with one instructive leak (Argentum) | Dispatch is an open registry (`KClass→Executor` map), *not* a `when`; a build-time test fails CI if any effect lacks an executor. **The one leak:** the mana subsystem has 26 + 11 `is AddMana*Effect ->` branches because `Effect` exposes no `manaProduced()` classification. **Lesson (mechanical): for every question the core must answer about an effect *before executing it* — produces mana? and what? redirects a zone change? — add an explicit ABI classification, or the core will grow an `is X` switch.** Audit the ABI for "what must the core know pre-execution." |
| **§2.3 Decider ≠ player (722)** | ✅ Confirmed (Argentum) | `actorFor(playerId)` returns *input authority* (the hijacker during Mindslaver) while *resource ownership stays with the player*; continuations carry a distinct `deciderId`. Clean seam — copy it. |
| **§2.4 Multiplayer/APNAP from day one** | ✅ Confirmed (Argentum) | `turnOrder: List`, `teams: List<List>`, APNAP pervasive, 2-player is only an `isHeadsUp` fast-path. Cheap to do up front, exactly as the doc says. |
| **§2.11 No fixed arity** | ✅ Confirmed from *both* sides | Argentum does it right (`cardFaces: List`, modes `List<Mode>`). XMage proves the cost of *not*: `SplitCard.leftHalfCard/rightHalfCard` (two hardcoded fields) is precisely why 5-part "Who//What//When//Where//Why" is unplayable. **Caveat:** even Argentum accreted *two* arity mechanisms (`backFace: CardDefinition?` for transforming DFCs vs. the `cardFaces` list) — commit to lists-everywhere from commit one to avoid the same drift. |
| **§2.12 Numeric tower (not `Int`)** | ✅ Confirmed necessary | Argentum needed `GameLimits` (saturating arithmetic, per-effect token cap, resolution-depth guard) because real combos overflow `Int` / exhaust memory. Your tower plus these backstops. |

---

## 3. Three open design decisions the evidence forces

These aren't in the doc as decisions; the code says they should be.

**D1 — Is randomness a suspension, or a threaded seed?** (challenges §2.2)
Argentum's seeded-RNG-in-state gives replay + MCTS determinization + WASM parity at lower cost than prompt-per-shuffle. *For* keeping it a suspension: uniformity (one interpreter mechanism), and determinization-for-MCTS reads more naturally as "sample a consistent hidden state" when chance is a request. *Against:* ~one extra prompt constructor per RNG use and interpreter plumbing for zero replay benefit over a recorded seed. **Recommendation: player choices = suspensions; chance = threaded seed recorded in the `DecisionLog`.** You lose nothing on replay/determinization and simplify the hot path. Revisit only if a card needs to *react to the RNG mid-request* in a way a seed can't express (none known).

**D2 — How much do you actually spend on trial application?** (the M3 bet)
The strongest sibling punted. Two honest options: (a) **Full 613.8**: solve what Argentum didn't — re-project effect A under B *cheaply* and compare A's affected set; your immutable+memoized state makes re-projection cheaper than in a mutable engine, which is your genuine edge here. (b) **Scoped**: ship the topological sort + timestamp (Argentum's real behavior) and a *small explicit* dependency table, but — unlike Argentum — **make the incompleteness a tracked metric with failing tests**, not a doc lie. **Recommendation: attempt (a), because it's your differentiator and cards-as-data makes re-projection tractable; but build the test set first (below) so you know the day it degrades into (b).**

**D3 — Is copy a projector layer, or an entry-time replacement?** (refines §2.6)
Argentum resolves copy (Layer 1) at object-entry time as a replacement effect: the copied component *becomes base state*, so the projector **skips Layer 1 and starts at Layer 2**. This is simpler *and* more correct for 707.2 copiable-values than running copy inside the 613 loop. **Recommendation: carve copy out of the layer loop; resolve it into base state at entry.**

**D4 — How does a first-order effect reference prior choices and payments?** (mtg-pure's wall; shapes every opcode)
"If {B} was spent to cast this…", "for each…", a mode chosen earlier, X. mtg-pure's author agonized over exactly this (`Recursive.hs:578-584`) — a first-order model can't close over a payment — and punted; their partial answer is `PlayerPays … (Variable FinPayment -> Elect …)`: a payment introduces a *bound variable* later steps read. Argentum's `EffectContext` named collections (§4-M4) are the same move. **Recommendation: named binding slots in the DSL — payments, chosen modes, selected sets, and X introduce names; later fields reference them; a static linter checks every read has a writer (Argentum's AST-dataflow lint, §5).** M1a's mana-unit provenance is the closed-half half of the answer; D4 is the open-half half. Decide at M3 with the first dozen opcodes — it changes their field shapes.

---

## 4. Milestone-keyed lessons

**M0 (complete game, zero cards).**
- Steal Argentum's **"atom" pattern**: funnel every observable mutation through one helper that performs the change *and* emits its event *and* owns the no-op→no-event guard (603.2f). Motivated by real lost-tap-event bugs. Co-locate state-change + event emission structurally from the start.
- Trigger detection is **three passes**, not one: battlefield / phase-step / leaves-battlefield — because LTB/dies triggers reference objects no longer on the battlefield. Design the trigger scan for this now.
- Profile the goldfish loop for thunk leaks (your risk register) — Argentum confirms memoized-per-state projection is the intended contract; make the projected field lazy + shared, everything else strict.

**M1–M2 (vanilla → French vanilla, zero opcodes).**
- XMage's `GrizzlyBears.java` is 33 lines of Java for a 2/2 — the visceral case for cards-as-data. Nothing to copy; everything to avoid.
- Argentum's **build-time hygiene tests** belong here: a test that fails CI if any effect/keyword lacks a handler; source-scanning bans on raw mutation. Invariants enforced, not discouraged.

**M3 (the ABI test — go/no-go).** This is where prior art is most valuable:
- **Magical Hack (Layer 3):** neither Argentum nor XMage does real text-word rewriting. Argentum's layer 3 is only `SetName`; word-changes are shunted to a *side subsystem outside layer order* — a known incompleteness. XMage can rewrite subtype enums but not words in compiled abilities. **You are genuinely ahead here if your AST is rewritable — but you have no reference implementation to copy. Prove it early.**
- **Humility + Opalescence / Blood Moon + Urborg:** port Argentum's `ClassicLayerScenariosTest.kt` and `LayerSystemTest.kt` as your target list — but note its Blood Moon test *asserts a simplified wrong outcome* and comments that true 613.8 is unimplemented. Use these as tests to **pass**, not behavior to copy.
- **Mindslaver:** copy Argentum's `actorFor`/`deciderId` seam directly (`HijackInputRoutingTest.kt` states the invariant exactly: "the affected player remains controller and resource owner; the controlling player is just the input device").

**M4 (the vocabulary).**
- **Build opcodes in Forge's empirical frequency order** (table below). Two non-obvious early needs Forge surfaces: `Effect` (#9 — creates a continuous side-effect object) and `Cleanup` (#6 — clears remembered objects after flicker/copy). They aren't "spells" but you need them early.
- Steal Argentum's **atomic effect pipelines** (`Gather → Select → Move` with named collections in an `EffectContext`): hundreds of library/zone cards become data compositions with zero new executors. And **late-binding symbolic targets** (`ContextTarget(i)` / `StoredEntityTarget(name)` vs. a resolved `ChosenTarget`) — makes 608.2b re-validation and Oblivion Ring fall out without closures.

**M5 (the nightmares as rules sections).**
- **728 subgames (Shahrazad):** confirmed genuinely impossible in XMage (mutated singleton `Game`). Your nested `Program Prompt` is the real advantage — this is where the architecture pays off.
- **726 restart (Karn):** XMage does it by in-place mutation with a self-admitted "dirty hack." You can do it cleanly by discarding to a fresh state. Keep 726 and 728 *separate* milestones.
- **`tests/nightmares/`:** you can't transcribe XMage's list from the repo (it's wiki-only). Derive it from XMage's *observed absences* — the text-word-change cards, the >2-face cards, the subgame cards — plus its `VerifyCardDataTest` skip-lists (Un-set joke cards whose printed data breaks naive validation — a *distinct* category worth its own file).

**M6 (the transpiler).**
- Forge cardsfolder is **GPLv3** — see §6. Use it as a **dev-time reference oracle only**; do not ship transpiled output unless pawl goes GPLv3.
- Argentum is **MIT** — you *can* port its card data and subsystems with attribution. Its `mtgish-tooling` predicts which unimplemented cards are pure-authoring vs. need new engine features — build the same, your first-order AST makes it possible.

**M7 (interpreters).**
- Argentum quantifies your MCTS edge: it **deep-copies the whole `GameState` per AI ply** (`createSimulationForAI()→copy()`, ~24 sub-structures cloned). Your held continuation + immutable state resumes twice for free. Real, measurable advantage.

---

## 5. The steal list (concrete, with source)

All from Argentum (MIT — attribute; port freely):
- **Seeded RNG in state, seed recorded once** (`GameRng`, `GameState.rng`) — bit-exact replay/MCTS/parity. (See D1.)
- **Build-time hygiene tests:** effect-executor coverage (every leaf has a handler), **card JSON snapshot goldens** (146 files — SDK changes show as per-card reviewable diffs), AST-dataflow linter (every pipeline-variable read has a writer), source-scan bans on raw mutation/raw effect construction. *Make invariants impossible to violate, not merely discouraged.*
- **The "atom" mutation helper** (mutation + event + no-op guard + replacement, in one place).
- **LKI as one frozen `EntitySnapshot`** behind a shared view interface, with `lkiPolicyFor(ref)` an exhaustive match so a new reference variant is a *compile error* until its LKI behavior is classified (603.10 / 608.2h).
- **`actorFor` / `deciderId` seam** for rule 722.
- **Atomic effect pipelines** + **late-binding symbolic targets** (M4 above).
- **Copy-as-entry-time-replacement** (D3) — projector skips Layer 1.
- **Two-phase durations** (reversible gate + latch SBA).
- **`GameLimits`** saturating arithmetic + token/depth caps.

From **MedeaMelana's Magic** (BSD-3 — attribute; port freely):
- **Effect ≡ event**: `SimpleOneShotEffect` is shaped so the effect value *is* the event value ("simple if its fields contain enough information to serve as an Event unchanged, using the `Did` constructor"), and `ExecuteEffects :: [OneShotEffect] -> ExecuteEffects [Event]` is the batch propose → replacement-rewrite → apply → emit-for-triggers pipeline. One vocabulary wires 614 and 603. (§10.2)
- The `Interact`/`Question` vocabulary (`AskKeepHand`, `AskPriorityAction`, `AskManaAbility`, `AskTarget`, `AskAttackers`, `AskSearch`, `AskChoice`) — a field-tested checklist for pawl's eventual `Prompt` constructors.

From **mtg-pure** (BSD-3 — attribute; port freely):
- **The `ElectStage` tag** (`IntrinsicStage / TargetStage / ResolveStage`): *when* a choice locks in — printed characteristic vs. cast-time target vs. resolution-time choice. Port as a runtime validation tag on DSL choice nodes (drop the singletons); it rejects a whole class of ill-formed cards, e.g. a target chosen at resolution. (§10.1)

Property to preserve from Argentum's continuation stack even though you use a free monad: **the suspension must serialize** (persist/resume across hosts).

Structural warning from XMage: keep the *AI-search-needs-deep-copy* observation (`GameImpl.copy()` clones ~24 sub-structures per ply) as motivation for your continuation approach.

---

## 6. Empirical opcode build order (Forge, 33,300 cards)

57,659 total usages · **190 distinct opcodes** · **top 20 = 77% · top 50 = 92%.** Build in this order for M4.

| # | Opcode | Uses | # | Opcode | Uses | # | Opcode | Uses |
|--|--|--|--|--|--|--|--|--|
| 1 | ChangeZone | 6544 | 21 | Mill | 577 | 41 | RemoveCounter | 199 |
| 2 | Pump | 4937 | 22 | Counter | 529 | 42 | MakeCard | 192 |
| 3 | Draw | 3664 | 23 | Untap | 473 | 43 | ChooseType | 179 |
| 4 | Token | 3506 | 24 | DelayedTrigger | 472 | 44 | Clone | 178 |
| 5 | PutCounter | 3222 | 25 | Scry | 451 | 45 | DigUntil | 168 |
| 6 | Cleanup | 2943 | 26 | ChooseCard | 445 | 46 | GenericChoice | 166 |
| 7 | DealDamage | 2829 | 27 | DamageAll | 424 | 47 | PeekAndReveal | 165 |
| 8 | Mana | 2474 | 28 | DestroyAll | 363 | 48 | ReplaceEffect | 160 |
| 9 | Effect | 1887 | 29 | CopyPermanent | 353 | 49 | Fight | 148 |
| 10 | GainLife | 1751 | 30 | RepeatEach | 351 | 50 | SacrificeAll | 146 |
| 11 | Destroy | 1532 | 31 | SetState | 347 | 51 | Investigate | 145 |
| 12 | Tap | 1459 | 32 | GainControl | 325 | 52 | StoreSVar | 141 |
| 13 | LoseLife | 1198 | 33 | Play | 321 | 53 | RollDice | 139 |
| 14 | Discard | 1085 | 34 | ImmediateTrigger | 320 | 54 | AnimateAll | 139 |
| 15 | PumpAll | 1030 | 35 | PutCounterAll | 292 | 55 | ChooseColor | 134 |
| 16 | Animate | 1014 | 36 | Regenerate | 272 | 56 | ChoosePlayer | 131 |
| 17 | Dig | 972 | 37 | CopySpellAbility | 250 | 57 | UntapAll | 124 |
| 18 | Sacrifice | 886 | 38 | Attach | 237 | 58 | PreventDamage | 122 |
| 19 | Charm | 773 | 39 | Surveil | 214 | 59 | Seek | 119 |
| 20 | ChangeZoneAll | 661 | 40 | RemoveCounter | 199 | 60 | Branch | 116 |

Tail (rank 61→190) = 130 opcodes sharing <8% of usage — the long cheap tail (`Meld`, `Amass`, `Manifest`, `Bolster`, `Vote`, one-off mechanics). Note the value sublanguage is *also* closed: Forge's `Count$` has 185 heads resolved by one switch — enumerated queries over state, not arbitrary arithmetic. Mirror that: a closed `DynamicAmount`, per Argentum.

---

## 7. Licenses

| Engine | License | What pawl may do |
|---|---|---|
| **Argentum** | **MIT** | Study, port, derive **code and card data freely**, *with* the MIT copyright/permission notice for substantial portions. Carry the same "unaffiliated fan work / Wizards trademark" disclaimer (constrains branding, not code). **Your most reusable source.** |
| **Forge** | **GPLv3** (repo + cardsfolder) | Study the DSL and use the frequency table (facts, not derivative). **Do not** bundle/transpile-and-ship cardsfolder data or copy `*Effect.java` unless pawl goes GPLv3. Keep it a **read-only dev-time oracle**; generate pawl's cards independently (e.g. from Scryfall/MTGJSON oracle text). |
| **XMage** | (per-repo; verify before reuse) | Studied here for architecture lessons only — nothing to port. |
| **mtg-pure** | **BSD-3** | Permissive. Study and port freely with the notice. **The other permissive MTG-specific source besides Argentum.** |
| **MedeaMelana's Magic** | **BSD-3** | Permissive — the third portable source (© 2012–2016 Martijn van Steenbergen). Free-monad `Interact` + layers-as-data are directly relevant; effects are closures (§10.2). |
| **jinteki.net** | **MIT** | Permissive — safe to read freely, unlike Forge. Not MTG, so lessons are structural only. |
| **ygopro-core** | **AGPL-3** | **Reference only** — same posture as Forge, and AGPL is stricter (network use triggers it). Read for the core/data seam; do not copy structure. |
| **Magarena** | **GPL-3** | **Reference only.** The `requires_groovy_code` ratio (§8.8) is a fact, not a derivative work — safe to cite. |
| **SabberStone / Fireplace** | **AGPL** | **Reference only.** Read the task/effect *vocabulary*; do not port. |
| **Shandalar / Manalink** | (unclear; derived from a 1997 proprietary binary) | **Do not port anything.** Cite as an architectural specimen only. Original MicroProse source is lost. |

*(Card names/text are Wizards IP regardless of engine license — orthogonal, relevant before redistributing any card data.)*

**Posture:** Argentum (MIT), mtg-pure (BSD-3), and MedeaMelana's Magic (BSD-3) are the only sources pawl may *derive from*. Everything else in §8 is a read-only oracle.

---

## 8. The wider survey — closed engines and the rest of the field

*Second study wave. Unlike §§1–7, most of this rests on public writeups rather than code. Claims are graded: **[source]** = verified against primary code/text I read; **[public]** = a dev statement I read on the page; **[weak]** = forum/wiki/search-snippet only, verify before relying.*

### 8.1 The invariant has two industry witnesses — but nobody enforces it by construction

Both shipping engines that scaled converged on §1's rule, and both stated the failure mode in almost pawl's words.

**Alex Werner (WotC, Arena rules team), ["On Whiteboards, Naps, and Living Breakthrough"](https://magic.wizards.com/en/news/mtg-arena/on-whiteboards-naps-and-living-breakthrough), 2023-07-31** — this is the `case effect of DealDamage{} -> …` anti-pattern, named **[public]**:

> "Imagine a function determining whether a player can play a particular land. So that one function would have to have code for Fastbond. And code for Explore. And code for Solfatara. And code for Crucible of Worlds. … It would be a nightmare."

> "The GRE has no idea that either Yawgmoth's Will or Meddling Mage exists, and neither one of them knows anything about the other. There's no special case code to make sure Meddling Mage and Yawgmoth's Will work together."

**Patrick Buckland (Stainless CEO, wrote the Duels engine), ["Duels of the Planeswalkers: The Magic Engine"](https://web.archive.org/web/20090619234532/http://www.wizards.com/Magic/Magazine/Article.aspx?x=mtg/daily/feature/43c), 2009-06-17 [public]**:

> "it came back to the problem of any card overriding the rules. The user-interface side of the game cannot possibly know what cards are doing what—only the engine can know this—otherwise many cards would be impossible to implement."

**The delta, and it is pawl's actual novel claim: both enforce the invariant by *discipline*, not *construction*.** Arena's GRE is C++ + CLIPS (a LISP production system); nothing structurally prevents a CLIPS rule from pattern-matching card identity. The tell is Werner's own language in the [Zurgo dev diary](https://magic.wizards.com/en/news/mtg-arena/dev-diary-zurgo-thunders-decree) (2025-04-08): "I was *pretty confident*… the word 'should' is carrying a lot of weight here", and then "**I was frankly shocked**" when the architecture held. He *hoped*. A type system can make the violation a compile error. **State this as a delta, not as parity — it is the one architectural claim available to pawl that is unavailable to Arena.** Note §2.7 already found Argentum has *zero* escape hatches; Argentum and pawl are the outliers in this whole field, not the norm.

### 8.2 Manalink / Shandalar (1997) — fusion, verified from source

Cloned to `_scratch/shandalar` ([ShandalarMagic/Shandalar](https://github.com/ShandalarMagic/Shandalar)). **The "card-effect interpreter" hypothesis is false — there is one native function per card, addressed into the rules binary [source].** From `src/manalink.lds`:

```
_dispatch_event        = 0x4359b0;
_card_magical_hack_exe = 0x4A45B0;
_card_shock            = 0x4134B0;
```

84 `_card_*` symbols in the linker script; 116 card `.c` files; `extern card_data_t cards_data[]` pinned at `0x7E7010` (`src/manalink.h:69`). **Magical Hack — pawl's first ABI test — was a hand-written function at a fixed address.** This is the purest specimen of the failure mode §1 guards against, and it is worth keeping as the canonical example.

`magic_updater/xml-to-csv.pl:2039` carries CSV columns **named after individual cards** — `Hack Color`, `Hack Mode`, `Sleighted Color` **[source]**. Text-changing wasn't expressible, so two specific cards were special-cased *into the data schema*. (Precisely: the updater no longer populates them — "Shandalar computes it itself" — so cite this as "the schema has per-card columns," not "the engine special-cases them at runtime.")

**One find that supports the ABI design:** `cards_data[iid]->extra_ability & EA_MANA_SOURCE` (`src/patches/patch_make_manasource_interrupts_interruptible.pl:49`) **[source]**. Even the maximally-fused engine needed *is-this-a-mana-ability* as a bit the core reads — the same classification Duels exposes as `COMPARTMENT_ID_TRIGGER_ABILITY_IS_MANA_ABILITY` **[weak]**, and the same one §1's mana leak says pawl must add explicitly. Three independent engines converged on that exact classification axis. Good evidence it isn't arbitrary.

**Unverified [weak], do not cite until checked** (slightlymagic.net 403s fetchers; `curl` with a browser UA reportedly works): the "`cards_data[]` walled Manalink at 2,000 cards" quote (`cards_coded[]` does not appear in the repo at all); the jatill "10 hours in ASM vs. 10 minutes in C" cost-per-card claim and the ~60× swing; the card-count timeline (465 → 2,345 → ~10,639); the Korath/Chromatic Lantern and Gargaroz/Counterbalance "engine limitation" quotes. **The architecture claim is verified; the economics claims around it are not.** The doc leans on the former.

### 8.3 Duels of the Planeswalkers — the closest analogue, and the sharpest warning

C++ core; cards as **Lua embedded in XML**, loaded from `.wad` at runtime, no recompile **[public]**. Buckland ran pawl's M0 bet in 2009: shipped without phasing, hybrid mana, type-changing, copying — "we structured the underlying engine in such a way that **none of these features are precluded from its core structure**. In fact some of them (such as the phased-out and removed-from-game zones) are implemented but not used." The stack was real from day one, merely hidden in the UI.

**Card count is set by architecture, not effort — the cleanest natural experiment in the field.** Stainless shipped ~151 (DotP 2014) to ~300 (Magic 2015); **the modding community reached ~19,000 cards on the same engine** via `DATA_DLC_DECK_BUILDER_CUSTOM` **[weak on the number, but the mechanism is documented]**. Ken Troop (WotC R&D) confirms the shipped cap was content/QA budget, not engine: "those cards have to be coded, their interactions tested" **[public]**.

**The cautionary half — this is the argument for static analyzability, and it's better than any argument in §2.7.** DotP's DSL was data-driven but *not* statically analyzable: Lua could define globals at runtime, and card state lived in "Data Chests" of **globally numbered registers** (`INT_REGISTER_0..3`) with no namespacing. The community's fix was a **human-maintained [Prefix/Id Registry](https://www.slightlymagic.net/wiki/DotP_2014:_Prefix/Id_Registry)** — "if different modders make a card with the same filename, or use a constant with the same name, or use a public chest with the same register, those mods will interfere with one another" **[weak]**. **Data-driven is necessary but insufficient. That registry is the concrete price of not having static analyzability, and it is exactly what pawl's first-order, non-recursive DSL buys back.**

### 8.4 Arena's GRE — three mechanisms worth stealing

- **"Agendas"** — CLIPS rules are partitioned into categories of behavior (abilities triggering, replacement effects, state-based actions), and "the order in which agendas happen is a carefully choreographed ballet that underlies basically everything" **[public]**. A battle-tested classification axis for the M1a priority loop. Their final Zurgo bug was an agenda *ordering* error (game actions ran before ability-granting), so this is load-bearing risk, not a detail.
- **"Qualifications"** — "can't be sacrificed" is not the ability checking things; the ability *creates a classified object*, explicitly so that "this creature can't be sacrificed" and "this turn, creatures you control can't be sacrificed" share one rule **[public]**. One rule covered ~7 unrelated sacrifice cases. This is the closed/open boundary in miniature.
- **The to-do list is a query, not just a plan**: "the engine can write that 'to-do list' on a whiteboard, wait to see if any of the elements on the list get crossed out, and then **not actually DO any of the things on the list. The initial list is there mostly to gather information**" **[public]**. Relevant to M1a resolution modeling.

**Layers are their acknowledged perf sink** — "any time they change, we have to fully rebuild them from scratch… It's one of the main things that the engine spends time doing. So, we want to avoid doing it whenever possible" **[public]**. They refuse to recalc on step/phase boundaries, and *that is what broke Zurgo* (their first card granting an ability only during a specific step). **pawl's memoized-per-immutable-state projection (§2.5) may make full rebuild cheap enough to always do — making correct-by-default what they hand-optimized around.** Worth noting before M3.

Two more: the **GRP** (Game Rules Parser, Python) compiles English card text → CLIPS rules *offline*, and gets "**80% or so** of newly written Magic cards to just work… It's also what we have to update, modify, and improve to get the other 20% to work" **[public]** — the only per-card figure from a shipping engine, and it corroborates §M6. And they keep **~5,000 regression tests**, "fully scripted games of Magic… A text file controls both players," plus **ASUP**, a debug-only card "set" of purpose-built cards to reach awkward states cheaply **[public]**. That second idea is cheap and pawl should copy it: *test-only cards are legitimate engine infrastructure.*

**Where their split leaks:** effect-classification independence protects the rules core but not the parser or the client — Living Breakthrough required coordinated GRP *and* client changes. Those become the new coupling points.

### 8.5 MTGO — keep its two failure stories apart

Conflating them credits the rewrite to the wrong cause, and the popular narrative gets this wrong.

- **The scaling story (why the rewrite hurt).** Announced Feb 2004, shipped **April 2008** — four years. The stated driver was users, not rules: "Leaping Lizard's 2.5 interface and backend are not scalable… It wasn't written with the goal of ten thousand users in mind" (hard cap ~4,400 players) **[weak]**.
- **The fusion story (the one that matters here).** Daniel Myers (WotC), 2003-08-08 **[weak — archived, read via snippet]**: "while normally a card set only requires coding the specific cards in the set, the Eighth Edition set actually required a change to the way Magic Online works. Those changes had to fit in with all older cards in the game, also. **Evidently, subtle tweaks to the Magic rules become major headaches within Magic Online.**" … "In short: We made some bad decisions." A *rules-level* change (land subtypes) leaked into *card-level* code across the entire back catalog.

**The encouraging half: the rules core survived every rewrite around it.** MTGO rewrote its client twice and its backend once over 20+ years and never rewrote the rules engine — which was the part contemporaries praised. **A correct closed core is a durable asset; clients and backends are not.** That is a direct argument for pawl's sequencing.

Cards are code, not data: "On the cardset team, we primarily use C++, C#, and Perl" (Matt Gregory, MTGO Card Set Development Lead, 2015) **[public]**, and a standing Cardset engineering team exists to this day. That is cost-per-card, paid as salary, forever. MTGO also still ships layer-dependency bugs of exactly the §M3 class (Spreading Seas on Urborg) **[weak]**.

### 8.6 The control group — no rules engine, cost-per-card zero

Apprentice (1995) "lacks a rules engine; the game moves forward by the players typing out their current actions," and consequently "the simple data format used to store cards has allowed new sets to be added and the registry of cards updated" — **by the community, for a decade after the company disbanded** **[weak]**. Magic Workstation "did not enforce any card game rules" and was deliberately *game-agnostic* — a universal engine for any CCG, which is structurally incompatible with a rules engine and is the strongest evidence the omission was a choice. Cockatrice inherits this; [issue #1679](https://github.com/Cockatrice/Cockatrice/issues/1679) proposed a Lisp card-effect DSL in 2015 and it remains unimplemented a decade later.

**This is the honest baseline for the whole thesis: delete the rules core and card count goes to infinity at zero marginal cost. Everything pawl's closed half costs is the price of enforcement.** The trade pawl is trying to escape — instant card scaling *and* enforced rules — is real, and no one has escaped it yet.

One orthogonal lesson worth keeping for whenever pawl grows a client: Apprentice's "Backwash" exploit "allowed undetectable cheating; for example, the ordering of each player's library," and Cockatrice's answer was to enforce **randomness and hidden information server-side while enforcing zero card behavior** **[weak]**. That is a *different axis* from rules enforcement, and it's the one where trust actually breaks. It also interacts with D1 (seed-in-state): a recorded seed is replayable, but a seed the client can see is a cheat.

### 8.7 The rest of the open-source field

Surveyed; nothing here displaces Argentum as the primary reference, but three are worth reading.

| Project | Lang / License | Why it matters |
|---|---|---|
| [**mtg-pure**](https://github.com/thomaseding/mtg-pure) | Haskell / BSD-3 | **The other Haskell attempt, and the one that took the opposite fork.** Stated goals mirror pawl's ("cards type-check iff they are valid cards", no special-casing, resilience to rules changes) — but its DSL is **recursive and compiled-in**: cards are Haskell values in a module, not runtime data. `MtgPure/Model/Recursive.hs` is the single highest-value file in this survey. **Now studied at code level (§10.1) — the reading is done, and the verdict is cautionary.** |
| [**Magic** (MedeaMelana)](https://github.com/MedeaMelana/Magic) | Haskell / BSD-3 | Free-monad `Interact` + full `Layer1..7e` as data — pawl's interaction model, already built in Haskell (2012–2019, M13 core set only); card effects are `Contextual (Magic ())` closures. Missed by the first two waves; studied at code level in §10.2. |
| [**ygopro-core / EDOPro**](https://github.com/edo9300/ygopro-core) | C++17 / AGPL-3 | Cleanest **core/data seam** in the field: the core takes three host callbacks (script reader, card reader, message handler) and nothing else. Scripts don't *do* things, they **register effects** classified by `EFFECT_TYPE_*` / `CATEGORY_*` / event codes — pawl's invariant, proven at ~13k cards. Then leaks: `SetOperation` takes an arbitrary Lua closure. |
| [**jinteki.net**](https://github.com/mtgred/netrunner) | Clojure / **MIT** | Netrunner, ~1,650 cards. `defcard` bodies are **plain data maps**; the engine dispatches on keys (`:cost`, `:choices`, `:events`). MIT, so unlike Forge it is safe to read freely. Then leaks: `:effect` bottoms out in arbitrary state-mutating Clojure. |
| [**Magarena**](https://github.com/magarena/magarena) | Java / GPL-3 | Dormant since 2023-04. Key-value card scripts with an **explicit `requires_groovy_code` flag** — the escape hatch, made countable. See §8.8. |
| [**SabberStone**](https://github.com/HearthSim/SabberStone) / [**Fireplace**](https://github.com/jleclanche/fireplace) | C# / Python, AGPL | Hearthstone. SabberStone's `SimpleTask`/`ComplexTask` algebra is a first-order effect vocabulary composed as data — read the namespace for *how few primitives cover how many cards*. Fireplace merges declarative action trees with Blizzard's own `CardDefs.xml` at init — the closed/open seam, at full coverage. |
| **phase-rs**, **manabrew**, **mtg-python-engine**, **mtghub-engine**, **Wagic**, **Incantus**, **corrosion** | various | Surveyed, nothing architecturally novel. `phase-rs` is a useful **control group**: code-per-card in the engine, LLM-assisted contribution pipeline, big card-count badge. Cockatrice is a client with no rules engine (Oracle XML DB only). |

**The universal pattern, and the decision it forces on pawl.** Every engine in this field that gets the *classification* half right then leaks at the *leaves*: ygopro's `SetOperation`, jinteki's `:effect`, Magarena's `requires_groovy_code`, Forge's `SVar`, DotP's runtime Lua globals. **Argentum (§2.7) is the sole exception — zero escape hatches, and its own docs proposed ones that were never built.** A first-order DSL with *no* escape hatch would be genuinely novel. The corollary is that pressure to add one is empirically universal, and **pawl should decide now what the answer is when card #400 doesn't fit** — because every other project answered it under duress and answered it the same way.

### 8.8 The escape-hatch ratio, measured — how data-driven is the field really?

Counted directly from the cloned corpora **[source]**. This reorders §8.7's conclusions, and one result inverts the received wisdom.

**Magarena** (`_scratch/magarena/release/Magarena/scripts`): **13,071** implemented card scripts, of which **2,057 (15.7%) carry `requires_groovy_code`**; 1,917 `.groovy` files. A sibling `scripts_missing/` holds **5,293** cards attempted and not implemented at all.

> **84.3% of implemented cards are pure declarative data. 15.7% need the escape hatch. A further 5,293 (29% of all 18,364 known cards) defeated the system entirely.**

That 84.3% is a strikingly close independent match to Arena's GRP "**80% or so** just work… the other 20% we hand-write" (§8.4). **Two unrelated systems, different games' worth of engineering, converged on ~80/20.** Treat that as the field's empirical prior for what a declarative card DSL covers — and note the honest denominator: Magarena's `scripts_missing/` shows the *real* residue is worse than 20%, because the hardest cards never got scripted at all. When pawl's transpiler (§M6) reports coverage, **count the unattempted pile too, or the number lies.**

**ygopro-core / EDOPro** — **this is not the precedent the survey claimed it was.** The classification vocabulary is real and small: **15** `EFFECT_TYPE_*`, **75** `EVENT_*`, **328** `EFFECT_*` codes total. But of **13,415** card scripts, **12,670 — 94.4% — call `SetOperation`**, i.e. hand the core an arbitrary Lua closure. (`SetCondition` 8,994; `SetValue` 6,385.) Even Change of Heart (`c4031928.lua`, 25 lines) — "take control of 1 monster," about as simple as a card gets — is a closure.

> **ygopro's `SetOperation` is not a leak. It is the primary mechanism.** The classification layer routes *when* an effect happens; *what it does* is essentially always code.

**Correct the §8.7 framing accordingly:** ygopro validates the *closed core's dispatch axis* (`EFFECT_TYPE`/`EVENT`, at ~13k cards — genuinely useful, and it corroborates §8.2's convergence on classification bits). It does **not** validate cards-as-data. Its open half is 94% arbitrary code.

**The field, ranked by how much of the open half is actually data:**

| Engine | Declarative | Escape hatch |
|---|---|---|
| **Argentum** | **100%** | none — and its own docs proposed hatches never built (§2.7) |
| **Forge** | ~95% | `SVar`, plus `*Effect.java` for the residue |
| **Magarena** | **84.3%** | `requires_groovy_code` (15.7%), + 5,293 never attempted |
| **Arena GRP** | ~80% | hand-written CLIPS for the other 20% |
| **ygopro / EDOPro** | **~5.6%** | `SetOperation` — 94.4% of cards |
| **MTGO / Manalink / phase-rs** | 0% | code *is* the card |

**What this settles.** The "everyone leaks" story is true but too flat — the leak ranges from 0% to 94%, and that range *is* the architecture. pawl's target sits above Forge, next to Argentum, in territory only Argentum occupies and only at ~1/10th Forge's corpus. **The open question is not whether a first-order DSL can express 80% — three independent systems say yes. It is whether pawl can hold the last 20% without a hatch, at a corpus size where Argentum has not yet been tested.** That is the honest statement of the bet, and it belongs in `design.md` next to the M4 vocabulary plan.

### 8.9 Complexity results worth citing

- **Chatterjee & Ibsen-Jensen, ECAI 2016** ([record](https://research-explorer.ista.ac.at/record/478)) — deciding the legality of a **single step** of Magic is **coNP-complete**, and in P if either of two small card sets is excluded; the bound holds even single-player. **More operationally relevant to an engine than the Turing result, and under-cited by implementers.** The useful corollary: the hardness is concentrated in a small, identifiable card set — which is an argument for pawl's `tests/nightmares/` being a *finite, enumerable* target.
- **Churchill/Biderman/Herrick** ([arXiv 1904.09828](https://arxiv.org/abs/1904.09828), FUN 2021) — no engine artifact ships with it. Its constraint on pawl: the closed half cannot have a terminating evaluator in general, so the priority/stack loop must be **non-terminating-by-design** (mandatory loops, no "resolve to fixpoint" shortcut). Its own concession is apt: "it is unclear how to prove this beyond exhaustive analysis of the over 20,000 cards in the game."

### 8.10 Corrections and confabulations caught in this wave

Recorded because §9's meta-lesson applies to *research about* engines as much as to engines' own docs.

- **The XMage wiki's not-implemented list is stale, and contradicts §1.** Agents citing it reported "~40 Mutate cards under 'game engine limitation'". §1 already disproved that **from code** (`PermanentImpl.mutate():737`, 36 cards, #6390 resolved). **Trust the code correction; do not re-import the wiki claim.** Magical Hack / text-changing *does* still appear on that list, and that corroboration is genuine — but it is now the only part of it this doc relies on.
- **"MTGO cards are Perl scripts" — do not cite.** Traced to a 2009 Wikipedia revision footnoted to an unrelated article about a password vulnerability; since removed from Wikipedia, still mirrored on fan wikis. The Gregory quote says the *team uses* Perl among other languages — a different claim.
- **VentureBeat's MTGO oral history** claims the GRE works by "reading the card text and extrapolating behaviour, removing the requirement to individually develop every card" — **wrong**; parsing is offline in the GRP and 20% of cards need hand work. The 2017 Arena announcement's "sophisticated machine learning that can read any card we can dream up" is marketing, contradicted by Werner's deterministic CLIPS system. Don't cite either.
- **No GDC/Unite talk on Arena's engine exists**; the two Werner dev diaries are the entire public technical record. **No WotC/Hasbro patent** on a rules engine, card scripting, or card-text parsing exists (the 1994 "tapping" patent expired 2014) — **no patent exposure for pawl's design**.
- **Never discussed publicly by WotC:** determinism, replay, server authority, mana-pool representation. M0 already replays deterministically, so pawl is *ahead of the published record* here, not behind it.
- **"SLED" / rules-engine-as-a-service:** no evidence, likely misremembered.

---

## 9. Meta-lesson

The most valuable thing in Argentum is not a mechanism — it's the **gap between its docs and its code.** Its `docs/` describe trial-application dependency resolution and lambda escape hatches that *were never built*, and its `CLAUDE.md` still asserts them. A reader trusting the docs would confidently mis-model the engine. Two consequences for pawl: (1) **mark your own design docs aspirational-vs-shipped**, and never cite a doc as evidence of behavior; (2) the hardest bet in this whole design — trial-application 613.8 — is exactly where the strongest prior art's ambition quietly outran its implementation. Assume that gravity applies to you too, and instrument against it (D2).
