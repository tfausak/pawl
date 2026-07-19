# pawl

## A Magic: The Gathering rules engine in Haskell — design approach and implementation path

> *pawl* (n.) — the catch in a ratchet mechanism that permits motion in one direction only.

---

## 1. Thesis

**This is a virtual machine, not a game.**

| VM concept | MTG equivalent |
|---|---|
| The machine | The comprehensive rules: turn structure, priority, the stack, zones, layers, state-based actions |
| The instruction set (ISA) | The effect DSL |
| Opcode implementations | Effect semantics (`DealDamage`, `Mill`, `ChangeZone`, …) |
| Programs | The ~28,000 cards |

Four pieces, not three. The DSL is the one that's easy to leave off the list and the one everything else depends on. Changing an opcode's shape invalidates the interpreter *and* every card that uses it. Version it from day one; expect the first year to be churn.

### The closed half vs. the open half

**Closed half** — everything in the comprehensive rules that never names a card. Turn structure, phases and steps, priority, the stack, zones, timestamps, mana, combat, state-based actions (704), replacement effects, the layer system (613), targeting legality, copy rules, keyword abilities (702). This is **finite**. It will not grow. You can genuinely finish it.

**Open half** — the effect vocabulary. Grows forever. Additive, so that's fine.

**The invariant that keeps them apart:**

> The closed half depends on a *classification* of effects, never on the *identity* of effects.

The rules core legitimately needs to know things about an effect: which layer a continuous effect applies in, whether an ability is a mana ability (which changes the priority rules), whether it targets, how timestamps order it. It must never contain `case effect of DealDamage{} -> ...`.

That classification is your **ABI**. Design the metadata before you design the opcodes. Fusing these two halves is the single failure mode that turns this project into Forge — an engine that grows a special case every time a set drops.

### Turing completeness cuts *toward* a boring DSL

Magic is Turing complete (Churchill, Biderman & Herrick). Look at how the construction works: the tape is creatures on the battlefield, the transition table is a pile of Rotlung Reanimators distinguished by creature type. Every card in it is trivially first-order — *"whenever a thing dies, make a token."* No card contains a loop, a branch, or a recursive call.

**The cards are the transition table. The engine is the head and the tape.** The computation is emergent from trigger cascades under the turn structure.

So: the closed half is Turing complete whether you like it or not. The DSL stays first-order, non-recursive, and analyzable. This is where the VM analogy deliberately breaks — real bytecode has jumps, and yours must not.

---

## 2. Architectural decisions

These are the expensive-to-change ones. Everything else can be discovered along the way.

### 2.1 The engine is pure; choices are suspensions

```haskell
data Prompt r where
  ChooseAction   :: Decider -> PlayerId -> [LegalAction] -> Prompt LegalAction
  ChooseTargets  :: Decider -> PlayerId -> TargetSpec   -> Prompt [EntityId]
  OrderTriggers  :: Decider -> PlayerId -> [EntityId]   -> Prompt [EntityId]
  OrderGraveyard :: Decider -> PlayerId -> [EntityId]   -> Prompt [EntityId]
  Shuffle        :: [EntityId] -> Prompt [EntityId]
  CoinFlip       :: PlayerId   -> Prompt Bool
  RollDie        :: Int        -> Prompt Int

type Game = StateT GameState (Program Prompt)
```

Everything the engine cannot decide for itself becomes a suspension. One pure core, swappable interpreters: WebSocket for humans, MCTS for a bot, JSON replay for tests, deterministic script for the card test suite. Compile the same core to WASM for the browser.

### 2.2 Randomness is a prompt

`Shuffle` and `CoinFlip` sit next to the player choices deliberately. The engine *requests* randomness; it never generates it.

This buys, for free:
- deterministic replay
- seeded reproduction of any bug
- **determinization for MCTS** — a bot doesn't need a *state*, it needs an *information set*: "sample an opponent's hand consistent with what I've observed"

Bake a shuffled library into `GameState` on day one and you will fight this forever.

### 2.3 Decider ≠ player

Note the `Decider` field, separate from `PlayerId`. Rule **723 (Controlling Another Player)** is a rules section, not a card hack. Mindslaver, Word of Command, and Sen Triplets all fall out of getting this type right on day one and are a rewrite if you don't.

### 2.4 Multiplayer from the start

APNAP ordering, **801 (Limited Range of Influence)**, the monarch. A hardcoded two-player assumption is a rewrite, and it costs almost nothing to avoid up front.

### 2.5 Immutable state, base vs. projected

Store the **base state**. Compute the **projected state** by applying continuous effects in layer order. When Giant Growth wears off you remove the effect and recalculate — you never undo anything, because the base state never changed.

Consequences worth naming:
- **Last known information** is just an older value you kept a reference to. Free in Haskell via sharing; a nightmare of defensive copies in a mutable engine.
- **Rule 400.7** (an object changing zones becomes a new object) means entity IDs need real discipline. Decide the identity model early.
- **Rule 104.4b** — a mandatory loop with no choices is a draw. With immutable states, hash and compare. Nearly free. (XMage doesn't attempt this properly at all: it uses a heuristic loop counter plus a "draw game?" prompt — `GameImpl.java:1904` — not state comparison. Its deep-copy machinery is real but reserved for AI search; see M7.) (Turing completeness caps this: you cannot decide it in general. But the rule only asks about *mandatory* loops, which is exactly the tractable fragment.)

### 2.6 Layer dependency resolution

Layers 1–7 with sublayers in 7. The hard part is dependency: sometimes effect A changes whether effect B applies, which changes whether A applies. Use trial application — apply tentatively, detect dependencies, establish order, apply for real.

**Layer 3 (text-changing) is the one that kills engines.** See §5.

### 2.7 Cards are runtime data, never Haskell modules

A deep-embedded AST, loaded from files at runtime. Non-negotiable, for four reasons:

1. 28k Haskell modules destroys Nix build times
2. An LLM can emit card definitions without a rebuild
3. Cards become inspectable, serializable, generable
4. **Layer 3 becomes a tree transformation on a value you already have** — see §5

Cards are still compiled-in Haskell values today (`Pawl.Card` — pure data, no
lambdas, but Haskell modules all the same). **M3.5** is where this section is
actually cashed: JSON files under `./data/cards/` become the source of truth.
Until then the promise is declared, not redeemed.

### 2.8 `Card` vs. `Printing`

`Printing = Card + metadata`. This is not a convenience — it is the rules' own abstraction boundary. Artist, art, and expansion symbol are **not characteristics** of an object; the rules' list of characteristics (name, mana cost, color, type line, P/T, …) deliberately excludes them. The "artist matters" cards are silver-bordered *precisely because* they reach across that line.

MTGJSON already models this split exactly: **Card (Atomic)** holds evergreen properties that never change from printing to printing; **Card (Set)** holds printing-specific data. Their own example of the excluded field is `artist`. The data source hands you both halves, keyed correctly.

Consequences:

- **Printing belongs to the physical card, not the game object.** Under 400.7 a card changing zones becomes a new object with no memory — but its printing is a physical fact that persists. `Printing` is immutable identity attached to the card; game objects reference it.
- **Decklists are lists of printings**, not cards. Matters for a draft client; irrelevant for deckbuilding.
- **Round-trip (§4) targets atomic `text`** (oracle), never `originalText` (as-printed). Old printings have wildly different wording and would generate thousands of false failures.
- This moves most of XMage's *"artist/art/expansion symbol matters"* category **into scope** — see §6.

### 2.9 Prompt elision by static pool analysis

Many prompts are dead weight in most games. Ordering N cards is N! orderings; a Wrath killing eight creatures is 40,320 branches in an MCTS tree, essentially all meaningless. That's not a UI problem — it's a search-space problem in the hot path.

Because the DSL is first-order and analyzable (§1), you can ask *"does any card in this pool reference graveyard order?"* and, if not, emit a canonical order and never prompt.

**This is the first concrete cash-out of the non-Turing-complete DSL, and it generalizes to many prompts.**

Precedent: WotC ships exactly this as policy. The tournament rules state that in formats involving only cards from Urza's Saga and later, players may change graveyard order at any time — a static analysis of the legal card pool. Rosewater's informal version: the default is that it doesn't matter unless someone speaks up at the start of the game.

### 2.10 Three serialization types, not one

| Type | Purpose |
|---|---|
| `GameState` | Internal. Never leaves the process. |
| `PlayerView` | Projected through layers *and* filtered for hidden information. What the browser client gets. |
| `DecisionLog` | Seed + list of prompt responses. Tiny. **The canonical replayable artifact.** |

Snapshots become a derived optimization rather than the source of truth.

### 2.11 Never bake arity into the card model

`faces :: NonEmpty Face` plus a `Layout` tag. Not `left`/`right`.

The comprehensive rules contain a numbered enumeration of card layouts, all closed half, all specced: **709** Split, **710** Flip, **711** Leveler, **712** Double-Faced, **714** Saga, **715** Adventurer, **716** Class, **717** Attraction, **718** Prototype, **719** Case, **720** Omen, **721** Station.

Get the shape right once and that whole block becomes tractable together. Who // What // When // Where // Why — XMage's only *"Miscellaneous"* entry, a five-part split card — is then a test case, not a feature. It is rule 709 with N=5.

The same discipline applies to modes, targets, and colors. **Fixed arity is the recurring root cause behind XMage's list** (§6).

### 2.12 The numeric tower

**P/T and mana are not `Int`.**

MTGJSON already concedes this: `power` is typed as a *string*, explicitly because some cards have powers like `1+*`. That's a data model surrendering to the problem.

The tower must accommodate at least: integers, `*` (characteristic-defining), `X`, `1+*` — and, if you want the silver-border stress tests, `½` (Little Girl) and `{∞}` (Mox Lotus).

This is load-bearing, not a party trick. **Designing for Little Girl gets you Tarmogoyf for free.** Discovering it halfway through the vocabulary is expensive; deciding it before M4 is nearly free.

---

## 3. Implementation path

Sequenced to retire *architectural* risk first, not to maximize card count. The ABI is the thing you can't cheaply change later; Lightning Bolt will still be there in six months and will take an afternoon.

This section is the **forward plan**. For what has actually landed — one distilled entry per completed milestone, with its gate card, the decision it proved, the opcodes/types it added, and every elision and its named expiry — see the completion log in `progress.md`. Milestones **M0 through M3g are complete**.

### M0 — A complete game with zero cards

60 Mountains vs. 60 Mountains. Both players draw, tap, pass, deck out. No effects, no vocabulary, no oracle text.

Exercises: turn structure (703), priority, the stack, SBAs (704), the draw-from-empty-library loss condition, the whole engine loop.

Note that basic lands are genuinely zero-opcode: a Mountain's mana ability is granted intrinsically from its subtype by **rule 305.6**. It's closed half. The card data is a type line.

**Exit criterion:** a game completes and produces a `DecisionLog` that replays deterministically.

### M1 — Vanilla creatures

Split into **M1a** and **M1b**. As originally written ("Grizzly Bears. Combat: declare attackers, declare blockers, damage, SBAs") this milestone quietly bundled two independent subsystems: getting a creature *onto* the battlefield, and *fighting* with it. M0's stack was built but only ever exercised empty, and its only permanent-maker was the land special action, which bypasses the stack entirely. So "cast Grizzly Bears" is an entire pipeline — mana, the stack, cost payment, resolution — and none of it is combat. Split so that a failure in the mana model surfaces on its own rather than tangled with combat bugs.

Lettered rather than renumbered: M2–M7 are referenced by number throughout, and the lettering says what's true — two halves of one idea, sequenced.

#### M1a — Casting a creature

Goblin Piker (`{1}{R}` 2/1 vanilla) rather than Grizzly Bears (`{1}{G}`), to reuse M0's Mountain mana base and fixtures; `{1}{R}` still exercises generic *and* colored payment. Deck is 36 Mountain / 24 Piker. Creatures resolve onto the battlefield and sit there. Still zero opcodes.

Exercises: the mana pool (106.4) and its emptying (500.4), intrinsic mana abilities from a subtype (305.6), casting (601), the stack carrying an object, priority under a non-empty stack (117.4), resolution of a permanent spell (608.3), cleanup discard as a real decision (514.2).

Two things get decided here because this is where they first appear:
- **The numeric model** (§2.12) — `Quantity`, shape landed with only `Literal` implemented. Named `Quantity`, not `Characteristic`: 109.3 already owns that word for the whole characteristic set.
- **Mana pool shape** — a multiset of *units*, not counts per type. Counts discard provenance by construction, and mana isn't fungible (snow `{S}`; Cavern-style spend restrictions; Yawgmoth's Day Planner's "only mana produced by abilities that caused you to lose life"). Counts→units is a rewrite; units→richer-units is a field addition.

Riders split two ways, and it matters: **production-time tags** (snow, caused-life-loss) are *closed half* — observable facts about the production event. **Spending restrictions** ("only Bard creatures") are *open half* — predicates over spells, and payment must ask a classification, never case on them.

**Risk:** the `priorityLoop` restructure to 117.4 — the only live M0 code with existing tests on it.

**Exit criterion:** a game in which creature spells are cast and resolve completes, and its `DecisionLog` replays deterministically.

Spec: `docs/superpowers/specs/2026-07-16-m1a-casting-design.md`.

#### M1b — Combat

Declare attackers, declare blockers, combat damage, and the SBAs that follow (lethal damage 704.5g, zero toughness 704.5f). Summoning sickness (302.6) lands here — nothing can observe it in M1a. Still zero opcodes.

This is the combat *skeleton*; M2 is where combat gets genuinely hard.

**Exit criterion:** M0's "no life changes" property finally fails — the first damage in the engine's life.

### M2 — French vanilla (the underrated milestone)

Creatures with keyword abilities and nothing else. **Still zero opcodes** — keywords are rule 702, not card text. `K:Flying` is a citation, not an effect.

This is where combat gets genuinely hard, and it's all closed half:
- flying / reach / menace → blocking restrictions
- first strike / double strike → restructures the combat damage step into two
- deathtouch / trample / lifelink → damage assignment and replacement interactions
- protection → the DEBT four-way (damage, enchant/equip, block, target)

**702 runs past 170 entries.** It is the densest single chunk of closed-half work you have — and it is enumerated, which means it is finishable. Deathtouch + trample + protection + first strike is a real rules problem solvable with an empty vocabulary.

#### The split: M2a / M2b / M2c, and why protection is not in it

702 has **194 entries**, and WotC ordered them for us: **702.2–702.21 is the evergreen block, alphabetical**; **702.22+ is chronological by set**, starting at banding. That is the easy/hard line, and it is theirs, not ours.

The strategy is **one keyword per structural axis to prove the architecture, then volume**. Not "all of one group first" — that proves you can do that group. Each proving milestone carries the keyword that **falsifies its own naive implementation**.

- **M2a — the seam + flying, reach, defender, vigilance, haste.** Touches `Combat`. Five keywords, four shapes: a relation (flying/reach), a unary attack legality (defender), a modifier on an action (vigilance), an override of existing state (haste). *Reach is the falsifier* — a reach creature blocks a flier without having flying, so flying-alone lets you write a check that passes its own test and is wrong. Spec: `docs/superpowers/specs/2026-07-16-m2a-keywords-design.md`.
- **M2b — first strike + double strike.** Touches `Turn`. *Double strike is the falsifier*: the obvious implementation is "first strikers in step one, everyone else in step two", which double strike breaks by being in **both**. Also pays off `git-bug 5f50eec` — see below.

  **M2b should make the turn *data*, not a pure function plus flags.** `Turn.next :: Phase -> Maybe Phase` walks a static `allPhases`, so nothing can add to a turn; and `GameState.phase` holds a *kind* of phase rather than an *occurrence*, so two combat phases in one turn are indistinguishable. Model the remaining phases as a sequence in `GameState`, with `allPhases` demoted to the template a new turn refills from, and one mechanism covers everything: CR 508.8's skip is a drop, 510.4's second damage step is a splice (CR 500.9), and an additional combat phase is a splice (CR 500.8 — "adding the phases directly after the specified phase … the most recently created phase will occur first", which is exactly *splice after current*, LIFO, for free). Occurrence identity becomes positional. The alternative — one flag per conditional — needs a new flag for every future case and cannot represent extra phases at all. The *cards* are M4+ (Relentless Assault, Aggravated Assault, World at War, Moraug are effects and need opcodes), but the mechanism is M2b's to choose, and it is cheap now and a retrofit later. Not covered by this: Moraug's "for each time it has attacked this turn" is turn-scoped **event history** — the same want M1b's spec already recorded for deathtouch and Bloodthirst-style cards ("those want an event log, which the engine needs anyway once triggers exist"). Second customer, not a new problem.

  **Pre-spec notes for M2b** *(2026-07-17, from the third-wave prior-art research and a code audit at M2a's completion — for whoever writes the spec):*

  - **Turn-as-data has an existence proof at scale.** ygopro-core's engine is a resumable step-machine whose pending work is a spliceable list of process units (`field.h:197`; `prior-art-lessons.md` §10.3, 13k cards). Cite it in the spec; the design needn't be re-argued from first principles.
  - **Evaluate CR 510.4 at the step boundary, through the projection.** "Any attacking or blocking creature has first strike or double strike" is a keyword question, and M2a made keyword reads a projection (`keywordsOf`, never `Card.keywords`). The splice decision must consult the projection *when the boundary is reached*, not precomputed at combat start — at M3 an ability granted or removed mid-combat changes the answer. Precomputing is Arena's Zurgo failure (recalculation avoided at step boundaries; `prior-art-lessons.md` §8.4). Check the exact timing wording against CR 510.4/510.5 in `rules.txt`.
  - **Decide where the atom/event funnel lands.** M3's obligations (see the M3 section) require every observable mutation to flow through one change-and-emit helper *before* M3, and only M2b and M2c remain. M2b reopens the damage call sites anyway (two waves with an SBA check between), and M2c's deathtouch bit is the first reader of a damage event record. Either adopt the funnel for damage in M2b, or write the expiry naming M2c — the failure mode is drifting past both.
  - **Code pointers at M2a's HEAD** for the 5f50eec fix: `allPhases` (`Turn.hs:9`), the static walk in `next` (`Turn.hs:28-33`), unconditional `grantsPriority` (`Turn.hs:35`). One shared predicate lands across those three. Also: the SBA check between the two damage steps runs M1b's single-pass SBA loop in a new mid-combat position — sound while nothing triggers, but the spec should say so, because first-strike deaths before the second wave is where the loop's position first becomes load-bearing.
  - **No rulings step.** The obvious test cards (Boros Swiftblade, Fencing Ace, Youthful Knight) carry zero Gatherer rulings (AllPrintings 5.3.0+20260717) — french-vanilla keywords' oracle is the CR itself. §4's rulings discipline first pays off at M3+. Test cards stay Scryfall-cited with CR-numbered test names, per M2a's convention.
  - If the spec touches `Damage.hs`, leave a comment at `attackerAssignment` pointing at the expiry M2c owes (the damage-assignment chooser hardcoded as the attacker's controller — banding 702.22j and Mindslaver both falsify it, per the M2c bullet below) so the assumption isn't silently rebuilt into the two-step structure.
- **M2c — deathtouch + trample.** Touches `Damage`/`Sba`. They look like one group and are different axes — deathtouch adds a state-based action (704.5h) plus a transient "dealt damage by a deathtouch source since the last SBA check" bit; trample restructures *assignment* and brings lethal-in-order back inside the keyword, where M1b said it belonged (702.19b). *Their interaction is the falsifier*: 702.2c makes any nonzero deathtouch assignment count as lethal "for the purposes of determining if excess damage is being dealt" — which is precisely trample's calculation.

**The evergreen twenty triage completely:**

| Bucket | Keywords | Count |
|---|---|---|
| **Covered** M2a/b/c | deathtouch, defender, double strike, first strike, flying, haste, reach, trample, vigilance | 9 |
| **Punchlist** — same axes, no new machinery | indestructible, intimidate, landwalk, lifelink | 4 |
| **Blocked** on machinery that does not exist | enchant, equip, flash, hexproof, protection, shroud, ward | 7 |

**Protection is not a judgment call.** CR 702.16 has five clauses; at M2 three have nothing to attach to — 702.16b (can't be targeted; nothing targets with zero opcodes), 702.16c/d (Auras and Equipment need `Attach`, a 701 keyword action, M4). Of the two live clauses, 702.16f (can't be blocked by) is *the same machinery flying builds*, and 702.16e ("any damage … is prevented") drags in CR 615's prevention subsystem, which has no other consumer until burn at M4. It would be one new axis bolted to three clauses nothing can falsify — the argument M1b used to reject a `controller` field.

**Banding (702.22) is punted for a better reason than difficulty.** 702.22j: "the defending player (rather than the active player) chooses how the attacking creature's damage is assigned." M1b's `Damage.attackerAssignment` hardcodes the chooser as `controllerOf attacker`. Banding falsifies that from one direction and Mindslaver from the other — it is a **`Decider` problem wearing a combat costume**, and cannot land before M3 at any price. This assumption is in no expiry table; M2c's spec should add it.

**CR 506.1 is one requirement, not two** (`git-bug 5f50eec`): "The declare blockers and combat damage steps are skipped if no creatures are declared as attackers (508.8). There are two combat damage steps if any attacking or blocking creature has first strike or double strike." Both are conditional turn structure, and M1b implements neither — `Turn.grantsPriority` returns `True` for those steps unconditionally, so today, on every turn where nobody attacks, pawl grants two priority rounds the rules say to skip. Unobservable while nothing can be cast at instant speed; a real bug the moment flash exists. CR 500.11 gives the semantics: skipping is "proceed past it as though it didn't exist" — no priority, no turn-based action. One predicate consulted by both `Turn.next` and `grantsPriority` serves 508.8 and 510.4 together, which is why the fix belongs to M2b rather than to a lone bugfix with one consumer.

### M3 — The ABI test (go / no-go)

With a vocabulary of roughly a dozen opcodes, make these work:

| Card(s) | What it proves |
|---|---|
| **Magical Hack** | Layer 3. Your effect AST is rewritable at runtime. |
| **Humility + Opalescence** | Layer dependency resolution via trial application. |
| **Panglacial Wurm** | Library search is not atomic — it's a nested game context. |
| **Mindslaver** | `Decider` ≠ `PlayerId`. |
| **Rest in Peace** | CR 614. An event is a value a replacement effect rewrites before it happens — and `changeZone`, today fire-and-forget, is interceptable. ("If a card or token would be put into a graveyard from anywhere, exile it instead." — Scryfall.) |

If these work with twelve opcodes, the ABI is right and the rest is volume. If they don't, you found out now instead of at card #8,000.

#### Three ABI decisions M3 owes beyond the gate cards

A 2026-07-17 audit of the specs against the code found these committed nowhere, and each is cheaper to decide before the M3 spec than during it. Detail and evidence: `prior-art-lessons.md` §3 (D3, D4) and §10.

1. **The event substrate.** Triggered abilities have reserved names (`OrderTriggers`, the commented-out `OfAbility`) but no designed mechanism, and three docs have independently wanted an event log (M1b's deathtouch note, M2b's Moraug note, the single-pass SBA loop's admitted debt). Adopt the "atom" pattern before M3: every observable mutation goes through one helper that performs the change *and* emits its event *and* owns the no-op→no-event guard (603.2f).
2. **Effect ≡ event.** MedeaMelana's Magic shapes each primitive effect so the effect value *is* the event value, flowing through one pipeline: propose → replacement-rewrite (614) → apply → emit for triggers (603). One vocabulary wires replacements and triggers together, and D3 (copy resolved at entry) is the same seam. Rest in Peace is the gate card that proves it.
3. **Binding.** How does a first-order effect reference prior choices and payments — "if {B} was spent to cast this," "for each," a mode chosen earlier, X? mtg-pure hit this wall and punted (§10.1's cost-continuation warning). The answer for a first-order DSL is named binding slots, not lambdas; M1a's mana-unit provenance is already the closed-half half of it. This shapes every opcode's fields — decide it with the first dozen, not at opcode #150.

#### The split: M3a–M3g *(planning pass 2026-07-17, at M2d's completion — pre-spec)*

The five gate cards are the right five — each attacks a different load-bearing wall, and nothing in them is premature (no Attach, no protection, no copy; D3 stays deferred). But each drags in an entire subsystem that does not exist at M2d: nothing targets, nothing casts at instant speed, there is no activated ability beyond the intrinsic CR 305.6 tap, no triggered ability, no continuous effect, no replacement, no search. M3 as one milestone is seven independent axes wearing one number. Same discipline as M2: **one structural axis per letter, each carrying the card that falsifies its own naive implementation.**

| | Axis | Gates | Falsifier |
|---|---|---|---|
| **M3a** | Effects as data: the DSL core, targeting, instant speed | Lightning Bolt | Bolt-vs-Bolt: CR 608.2b re-checks target legality at resolution, so stored-resolved-targets is wrong — forces symbolic late-binding targets |
| **M3b** | The projection generalized: continuous effects, single-effect layers, durations | Giant Growth; a real keyword granter/remover; Humility solo | Grant/remove deathtouch between damage-deal and SBA check: `Sba.woundedByDeathtouch`'s live read (its comment names this expiry) becomes wrong; `DamageEvent` grows a deal-time deathtouch bit (CR 702.2c/702.2e) |
| **M3c** | CR 613.8 dependency: trial application — **the go/no-go** | Humility + Opalescence, both timestamp orders; Blood Moon + Urborg, both orders | Blood Moon/Urborg is the pair Argentum couldn't represent; port its layer tests but assert *correct* outcomes — its Blood Moon test asserts a documented wrong one |
| **M3d** | Layer 3: the rewritable AST | Magical Hack — on a permanent *and* on a spell on the stack | A hacked basic Mountain taps for the new color with zero special cases, purely because CR 305.6 reads the projected type line (CR 612.1: text-changing covers type-line text) |
| **M3e** | Abilities on the stack: activation (CR 602), non-mana costs, the CR 605 classification | Prodigal Sorcerer (reuses DealDamage); Evolving Wilds (sacrifice cost, Search) | Mana abilities must *not* go on the stack — CR 605.1a is literally an ABI predicate, the classification bit Shandalar, Duels, and Arena all independently grew (§8.2) |
| **M3f** | The event pipeline: triggers (603) + replacements (614) | Rest in Peace, whole card | RiP must catch a *token* dying and a *spell* going to the graveyard from the stack (a resolved Bolt gets exiled) — replacement at the `changeZone` funnel, not a battlefield-only hook; plus the three-pass trigger scan (§4-M0) |
| **M3g** | The payoff pair: Decider (CR 723) + re-entrancy | Mindslaver; Panglacial Wurm | Mindslaver: prompts aimed at the controlled player route to the controller while resource ownership stays put (Argentum's `actorFor`). Panglacial: cast mid-search *while Evolving Wilds' ability is still resolving*, then the search continues (CR 605.3a permits mana activation mid-resolution) |

Notes the letters' specs must not lose:

- **Ordering.** a→b→c→d front-loads the two bets no prior art has ever landed — trial application and the rewritable AST. **The go/no-go verdict arrives at the end of M3d, not M3g**: e–g validate seams prior art already proves work (ygopro's suspension protocol at 13k cards, Argentum's `actorFor`). If the architecture is going to die, it dies by M3d. M3e must precede M3g (Mindslaver and Evolving Wilds both need activation); e/f are otherwise independent of b/c/d.
- **The vocabulary is three leaf families, not twelve opcodes.** Tallied against the gates' oracle texts, ~12 leaves holds — but they split into one-shot effects, continuous-effect specifications (classified by layer), and replacement specifications (classified by the event pattern they intercept), each with its own ABI classification record. Designing those three records *is* the ABI test; the leaf count is almost incidental.
- **Rest in Peace's hidden dependency.** The gate table quotes only the replacement clause, but the card's first line is an ETB trigger ("When this enchantment enters, exile all graveyards" — Scryfall). The RiP gate therefore implies minimal CR 603 machinery; M3f owns it deliberately rather than discovering it mid-spec.
- **Giant Growth is added as a gate** because no original gate exercised until-end-of-turn durations — Humility, Opalescence, and RiP are permanent statics, and Magical Hack "lasts indefinitely." §2.5's founding story ("when Giant Growth wears off you remove the effect and recalculate") was in no milestone. M3b owns durations: CR 611.2b's two-phase latch, 611.2c's fixed-set semantics, wear-off as delete-and-recompute.
- **M3b discharges M2c's debts.** The synthetic both-keywords fixture's expiry waits on a layer-6 grant; the grant is also what makes the M2c live-projection reads and CR 702.2e's last-known information load-bearing, per the current-work note in `CLAUDE.md`.
- **613.8 precision.** Humility+Opalescence's famous behavior is cross-layer (4 vs 6) plus 7b timestamp ordering; the pair that genuinely exercises 613.8 dependency — applying one changes the *existence* of the other's effect — is Blood Moon+Urborg. The M3c spec must derive which pair witnesses what from `rules.txt`, not folklore, and the test set must exist **before** the resolver (risk register).
- **`abilitiesOf` is a projection from day one** (M3e), the same move as `keywordsOf` — so a Humility'd Prodigal Sorcerer can't tap.
- **Mindslaver details.** It's *Legendary*: tests keep it singleton or note the CR 704.5j elision. CR 723.1: control applies to the next turn the player *actually takes*; 723.1a: multiple player-controlling effects overwrite — the duration model is turn-scheduled, not time-scheduled.
- **`git-bug 15de615`** (Setup: `emptyGame`/matchup player agreement by construction) lands at the **front of M3a** — the first letter to touch the setup seam (new printings and matchups for instant coverage; Magical Hack's `{U}` and the white gates need Island/Plains printings, cheap under M2d's per-player mono-color decks). The bug's own "revisit at Mindslaver" note predates this split; under it, every letter after M3a inherits whatever Setup does, so the unenforced invariant must not survive six letters of new fixtures.
- **Random-game coverage will trail again.** Like M2c→M2d, most gates land as deterministic fixtures; an M2d-style coverage tail (a white or blue matchup for random games) should follow M3 rather than be discovered later.

### M3.5 — Cards as data files (cashing §2.7)

*(Pre-spec note, 2026-07-18. Numbered `.5` deliberately: it is an interstitial
between M3 and M4 and does **not** renumber M4–M7, which are cited by number
throughout.)*

Cards today are hand-written `Printing` values compiled into `Pawl.Card` — pure
data, no lambdas, but Haskell modules, which §2.7 names as not the end state.
This milestone makes JSON the card representation and files the source of truth.
**Zero opcodes, zero rules** — a representation change, and it is sequenced
*before* M4 on purpose: proving the codec against the two-dozen-card M3 pool is
cheap, and every M4 opcode is then born serializable instead of retrofitted
across ~200 cards. It is also the honesty boundary — the round-trip is what
mechanically forbids a closure from ever entering the card model (§2.7's
"never Haskell modules" made enforceable rather than aspirational).

Two steps, **A** then **B**:

- **A — the codec and the honesty round-trip.** A hand-rolled JSON
  parser/renderer (`Pawl.Type.Json` for the value; `Pawl.Json` for parse and
  render) plus a `Card ⇆ Json` codec. The payoff is a structural round-trip
  property over `allPrintings`: `jsonToCard . cardToJson ≡ Right`. This is the
  "keeps us honest" check — it proves the card model is fully first-order data,
  and fails loudly the day a lambda is smuggled in. Self-contained; does not
  touch the engine loop.
- **B — files become the source of truth.** Render every card to
  `./data/cards/<slug>.json` (slugified name or Gatherer ID — the spec picks
  one), commit the files, and flip `Pawl.Card` from hand-written values to a
  loader that parses them. The invariant that makes B safe is A's property, now
  load-bearing: the committed files parse back to byte-identical cards.
  **"Runtime" is the test suite** — the loader serves the tests; there is no
  engine-shipped card database and no embedding to design yet.

Pre-spec notes for whoever writes the spec (option 1, when this milestone is next
up):

- **No aeson** (`prefer-boot-libraries`; a hand-rolled JSON parser needs zero
  dependencies).
- **Lift-and-shift from scrod, don't reinvent.** `Scrod/Json/Value.hs`
  (github.com/tfausak/scrod) is the same author and largely the same house
  style; port it near-verbatim, reconciling only residual gaps (export lists,
  extension pragmas). Clone into `_scratch/` for reference. Steal scrod's own
  **`Decimal`** type with it: it exists precisely to carry JSON numbers without
  `Rational`, which is exactly the call to make here.
- **The numeric tower is the trap (§2.12).** JSON numbers decode to
  `Decimal`/`Integer`, never `Double` — fixed-width floats are barred by the
  arbitrary-precision rule. And `Quantity`, mana symbols, subtypes and card
  types are tagged sums: they serialize as tagged objects, never bare JSON
  scalars. A `power` of `*` is not a number.
- **Codec shape** — a `ToJson`/`FromJson` class pair versus free
  `xToJson`/`jsonToX` functions — is the spec's call; either way the round-trip
  law holds and decode returns `Either`, so a malformed file is a loud test
  failure, not a partial crash.
- `allPrintings` is already the hygiene registry ("a printing not listed here
  escapes the hygiene net"); the round-trip property iterates it, and at B the
  loader is what repopulates it.

### M4 — The vocabulary

**Rule 701 (Keyword Actions) is your opcode list, written by Wizards.** ~55 entries, numbered, individually specified: Activate, Attach, Cast, Counter, Create, Destroy, Discard, Exchange, Exile, Fight, Mill, Play, Reveal, Sacrifice, Scry, Search, Shuffle, Tap/Untap, Explore, Surveil, Amass, Connive, Venture, Manifest, Meld, Discover, Time Travel, Convert…

Note what's **not** in 701: draw, deal damage, gain life. Rule 701.1 is explicit that most actions use standard English definitions of their verbs; 701 defines only the specialized ones. So 701 is precisely the list of opcodes whose semantics you'd get wrong by intuition. Nobody mis-models "draw a card." Everybody mis-models Manifest.

Add the obvious English verbs (draw, damage, life, counters, +N/+N) alongside. Vocabulary usage across the corpus is brutally Zipfian — the first ~150–200 opcodes likely cover 80%+ of cards, because most of Magic is creatures, removal, counterspells and draw.

**The cards are not the long pole. The vocabulary is.** "All the cards" isn't a third piece of comparable size once the DSL exists; it's derived. What's open-ended is the tail of opcodes needed by the last 15%, each bought at the price of a full scenario test suite.

### M5 — The nightmares, as rules sections

These are **not** exotic opcodes. They're numbered sections of the closed half:

- **723** Controlling Another Player → Mindslaver, Word of Command
- **727** Restarting the Game → Karn Liberated
- **729** Subgames → Shahrazad, Enter the Dungeon
- **732** Taking Shortcuts
- **733** Handling Illegal Actions

*(Numbers per the vendored `rules.txt`; the CR renumbers this block as sections are inserted, so cite it, not memory — the 2026-07-17 audit found the previous numbering here had already drifted by one.)*

XMage lists Shahrazad as unimplementable not because 729 is unclear but because their architecture can't nest a game inside a game. Yours nests a `Program Prompt` inside a `Program Prompt` — which is a function call.

mtg-pure corroborates this from inside Haskell: its prompts are IO callbacks (`promptPick :: … -> m a`), so a game in flight cannot be snapshotted or resumed — subgames are absent and Mindslaver is a `-- TODO` comment. The suspension model is what makes 729 a function call; the language is not.

Keep this claim scoped to true subgames (729). Restarting (727) is *not* the same problem: XMage does implement Karn Liberated by mutating the single `Game` instance in place (`KarnLiberated.java:95`, code comment: "dirty hack… can cause bugs"). Restart-in-place is tractable even in a mutable engine; nesting a subgame to completion and *resuming the parent* is the part that isn't.

Do 732 and 733 eventually. Nobody plans for them; everybody needs them.

### M6 — The transpiler

Only now, once the DSL has shape. Two inputs:

1. **LLM bulk translation** of MTGJSON oracle text → DSL draft. Cards are individually testable units of work; this is the most LLM-tractable task imaginable. It's a batch job, not a project.
2. **Forge's `cardsfolder`** as reference semantics — ~28k text files, the only machine-readable encoding of card behavior in existence. *Check the license before deriving from it.* Use it as input data to transpile, never as design inspiration; it's a decade of accreted special cases.

### M7 — Interpreters

Bot (MCTS over suspended continuations — resuming twice with different decisions is free; Forge and XMage deep-copy state to search, you just hold the continuation). WASM for the browser. WebSocket server. Headless simulation harness for deckbuilding.

---

## 4. Testing strategy

**Three pieces, three oracles — and one has none.** This asymmetry is the most important thing to internalize.

| Piece | Oracle | Scales? |
|---|---|---|
| Closed half | The comprehensive rules. Numbered, finite, citable. Name tests after rule numbers → free coverage map. | Yes |
| Cards | **Round-trip against MTGJSON.** Pretty-print AST → oracle text, diff against MTGJSON's `text` field for all 28k. | Yes, automatically |
| **Open half** | **Nothing complete. The one partial oracle is official rulings — below.** | **No** |

**The trap:** if `DealDamage` is implemented wrong, every card using it round-trips *perfectly* and every card is *wrong*. The round-trip validates that a card **says** the right thing. It says nothing about whether the interpreter **does** the right thing.

So the open half needs hand-written scenario tests per opcode. That work doesn't scale and doesn't parallelize with an LLM. It's the real cost center — not the cards.

### Breadth over depth, and the opcode definition-of-done

*(Added 2026-07-18.)*

Two coupled commitments about how the open half is populated.

**An effect is not done until a card exercises it in a gameplay-level test.**
Not a unit test of the executor in isolation — a scenario that casts or resolves
the effect through the stack and asserts on the resulting game state. Prefer a
**real, recognizable** card (the `tests-prefer-real-cards` discipline). A
**labeled synthetic crutch with a documented expiry** naming the milestone that
retires it is legitimate — but *only* when a real card would drag in something
not yet built, which is exactly the `Landform` and synthetic-deathtrampler
pattern already in use. This is the §1 invariant's testing corollary: coverage
is per-*classification*/opcode, and the card is how you prove the interpreter
**does** the right thing — the round-trip proves only that the card **says** it
(the central trap above).

**Breadth is the progress signal; card count is not.** What to optimize is
effect coverage — how many distinct opcodes have a card and a passing
gameplay test. 200 cards spanning removal, draw, lifelink, counters, tokens,
sacrifice, bounce and mill are worth far more than 2,000 near-duplicate vanilla
creatures. **The card-count metric is deliberately deferred:** it graduates to a
tracked health number *eventually* (beside §6's compiled-in count and the
round-trip %), once the M4 vocabulary and M6 transpiler make volume meaningful —
but chasing it early manufactures motion without progress and is a burnout trap.
Cheap JSON card files (M3.5) are what make breadth low-friction to pursue when
that time comes.

### Errata and rulings — the partial oracle for the open half

*(Added 2026-07-17. Counts measured against MTGJSON AllPrintings 5.3.0+20260717, English printings.)*

The "no oracle" cell above is slightly too strong, and the exception is worth engineering around. **Gatherer rulings are dated, official WotC statements of expected behavior** — many are literally "in this situation, X happens": scenario tests someone at Wizards already wrote. They ride along in the data source already chosen for the round-trip: **19,801 of 34,652 card names (57%) carry at least one ruling.** Rulings don't validate the interpreter automatically — someone still transcribes each into a scenario — but they answer *which scenarios matter*, which is the expensive half of hand-written tests. **M4 discipline: when an opcode lands, pull the rulings for the cards that use it and transcribe the Q&A-shaped ones.**

Where the errata trail lives, in decreasing order of authority:

- **Oracle (Gatherer)** — the authoritative current text; errata's end state, no history. Already the round-trip target via MTGJSON `text`.
- **Update Bulletins** (magic.wizards.com announcements) — official per-set articles documenting Oracle and CR changes *with rationale*, labeling each change functional vs. non-functional. Published regularly until fall 2023, sporadically since; judges' unofficial bulletins (The Name of the Rule, blogs.magicjudges.org) fill the gap at judge-grade quality but without WotC authority.
- **Per-set Release Notes** (magic.wizards.com/en/rules) — still published for every set; the card-specific notes are pre-written tricky-interaction tests for each new mechanic.

**The computable corpus is already in hand, and it is mostly noise by construction:** `originalText ≠ text` on **47,831 printings — 22,585 distinct names, 65% of all cards**. That 65% is not "two-thirds of Magic has errata"; it is templating churn (the 2024 "enters the battlefield" → "enters" sweep alone touches most of the game). The functional subset is small, and the Update Bulletins are its labels. Don't diff-mine first; read the bulletins, then use the diff to locate printings.

Two operational consequences:

- **Oracle text moves.** Pin the MTGJSON snapshot the round-trip runs against and re-baseline deliberately, the same way the AST is versioned. A round-trip failure after a data refresh may be an errata event, not a regression.
- **Rulings move too.** WotC prunes and rewrites them — Humility, the poster child for layer confusion, carries only **3** rulings today; its infamous longer list was retired as the layer rules matured. A transcribed scenario test outlives the ruling that inspired it, so record the ruling's date in the test name.

`tests/nightmares/` gains a category: **the functional-errata sagas** — cards whose printed text Oracle itself couldn't honor without patching. Candidates: Time Vault (erratad repeatedly across two decades), the Grand Creature Type Update (2007), Lotus Vale and its kin. Each is a documented case of WotC debugging a card against the rules, usually with a bulletin explaining the intended semantics — verify each specific history against the bulletins before transcribing, per §9 of `prior-art-lessons.md`: never cite a doc (including this one) as evidence of behavior.

**Coverage metric:** % of cards that transpile and round-trip. Let it tell you where the tail actually hurts.

**`tests/nightmares/`** — transcribe XMage's *"List of cards that will not be implemented"* now, one file per **category**, all marked pending. It's a ready-made architectural roadmap written by people who lost.

---

## 5. Why layer 3 is the canary

**XMage lists text-changing effects as "very unlikely."** Magical Hack, Sleight of Mind, Artificial Evolution, Swirl the Mists, Crystal Spray, Mind Bend, Glamerdye — 15 cards, punted. That's layer 3, sitting in the middle of the layer system, and a mature 15-year-old engine can't do it.

Why: layers 1–2 and 4–7 rewrite *game state*. **Layer 3 rewrites card text** — the rules of an object, mid-game. In XMage every card is a Java class. It *can* rewrite structured characteristics — subtypes, colors — at runtime (that's ordinary layer 3; see `NewBlood.java:147`). But the *words* that make up an ability live inside compiled Java logic, and there is nothing there to rewrite. That's the precise blocker: not the absence of a mutation mechanism, but rules encoded as code. (The named text-changers — Magical Hack, Sleight of Mind, Artificial Evolution, Mind Bend, Glamerdye — are simply absent from the repo.)

In this design, cards are a data AST loaded at runtime. Text-changing is a tree transformation on a value you already have. **The decision made for build-time reasons (§2.7) is the same decision that unlocks the category that beat XMage.**

Put Magical Hack in the M3 acceptance suite. If the effect AST isn't rewritable, you want to know at card #3.

Related datapoint, now corrected: **Mutate** (~40 Ikoria cards) is often cited as an XMage limitation (issue #6390) — but the issue was resolved. XMage implements Mutate today (`PermanentImpl.mutate():737`), and did it with exactly the list-based merged-permanent model §2.11 argues for. The failure got fixed; the lesson didn't change — fixed arity was the root cause, a list was the cure. Read it as evidence *for* §2.11, not as an XMage impossibility.

---

## 6. Escape hatches and non-goals

### The compiled-in card list

There will be a few dozen cards where the DSL is the wrong tool and contorting it to fit them makes it worse for the other 27,950. Word of Command. Shahrazad. Illusionary Mask.

The temptation is to make the DSL Turing complete so it can express them. **Resist.** The moment cards can compute arbitrarily, you've lost the analyzability that made cards-as-data worth having.

Instead: allow a small set of hand-written, compiled-in cards alongside the loaded ones. Forge effectively does this. **Track the count as a metric.** Growing → your DSL has a real gap. Stable around 30–50 → that's just Magic being Magic.

### Explicitly out of scope

Two different reasons. Worth separating, because the second list might get reversed on a whim someday and the first never will.

**Impossible — no data, or no ground truth:**

- **Art-content matters** — cards caring about what is *depicted*. No dataset has this, and it's arguably subjective, so even manual annotation has no ground truth. (A vision model over card images is a plausible one-time batch job. It is not a milestone.)
- **Cards that track what people are/do/have/say** — the Unglued/Unhinged social cards
- **People from outside the game** — Kindslaver, Subcontract

**Possible but pointless — zero downstream leverage:**

- **Dexterity** — Chaos Orb, Falling Star, Chaos Confetti. Costs exactly one prompt constructor (`DexterityCheck :: PlayerId -> Prompt Bool`); the engine can't tell whether the outside world answering it is a human QTE, an RNG, a bot, or a hand flipping cardboard, and the outcome lands in the `DecisionLog` like any other response, so replay and determinization keep working. Excluded on value, not capability.
- **Ante** — the 9 cards referencing it
- **Contraptions** — Steamflogger Boss and friends. Not hard: two extra zones (Contraption deck, scrapyard), a battlefield subzone (three sprockets), a wrapping CRANK! counter, one turn-based action at upkeep, optional triggers. XMage punts for the usual reason — zones are an enum and sprockets are a position their permanent model has no room for (§2.11 again).

  **Caveat on leverage:** Contraptions share a substrate with **Attractions (rule 717)**, their black-border descendant — separate deck, separate graveyard (the junkyard, i.e. the scrapyard renamed), scheduled triggers, and a keyword action at 701.49 ("roll to visit"). Attractions are Commander-legal. Contraptions are Attractions with a deterministic scheduler instead of a d6. So: zero leverage *as Contraptions*, real leverage *as the substrate* — if 717 is ever in scope, Contraptions are nearly free.

  Note rule 701.45a defines Assemble and then states that Unstable cards and mechanics aren't included in the rules. WotC drew this scope boundary in the rulebook.

  Steamflogger Boss itself is the only Commander-legal card touching Contraptions, and in a black-border pool its Contraption clause is a replacement effect that can never fire. **It is supported at M1 by doing nothing.**

**Declined, with a known price — possible and genuinely valuable, but too expensive today:**

- **Draft matters** — the Conspiracy cards.

  A draft is *not* a game and cannot be built "on top of" this engine: no zones, no stack, no priority, no permanents, no SBAs. It shares nothing but the card database. It's a sibling, not a child, and a standalone draft engine is close to trivial — packs, seats, picks, a passing direction.

  **The price is a second VM.** Cards like Cogwork Librarian, Agent of Acquisitions and Lore Seeker need their own closed half (pack passing, pick order, card pools) and their own opcode vocabulary (draft an extra card, reveal a pack, add a pack, take the whole pack). That's a second closed/open/DSL/cards stack. Declined on that basis — not because it's hard, and *not because it's impossible*.

  **The half that matters is free anyway.** The category splits: cards that *affect the draft* need the second VM; cards that merely *note something during the draft and use it in the game* (Aether Searcher, Animus of Predation) need nothing but a per-card annotation on decklist entries. The engine reads a value and doesn't care that a draft put it there — a human could type it in. Same discipline as `Printing` (§2.8): don't collapse a field just because you can't currently populate it.

**You don't need to ban any of this.** Chaos Orb, Falling Star and Shahrazad are already banned in every format WotC runs. Format legality is machinery you need anyway — MTGJSON ships `legalities` on the atomic model, and the deckbuilder needs it to know Black Lotus isn't Modern-legal. These fall out of the pool through the same door. No special case, no engine flag.

### Silver border: canaries, not flexes

Sorting rule: **do the un-cards whose difficulty is shared with black border.**

| Un-card | Black-border twin | What it stresses |
|---|---|---|
| Little Girl (`½`), Mox Lotus (`{∞}`) | Tarmogoyf (`*/1+*`), X | §2.12 numeric tower |
| Magical Hacker | Magical Hack | Layer 3 (§5) |
| Enter the Dungeon, The Countdown Is at One | Shahrazad | 729 subgames |
| Who // What // When // Where // Why | Every modal / split / DFC card | §2.11 layout arity |

These aren't flexes — they're canaries wearing funny hats, and they're *sharper* than their black-border twins because un-sets are deliberately designed to attack the rules. A free adversarial test suite written by the people who wrote the rules.

The criterion is **downstream leverage**, not border color — border color is only a proxy, and it breaks on dexterity (Chaos Orb and Falling Star are black border). Ask instead: *does implementing this force a decision that pays off elsewhere?* Little Girl forces the numeric tower and hands you Tarmogoyf. Chaos Orb forces one prompt constructor that nothing else will ever use. It's a leaf. Skip the leaves.

### Reclaimed from XMage's list

Two categories XMage punts that this architecture gets cheaply.

**Artist / expansion symbol matters** — in scope, via §2.8. XMage is class-per-card; there is nowhere to put an artist. That's a symptom of *their* data model, not an inherent difficulty. Artist is a string, expansion symbol is a rarity enum, and "Our Market Research Shows That Players Like Really Long Card Names…" needs `length`. Roughly two-thirds of the category is free. Only art-*content* stays out.

**Graveyard order** — in scope, via `OrderGraveyard` (§2.1) and elided by §2.9. Bounded and closed: only 21 cards ever cared, the last being Volrath's Shapeshifter in Stronghold, and Rosewater rates it a 10 on the Storm Scale. It cannot grow on you.

Note rule **404.3**: when multiple cards hit a graveyard simultaneously, the owner *chooses the order*. Cast Wrath of God and you arrange everything that died however you like, then Wrath goes on top (a resolving sorcery is the last thing to arrive). So graveyard order isn't a list — it's a `Prompt`, fired on every mass removal spell. Which is exactly why §2.9 matters.

---

## 7. Risk register

| Risk | Mitigation |
|---|---|
| **Fusing the halves** | The §1 invariant. Audit for `case effect of` in the rules core. Watch the axis Argentum fused on: the mana subsystem grew 26 + 11 `is AddMana*Effect ->` branches because `Effect` carried no `manaProduced()` classification. **For every question the core must answer about an effect *before* executing it (produces mana? redirects a zone change?), add an explicit ABI classification — or the core will grow the switch.** |
| **Trial application unproven in all prior art** | This is the M3 go/no-go and the single hardest bet in the design. **No studied engine actually does it.** Argentum *documented* full 613.8 trial application and shipped a hardcoded whitelist (`EffectSorter.dependsOn`, ~2 `Modification` subtypes; else → timestamp) — it couldn't even represent "Blood Moon removes Urborg's ability." Do not trust any doc, blog post, or `CLAUDE.md` claiming otherwise. **Mitigation: build the dependency test set — Blood Moon + Urborg, Humility + Opalescence, Magical Hack (layer 3) — *before* the resolver, and make the incompleteness a failing test, never a doc footnote.** The edge is real: immutable + memoized state makes the re-projection step of trial application cheaper for pawl than for any copy-on-write engine — this is the unclaimed territory the substrate is built for. |
| **DSL churn** | Expected. Version the AST from commit one. |
| **Effects that reference prior choices and payments** | "If {B} was spent to cast this," "for each," modal back-references, X. mtg-pure hit this wall in a first-order model and punted (its cost-continuation warning, `prior-art-lessons.md` §10.1). Mitigation: named binding slots in the DSL, decided at M3 alongside the first opcodes (D4 in `prior-art-lessons.md` §3); M1a's mana-unit provenance is the closed-half half of the answer. |
| **Thunk leaks** | Strict fields, `Data.IntMap.Strict` keyed by entity ID. A 100-turn game must not build a thunk chain the size of the match. Exception: the projected state should be a lazy field, shared per state — that's a genuine laziness win. Profile the goldfish loop at M0. |
| **Action enumeration underestimated** | Generating the legal action set (priority, targeting legality, X values, modal choices, trigger ordering, alternate cost payments) is roughly as much work as resolution. Budget for it as a peer of the resolver, not a helper. |
| **Simulation throughput** | Target thousands of games/sec. For reference, MageZero aspires to ~1,000 games/**hour** on XMage. The bar is low. |
| **Scope creep into card count** | M0–M3 have a combined card count under ~20. Resist adding cards to feel progress. |

---

## 8. Reference material

- **Comprehensive Rules** — Yawgatog's hyperlinked version is the practical one to work from
- **XMage, "List of cards that will not be implemented"** — `github.com/magefree/mage/wiki` — the best taxonomy of engine failure modes in existence
- **Forge, "Missing Cards in Forge"** + `res/cardsfolder` — reference semantics; check the license
- **MTGJSON** — `AtomicCards` for the static shell and the round-trip oracle. Note: `power` is a *string* (`1+*`). Nothing in MTGJSON is executable.
- **Argentum** (`github.com/wingedsheep/argentum-engine`) — the Kotlin prior art; the project that spurred this one. Base/projected split: **confirmed in code.** Trial-application dependency resolution: **documented, never built** — the real resolver (`EffectSorter.dependsOn`) is a hardcoded whitelist of ~2 `Modification` subtypes; everything else falls to timestamp, while the docs and `CLAUDE.md` still describe the trial application it doesn't do. A *cautionary* datapoint on 613.8, not a positive one. No AI, small pool. MIT — portable with attribution. Full study: `docs/prior-art-lessons.md`.
- **mtg-pure** (`github.com/thomaseding/mtg-pure`, BSD-3) — the other Haskell engine, and the opposite fork on both central bets: cards as a type-indexed EDSL compiled as modules (~5,200 LOC of type-level object plumbing plus a mandatory codegen step), choices as IO callbacks (no snapshot/resume — Mindslaver is a `-- TODO`). Stalled after six months of real work, at exactly the layers/replacement/subgames boundary M3 probes. Study: `prior-art-lessons.md` §10.1.
- **MedeaMelana's Magic** (`github.com/MedeaMelana/Magic`, BSD-3) — the design point one step short of pawl: free-monad `Interact` prompts and `Layer1..7e` as data, but card effects are `Contextual (Magic ())` closures. Source of the effect≡event pipeline M3 should adopt. Study: `prior-art-lessons.md` §10.2.
- **Churchill, Biderman & Herrick**, *Magic: The Gathering is Turing Complete*

---

## 9. The one-paragraph version

Build a pure Haskell VM whose closed half is the comprehensive rules and whose open half is a first-order, non-recursive effect DSL loaded as runtime data. Model every decision — including shuffling — as a suspension in a free monad, so one engine serves humans, bots, replays, and WASM. Prove the ABI at ~12 opcodes against Magical Hack, Humility/Opalescence, Panglacial Wurm and Mindslaver before writing card #13. Get to a complete game with zero cards, then French vanilla with zero opcodes, then let rule 701 hand you the instruction set. Test the closed half against numbered rules, the cards against MTGJSON round-trips, and accept that the open half will need hand-written scenario tests forever. Keep a compiled-in escape hatch for the three dozen cards that deserve it, and watch its size as a health metric.
