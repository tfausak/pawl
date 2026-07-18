# M3a effects as data — design

Design for milestone **M3a**, the first letter of M3 (see the split table in
`docs/design.md`): the effect DSL core, targeting, and instant speed. The gate
card is **Lightning Bolt** (`{R}` Instant, "Lightning Bolt deals 3 damage to any
target," Scryfall-verified) — the first card whose rules text is **data**, the
first spell that targets, and the first thing castable at instant speed. The
falsifier is Bolt-vs-Bolt: CR 608.2b re-checks target legality at resolution, so
any implementation that resolves against targets stored as settled facts passes
its own happy-path test and is wrong.

This milestone also discharges `git-bug 15de615` (setup/matchup agreement by
construction) and decides two of the three ABI questions design.md assigns to
M3: the effect≡event *shape* (adopted here; the full pipeline is M3f's) and D4's
binding slots (targets are the first slots; the mechanism is the decision, the
generalization stays prose).

This is a types-and-architecture spec, not an implementation plan.

## Goal and scope

**Exit criterion.** A deterministic test demonstrates the CR 608.2b fizzle: Bolt
A targets a creature, Bolt B resolves first and kills it, A does not resolve and
goes to the graveyard having done nothing. Lightning Bolt joins `redDeck`, and
random red-red games cast it under random play with conservation, termination,
and "life never increases" all holding; some seed casts a Bolt. The
`DecisionLog` replays deterministically with target choices in it.

**Non-goals.** No Sorcery card type (no card needs it; it is a one-constructor
sibling of Instant when one does). No counterspells, no X, no modes, no
multi-slot card, no targeting restrictions (protection, hexproof, and shroud do
not exist — every creature and player is targetable). No general `Event` type
and no 614/603 machinery (M3f). No activated abilities (M3e). No card files or
serialization: printings stay Haskell-defined pure data **values**, as since
M1a — the AST is serializable and rewritable *in principle*, which is what M3d's
Magical Hack actually tests; a wire format and its version field wait for their
first real consumer (design.md §7's "version the AST from commit one" attaches
to the serialized form, which this milestone deliberately does not create).

## 1. The AST: three new types, two new Card fields

**`Pawl.Type.Effect`** — the ISA. First-order, non-recursive, no functions in
any field, per design.md §1:

```haskell
data Effect
  = DealDamage SlotName Quantity
  deriving (Eq, Ord, Show)
```

One constructor, on purpose: the milestone proves the seam, M3b–M4 add volume.
`Quantity` is M1a's numeric shape, exercised beyond P/T for the first time
(`Literal 3`).

**`Pawl.Type.SlotName`** — `newtype SlotName = MkSlotName Text`, the name of a
binding slot. This is D4's **named binding slots**, both halves: an effect leaf
never contains a target, it *references a slot by name* that casting filled in
— and a hygiene test (§8) statically checks every reference resolves, the
Argentum AST-dataflow lint from `prior-art-lessons.md` §5 ("every read has a
writer"). Nothing anywhere aligns by position: the alternative — slot indices
into a parallel spec list — puts three structures (specs, references, choices)
in silent positional agreement, and a misauthored card no-ops instead of
failing. Names make the agreement checkable; the lint checks it. Payments,
modes, and X introduce names into the same namespace in later milestones.
The type is deliberately `SlotName`, not `TargetSlot`: targets are the first
binding slots, not the last.

**`Pawl.Type.TargetSpec`** — what a slot may legally hold. Classification data,
never a predicate function:

```haskell
data TargetSpec
  = AnyTarget
  deriving (Eq, Ord, Show)
```

`AnyTarget` is Bolt's "any target": a creature or a player (planeswalkers and
battles grow it when those card types exist).

**`Card`** grows `effects :: [Effect]` and
`targetSpecs :: Map SlotName TargetSpec`. The effect list is *ordered* because
execution order is real (CR 608.2c: follow the instructions in the order
written); the spec collection is a `Map` because slots have no order, only
names — the two structures share nothing positionally, and arity is unfixed
from day one (design.md §2.11) without a parallel list to misalign. Every
existing printing gets an empty list and an empty map. CR 601.2c settles a
question this shape raises: the same
object or player **may** be chosen once for *each* instance of the word
"target," and may **not** be chosen twice for *one* instance — so per-slot
choices are independent at M3a (each named slot is one instance choosing one
target), and a within-one-instance distinctness check first bites when a slot
can hold multiple targets, not when a card has multiple slots.

**`CardType`** grows `Instant`. `Card.isPermanentType` answers `False` for it
(CR 110.1 lists the permanent types; Instant is not among them) and
`Card.isInstant` is the timing classification, a type-line read shaped exactly
like `isPermanent`.

**`Recipient.ToDefender` is renamed `ToPlayer`.** Bolt targeting a player is
the second consumer of the type; "defender" was combat's name for it, not the
type's meaning. Mechanical rename, all call sites; combat reads fine
("assign to `ToPlayer` defender").

## 2. The executor and the invariant

New logic module **`Pawl.Resolve`** — the *single legitimate home* of
`case effect of`. It is the opcode-semantics quarter of the VM (design.md §1),
the same standing `Stack.resolveTop`'s invariant comment gives type-line
dispatch. The rules core (`Stack`, `Engine`, `Sba`, `Cast`) never matches an
`Effect` constructor; every question it must answer *before* executing one is a
total classification function, added the moment the core first asks it (the
Argentum mana-leak lesson, `prior-art-lessons.md` §2): at M3a that is
`Card.isInstant` for timing and "does this spell target," which is
`targetSpecs` being non-empty. `-Weverything`'s incomplete-pattern warning is
the executor-coverage hygiene test, at compile time: a constructor without a
`Resolve` case fails the build.

`Resolve.resolveEffects` executes a resolving spell's effect list in order.
`DealDamage slot qty`: evaluate `qty` (`Quantity.evaluate`; an unevaluable
quantity is a no-op, the `powerOf` posture), read the recipient the named slot
was filled with (a `Map.lookup`, kept total: an unfilled slot is a skip,
though the §8 lint makes one unrepresentable in a well-formed printing), skip
it if that target is now illegal (CR 608.2b's partial
resolution: illegal targets are unaffected, other parts still happen), and
apply through the damage funnel (§4).

New logic module **`Pawl.Target`** — targeting legality, shared by casting and
resolution: `legalRecipients :: TargetSpec -> GameState -> Set Recipient`
(for `AnyTarget`: creatures on the battlefield plus players still playing) and
`stillLegal :: Recipient -> TargetSpec -> GameState -> Bool` (CR 608.2b: a
creature target must still be on the battlefield — a zone change made it a new
object per CR 400.7, so its old id is simply absent; a player target must still
be playing).

## 3. Casting: targets, instant speed, and the prompt

**Timing** (`Cast`): an instant is castable any time its controller has
priority (CR 117.1a / 304.1) — for instants, `castable` drops the
`sorcerySpeed` gate and keeps hand, affordability, and the new targeting gate;
everything else keeps `sorcerySpeed` unchanged. Priority is implicit as before:
the engine only offers actions to the priority holder. This makes every
priority window real — responses under a non-empty stack, casting in the
opponent's turn — and is the milestone's riskiest change, which is why Bolt
joins the random decks (§6).

**The targeting gate** (CR 601.2c): a spell with a slot whose legal-recipient
set is empty cannot be cast; `castable` requires every slot be fillable.
Unobservable for Bolt — `AnyTarget` always holds a living player — so it is
written as the rule and first *falsified* by Giant Growth at M3b, which targets
only creatures. Recorded here so M3b asserts it rather than rediscovers it.

**The prompt**:

```haskell
ChooseTargets :: Decider -> PlayerId -> ObjectId -> Map SlotName (Set Recipient) -> Prompt (Map SlotName Recipient)
```

The `ObjectId` is the spell being cast (for interpreter display); the outer
`Map` is keyed by slot name — slots have no order, and the question and answer
agree by *name*, never by position. The inner collection is honestly a `Set`:
legal recipients are unordered and duplicate-free (the `Deck`-as-multiset
argument, M2d §2). `Response` grows `ChoseTargets (Map SlotName Recipient)`;
interpreters iterate the map in its `Ord` order, which is deterministic.

**Order within `castSpell`**: compute legal sets, prompt, validate — the
answer's keys are exactly the spec's keys and each choice is a member of its
slot's set — then pay, then move to
the stack, then stamp the choices on the **new** stack incarnation. An illegal
answer makes the whole cast a no-op (reject-not-repair, the
`AssignCombatDamage` posture; nothing moved, nothing paid). Note this order is
*closer* to CR 601.2 than M1a's, not further: targets are announced before
costs are paid (601.2c before 601.2f–h). What remains elided is the rewind:
`legalActions` still offers only affordable, fully-fillable casts, so a legal
answer cannot fail after the prompt. That elision expires when casting can fail
mid-announcement (kicker-style choices, or M3g's cast-during-search).

**Chosen targets are object state.** `Object` grows
`targets :: Map SlotName Recipient` (empty for everything but spells on the
stack), **per-incarnation** state reset
by `changeZone` exactly like `damage` and `sickness` — CR 400.7 forgets a
spell's targets for free when it leaves the stack.

## 4. Resolution: the fizzle, the funnel, and the mid-loop SBA

**`Stack.resolveTop`** grows the real non-permanent branch (its M1a comment
promised one): for a spell with targets, first CR 608.2b — re-validate every
slot via `Target.stillLegal`; if the spell has at least one slot and **all**
are now illegal, it does not resolve: it moves to the graveyard with no effects
applied (the fizzle). Otherwise `Resolve.resolveEffects` runs (illegal slots
skipped per 608.2b), and the spell goes to its owner's graveyard as the final
part of resolution (CR 608.2n). Permanent spells are untouched.

**The damage funnel generalizes.** `Damage.applyCombatDamage` is already
recipient-generic (mark creatures, drain players, emit into
`GameState.damageEvents`); it is renamed **`Damage.applyDamage`** and becomes
the single way *any* damage happens — combat's two waves and `DealDamage`
alike. The effect≡event shape lands here: applying a `DealDamage` **is**
constructing `DamageEvent`s (source = the spell's object id, target = the
slot's recipient, amount = the evaluated quantity) and funneling them — the
applied effect and the emitted event are the same value, MedeaMelana's `Did`
pattern in one-variant form. No multi-variant `Event` type yet: its first
reader is M3f's trigger scan, and designing it readerless is the mistake M2
declined with protection. The funnel is also where CR 614's rewrite step hooks
at M3f — today that slot is the identity, structurally: there is exactly one
place to insert it.

A correctness bonus recorded now: CR 704.5h reads damage "dealt … by a source
with deathtouch" with no combat qualifier, and `Sba.woundedByDeathtouch`
already reads the funnel's events — so noncombat deathtouch damage is
automatically right the day a deathtouch source can deal it (nothing at M3a
can; no test possible yet).

**The SBA check moves into the priority loop.** `priorityLoop` must run
`checkSba` after `Stack.resolveTop`, before anyone receives priority (CR 117.5:
state-based actions are performed each time a player *would* get priority; CR
704.3). Today nothing dies mid-step, so the omission is unobservable; after
Bolt, a creature dies mid-step and the next player must see it dead — the
fizzle test depends on this ordering (Bolt B's kill must be buried before Bolt
A checks its target). M1b's single-pass SBA loop remains sound: a Bolt death
still cannot cause another state-based action (no triggers, no life changes
from deaths); the loop's position is now load-bearing, its depth still is not.

## 5. Setup: `runMatch` (git-bug `15de615`)

`Setup.newGame` runs inside `Game`, so the initial state must come from
`Setup.emptyGame` at the `runGame` boundary — which is exactly where the
players/matchup agreement can today be violated (`emptyGame` takes
`NonEmpty PlayerId`, `playFrom` takes the matchup; nothing checks they agree).
The fix is the bug's combinator suggestion:

```haskell
Engine.runMatch :: Monad m => (forall r. Prompt r -> m r) -> NonEmpty (PlayerId, Deck) -> m (Result, GameState)
runMatch answer matchup = runGame answer (Setup.emptyGame (fmap fst matchup)) (playFrom matchup)
```

The matchup is passed **once**; the player list is derived, so a matchup player
without a `Player` record is unrepresentable at this seam. The benchmark's game
runners, the full-game integration tests, and the random-game property runner
all migrate to `runMatch`. `Setup.emptyGame` stays public as the **deckless
fixture door** — fixtures place objects directly, so no deck agreement exists
there to violate. This lands first (its own commit), before any effect work,
and closes `15de615`.

## 6. The card and the deck

**`Card.lightningBoltPrinting`**: `{R}` Instant, no P/T, no keywords,
`effects = [DealDamage (MkSlotName "target") (Literal 3)]`,
`targetSpecs = Map.singleton (MkSlotName "target") AnyTarget`. The slot name is
the card author's to choose; the lint only checks agreement. `Card` also gains
**`allPrintings :: [Printing]`**, the registry the lint (§8) and any future
golden-snapshot test iterate — a printing not in the registry escapes the
hygiene net, so the registry's completeness is itself asserted by the existing
deck-composition tests referencing printings through it. Scryfall-verified; per design.md §4's rulings
discipline, the plan's card step pulls Bolt's Gatherer rulings and transcribes
any Q&A-shaped ones (expected: few to none — redirection rulings died with the
planeswalker redirection rule).

**`redDeck` recomposes** to 36 Mountain / 12 Goblin Piker / 8 Bird Maiden /
4 Lightning Bolt — still 36 land + 24 spells = 60, so `objectCount` stays 120
and conservation is untouched. Maiden stays at 8 (flying keeps its random
coverage); four Bolts are enough for random games to cast them without
starving combat, so the "combat happens" and green-black engagement guards
keep their seeds. `greenDeck` and `blackDeck` are unchanged.

## 7. Deciders and replay

The test-suite's random answer function answers `ChooseTargets` by a uniform
independent choice from each slot's set; `Replay` answers it from the recorded
`ChoseTargets`. The determinism property is unchanged in statement and now
covers target choices. The random chooser needs no cleverness — a Bolt at a
random legal target is exactly the fuzz the new priority windows want.

## 8. Testing approach

Deterministic, each named by rule number, cards by real name:

- **Cast and kill through the stack**: Bolt targets a Piker, resolves, 3 damage
  marks it, CR 704.5g buries it; Bolt is in the graveyard (CR 608.2n).
- **Damage to a player** (CR 120.3a): Bolt at a player drops their life by 3;
  no marking.
- **Instant speed** (CR 117.1a): Bolt cast during the opponent's turn, and in
  response to a spell on the stack (CR 117.3c kept priority casts both).
- **The falsifier — CR 608.2b fizzle**: Piker on the battlefield; its opponent
  casts Bolt A targeting it, keeps priority, casts Bolt B targeting it. B
  resolves first, the mid-loop SBA buries the Piker, then A's only target is
  illegal: A moves to the graveyard, no damage anywhere, defender's life
  unchanged. This exercises §4's SBA ordering *and* the fizzle in one test.
- **CR 508.8 with an instant in hand**: an attacker-less combat offers no
  priority window in the dropped declare-blockers/combat-damage steps (the
  M2b drop, now observable because a Bolt *could* have been cast there), while
  the beginning-of-combat and end-of-combat steps still offer it.
- **Cast legality**: a Bolt is not offered without `{R}` available; not offered
  from the graveyard; `Play` never offers it (it is not a land).
- **The slot dataflow lint** (D4's "every read has a writer,"
  `prior-art-lessons.md` §3/§5): for every printing in `Card.allPrintings`,
  the set of slot names referenced by its effects **equals** the key set of its
  `targetSpecs` — no dangling reference, no unused slot. A misauthored card is
  a failing test, not a silent no-op. (Equality, not subset: a spec no effect
  reads is a card announcing a target it ignores — representable in Magic but
  not in this pool; loosen to ⊇ if such a card ever lands.)

Properties (`runMatch`, both matchups): all M2d properties as they stand —
conservation at 120, termination, ids, no floating mana, life never increases
(a Bolt only lowers a life total), combat happens, green-black engagement — and
one new guard: **some red-red seed casts a Bolt** (the engagement pattern, so
instants cannot silently never fire while the suite stays green). Replay
determinism runs as before.

The benchmark stays on `redDeck` and now includes Bolts; throughput is watched,
not asserted — the new prompt volume (instant-speed action enumeration in every
priority window) is the thing to notice.

## 9. What M3a preserves

- **The two invariants.** `Pawl.Resolve` is the executor, not the rules core;
  no other module matches an `Effect` constructor. No prompt is elided that
  isn't forced (target choice is always prompted, even when the legal set is a
  singleton — a forced choice among one is still the player's, and eliding it
  would change the `DecisionLog` shape per legal-set size; cheap, honest,
  uniform).
- **M1a's pay-first elision**, narrowed and restated (§3): offers are
  pre-validated, so nothing rewinds; expiry moves to mid-announcement failure
  (kicker-style choices, M3g's cast-during-search).
- **`Damage` semantics**: `legalAssignment`, `blockerThreshold`,
  `attackerAssignment` untouched; `applyDamage` is a rename plus a second
  caller, not a behavior change. The `attackerAssignment` chooser expiry
  (banding/Mindslaver, M2c §4) is unchanged and still points at M3g.
- **`keywordsOf`/`controllerOf` seams**: untouched; their M3 expiries belong to
  M3b and M3g respectively.
- **Mono-color decks**: Bolt is red in the red deck; `payCost`'s source elision
  is untouched.

## 10. Expiries this milestone opens

- **`TargetSpec` = `AnyTarget` only**: grows a constructor per new targeting
  shape (M3b: creature-only, for Giant Growth).
- **`Target.legalRecipients` knows no restrictions**: protection, hexproof,
  shroud, and "can't be targeted" all modify this one function later; today
  everything is targetable, and that is a fact about the card pool, not an
  elision.
- **The funnel's identity rewrite slot**: CR 614 hooks into `applyDamage`'s
  proposal at M3f; until then applying is proposing.
- **`damageEvents` as the only event stream**: the general `Event` type arrives
  with its first reader (M3f's triggers); the funnel discipline (every
  observable mutation through one change-and-emit helper) is what M3b–M3e must
  maintain so M3f is a widening, not a chase.
- **Effect list execution is strictly ordered, no interleaved choices**: fine
  while effects prompt for nothing during resolution; expires when a resolving
  effect asks (M3g's search, modal effects).

## 11. Explicitly deferred past M3a

- **Sorcery** (trivial sibling of Instant, waits for a card), **counterspells**,
  **X costs**, **modes**, **multi-target slots** (601.2c's
  within-one-instance distinctness first bites there), **planeswalkers/battles
  in `Recipient`**.
- **Continuous effects, layers, durations** — M3b (Giant Growth is *not* in
  M3a precisely because "+3/+3 until end of turn" is a continuous effect).
- **The 614/603 pipeline and the general `Event` type** — M3f.
- **Activated abilities** — M3e. **Decider routing and re-entrant casting** —
  M3g.
- **Card serialization, AST version field, golden snapshots** — with the first
  serialized consumer (M3d's rewriting may want goldens; M6 wants the loader).
