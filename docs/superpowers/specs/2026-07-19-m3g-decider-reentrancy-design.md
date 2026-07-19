# M3g the payoff pair (Decider + re-entrancy) — design

Design for milestone **M3g**, the seventh and final letter of M3 (see the split
table in `docs/design.md`): **the payoff pair — controlling another player
(CR 723, Mindslaver) and re-entrancy (casting during resolution, Panglacial
Wurm).** This letter is not the M3 go/no-go — that verdict arrived at the end of
M3d. It **validates two seams prior art already proves work**: Argentum's
`actorFor` (route decisions to a controller while resources stay put) and
ygopro's suspension protocol at scale (a spell cast while another is mid-flight).
The whole point of M3g is that both are *cheap here because the substrate was
built for them* — the `Decider` field has ridden every prompt since day one
(§2.3), and choices are suspensions in a free monad (§2.1), so nesting a cast
inside a resolution is a function call, not an architecture.

The gate cards, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-19):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Mindslaver** | `{6}` | Legendary Artifact | — | "{4}, {T}, Sacrifice Mindslaver: You control target player during that player's next turn. (You see all cards that player could see and make all decisions for them.)" |
| **Panglacial Wurm** | `{5}{G}{G}` | Creature — Wurm | 9/5 | "Trample / While you're searching your library, you may cast this card from your library." |

Both are chosen because each **falsifies its own naive implementation**:

- **Mindslaver** is the `Decider ≠ PlayerId` gate. The naive engine fuses the two
  — it routes the controlled player's *resources* to the controller along with the
  decisions (Argentum's `actorFor` bug). CR 723.3 is explicit that this is wrong:
  "Only control of the player changes. All objects are controlled by their normal
  controllers." So the falsifier is a scenario where the controller makes a
  decision the controlled player never would, using the controlled player's own
  card, mana, graveyard, and life.
- **Panglacial Wurm** is the re-entrancy gate. The naive engine resolves a spell
  or ability atomically — run it start to finish, *then* return to the game loop.
  Panglacial is cast *during* another object's resolution (CR 605.3a permits mana
  activation mid-resolution; the card's own permission grants the cast), so an
  atomic resolver can never interleave it.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the phased spine

M3g cashes two independent architectural bets that the substrate has been
carrying, unexercised, since M0. Neither adds a rules subsystem the size of M3b's
projection or M3f's event pipeline; both are **the day-one investment paying
off**.

| Axis | Mechanism | Gate |
|---|---|---|
| **Decider (CR 723)** | A turn-scheduled control store; `deciderFor` — already consulted at every genuine decision prompt — reads it, so the controller answers the controlled player's prompts while every resource stays the controlled player's | Mindslaver |
| **Re-entrancy** | `Cast.castSpell` invoked from *inside* `Resolve.applyEffect (Effect.Search …)`; the suspension model makes the nested cast (with its own targets and mana payment) a plain monadic call | Panglacial Wurm |

**The phased spine** (the M3c–M3f structure — land the lighter, understood axis
first, keep the risk isolated):

1. **Mindslaver / Decider (Phase 1).** `TargetSpec.PlayerTarget`, the
   `Effect.ControlPlayerNextTurn` opcode, the `pendingControl` / `activeControl`
   store on `GameState`, the promotion seam in `handoffTurn`, `Decide.deciderFor`
   reading the store, and the mana half of activation costs
   (`AbilityCost.mana`, forced by Mindslaver's `{4}`). Gate: **Alice Mindslavers
   Bob; on Bob's next turn Alice makes Bob's decisions while Bob's resources move.**
2. **Panglacial / re-entrancy (Phase 2).** The `CastingPermission` classification,
   `Card.castingPermissions`, the `Prompt.CastWhileSearching` loop, and the one new
   step in `Effect.Search` (offer the cast *before* the find, per the ruling).
   Gate: **cast Panglacial from the library during Evolving Wilds' search; the
   search continues; the Wurm resolves to a 9/5 trampler afterward.**

M3g stays **one milestone, one spec, one plan**: the phases are ordered commits;
the plan owns the decomposition. Phase 1 and Phase 2 are independent (they share
no type), so the plan may interleave their tasks only if it keeps each commit
complete.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Decider routing (Phase 1, CR 723.5).** Alice activates Mindslaver (paying
  `{4}`, tapping and sacrificing it) targeting Bob. On Bob's **next** turn, every
  decision prompt aimed at Bob carries `Decider = Alice`, and the scripted
  interpreter's Alice-branch makes Bob take an action his own branch never would
  (cast Bob's Lightning Bolt at Bob). The falsifier: an engine that ignores control
  answers with Bob's own branch, and the action never happens.
- **Resources stay put (Phase 1, CR 723.3 / 723.5a).** The Bolt Alice made Bob cast
  is *Bob's* card, paid with *Bob's* mana, resolving to *Bob's* graveyard, and it is
  *Bob's* life that changes. Bob remains the active player and the owner/controller
  of his objects; only the *decision* routed to Alice.
- **Scheduling and expiry (Phase 1, CR 723.1 / 723.1a).** Before Mindslaver is
  activated and after the controlled turn ends, `deciderFor Bob` is Bob; it is Alice
  only during that one turn. Control is installed as *pending* when the ability
  resolves and promoted to *active* at the actual start of Bob's turn
  (CR 723.1b — robust to skipped turns by construction, though none exist yet); it
  auto-expires when the following turn begins.
- **Re-entrant cast (Phase 2, the rulings + CR 605.3a).** With Panglacial Wurm in
  the library, activating Evolving Wilds and, *during* its search resolution,
  casting Panglacial (`{5}{G}{G}`, tapping Forests mid-resolution) leaves the Wurm
  on the stack; the search then continues (finds a basic land, shuffles) and the
  ability ceases; the active player receives priority **with the Wurm on the
  stack**; it resolves to a 9/5 trampler on the battlefield. Panglacial has left the
  library and so cannot be found by the search.
- **Negative control (Phase 2).** Declining the cast opportunity resolves Evolving
  Wilds' search normally, with Panglacial still in the library.

The `DecisionLog` replays deterministically with the Mindslaver activation and its
target choice, the routed decisions of the controlled turn, and the
cast-while-searching path (the new `CastWhileSearching` prompt and the nested
cast's target/mana prompts).

**Non-goals.**

- **No information filtering (CR 723.4).** "You see all cards that player could
  see" is a `PlayerView` / hidden-information concern. pawl has no hidden-info
  projection yet — deterministic tests answer prompts with full information
  regardless of who decides — so 723.4 is a no-op today and named as an expiry
  (§7), due with the PlayerView work (M7).
- **No limited-duration control (CR 723.2).** Word of Command and Opposition Agent
  control a player for a bounded window, not a whole scheduled turn. A different
  duration shape; deferred (§7).
- **No control restrictions or compulsions (CR 723.7), no self-control (CR
  723.9).** Mindslaver neither restricts nor forces the controlled player's actions,
  and targets an opponent in the fixture. Both deferred (§7).
- **No legend rule (CR 704.5j).** Mindslaver is Legendary, but the SBA fires only
  when one player controls two or more same-name legendaries. The fixture keeps
  Mindslaver singleton — indeed it is sacrificed as an activation cost, so it is
  never even on the battlefield after use — so the SBA has nothing to act on.
  `Supertype.Legendary` (M3c) represents the type line faithfully; the SBA is a
  named expiry (§7) that **must land suppressible** (Mirror Gallery, "the legend
  rule doesn't apply") — a classification a static ability can switch off, never a
  hardcoded check. Building it now would risk baking in the non-suppressible shape.
- **No CR 723.1a overwrite gate test.** The `Map.insert` in
  `ControlPlayerNextTurn` overwrites a prior pending control (last created wins) by
  construction, but exercising it needs *two* control sources targeting one player.
  Only one Mindslaver exists in the fixture; the overwrite is asserted structurally
  and its scenario deferred (§7).
- **No re-entrant casting beyond Panglacial's permission.** `CastFromLibraryWhile
  Searching` is Panglacial's exact shape. Cast-from-top-of-library (Garruk's Horde,
  Melek), cast-from-exile, and other CR 601.3 permissions are deferred (§7).
  Multiple simultaneous searchers (CR 701.23i, APNAP find order) — single searcher
  only.
- **No CR 733 rewind.** Casting mid-search stays inside the existing
  reject-not-repair elision (`Cast.castSpell`'s comment names cast-during-search as
  its expiry): the cast opportunities are pre-filtered to affordable and fillable,
  so a legal answer cannot fail after the prompt and no action reversal is needed.
- **No tokens, X, modes, counters, new keywords, Auras/Equipment (Attach), or
  serialization/AST version field.** Untouched from M3f. Panglacial's Trample is
  M2c; its 9/5 body needs no opcode.

## 1. New and grown types

### Phase 1 — Mindslaver / Decider

**`Pawl.Type.TargetSpec`** grows one target restriction:

```haskell
  | PlayerTarget  -- CR 115: "target player" -- a player still in the game.
```

`Target.legalRecipients` gains the arm `PlayerTarget -> Set.fromList players`,
where `players = map Recipient.ToPlayer (Sba.stillPlaying gs)` (already computed
for `AnyTarget`). `Recipient.ToPlayer` already exists; `PlayerTarget` is the
players-only restriction `AnyTarget` does not express.

**`Pawl.Type.Effect`** grows one opcode:

```haskell
  | ControlPlayerNextTurn SlotName  -- NEW (Mindslaver)
```

On resolution it reads the slot's chosen recipient (a `ToPlayer target`) and
installs pending control of `target` by the ability's **controller**
(`Object.owner` of the stack ability object — the activator, not the sacrificed
source permanent). `slotsOf` returns `Set.singleton slot`; `manaProduced` returns
`Nothing`; `rewriteEffect` is identity (no land-type word). Casing on this arm is
`Resolve`'s charter.

**`Pawl.Type.AbilityCost`** gains the mana half (the field its own comment names):

```haskell
data AbilityCost = MkAbilityCost
  { mana :: Maybe ManaCost,          -- NEW: Mindslaver's {4}. Nothing = free.
    additional :: [AdditionalCost]
  }
  deriving (Eq, Ord, Show)
```

Promoted from `newtype` to `data`. `Activate.activatable` gains a
`maybe True (\c -> Mana.canPay pid c gs) (AbilityCost.mana cost)` conjunct;
`activateAbility` pays the mana (CR 602.1b, via `Mana.payCost`) alongside the
additional costs. Every M3e ability has `mana = Nothing` and is unaffected.

**`Pawl.Type.GameState`** grows the control store:

```haskell
  -- CR 723.1: a player-controlling effect installs a PENDING control keyed to the
  -- player to be controlled; it is promoted to activeControl at the actual start
  -- of that player's next turn (CR 723.1b). Map.insert overwrites: last created
  -- wins (CR 723.1a).
  pendingControl :: Map PlayerId Decider,
  -- CR 723.1/723.3: the decider controlling the ACTIVE player this turn, if any.
  -- A single Maybe suffices because a controlled player is always the active
  -- player during their controlled turn (CR 723.3). Overwritten every turn start,
  -- so control auto-expires at the beginning of the next turn (CR 723.1's "the
  -- effect doesn't end until the beginning of the next turn").
  activeControl :: Maybe Decider
```

`Setup` initializes both empty (`Map.empty` / `Nothing`).

### Phase 2 — Panglacial / re-entrancy

**`Pawl.Type.CastingPermission`** (new) — a card's permission to be cast from a
zone or under a condition it normally could not (CR 601.3's "a rule or effect
allows that player to cast it"), classified by the permission pattern, the M3f
`TriggerCondition` shape:

```haskell
-- CR 113.6 / 601.3: a static ability, functioning while the card is in the
-- library, that permits casting it from the library during a search of that
-- library. Panglacial Wurm = [CastFromLibraryWhileSearching]. A general
-- "cast from the top of your library" (Garruk's Horde) is a future permission.
data CastingPermission = CastFromLibraryWhileSearching
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Card`** grows one field, empty for all but the gate:

```haskell
  -- CR 601.3: this card's casting permissions -- zone/condition exceptions to
  -- normal timing. Read by a classifier (Cast.permitsCastWhileSearching), never
  -- by card identity. Read DIRECTLY from the card, not the projection: the
  -- permission functions in the library (CR 113.6), and the CR 613 layer system
  -- projects only permanents on the battlefield, so there is nothing to project
  -- and nothing (Humility) that can reach a library card.
  castingPermissions :: [CastingPermission]
```

**`Pawl.Type.Prompt`** grows the re-entrant cast offer:

```haskell
  -- The re-entrant cast opportunity during a library search (Panglacial Wurm).
  -- The [ObjectId] is the searcher's library cards castable-while-searching
  -- (permitted, affordable, fillable -- the engine pre-filters to legal choices).
  -- Nothing = decline / done. Prompted in a loop before the find (per the ruling:
  -- casting must occur before any cards are found), so multiple copies may be
  -- cast (also per the ruling). CR 605.3a permits mana activation to pay.
  CastWhileSearching :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

**`Pawl.Type.Response`** grows the replay variant for `CastWhileSearching` (a
`Maybe ObjectId`), mirroring `Searched`.

**`Pawl.Card`** gains **Mindslaver** (`{6}`, Legendary Artifact; one activated
ability, cost `mana = Just {4}` + `[TapSelf, SacrificeSelf]`, effect
`[ControlPlayerNextTurn slot]`, one `PlayerTarget` slot) and **Panglacial Wurm**
(`{5}{G}{G}`, Creature — Wurm, 9/5, `Keyword.Trample`, `castingPermissions =
[CastFromLibraryWhileSearching]`). Both are deterministic fixtures, out of the
random decks.

## 2. Phase 1 — Mindslaver and the Decider (CR 723)

**The claim.** The `Decider` field has been threaded through every genuine
decision prompt since day one — `ChooseAction`, `ChooseDiscard`,
`DeclareAttackers`/`DeclareBlockers`, `AssignCombatDamage`, `ChooseTargets`,
`ChooseBasicLandTypes`, `SearchLibrary`, and (Phase 2) `CastWhileSearching` — via
`Decide.deciderFor`. (The two `Shuffle` prompts carry no decider, correctly:
randomness is not a player decision, §2.2.) So controlling a player is a
**data-model and scheduling** change with no plumbing: teach `deciderFor` to read
the control store, and every prompt routes for free. This is the day-one `Decider`
investment (§2.3) cashing out.

**The opcode installs pending control.** `Effect.ControlPlayerNextTurn slot`, at
resolution, reads the chosen `ToPlayer target` for `slot` and sets
`pendingControl := Map.insert target (MkDecider controller) pendingControl`, where
`controller` is the activator (the `controller` argument `applyEffect` already
carries — `Object.owner` of the ability's stack object). It does **not** read the
source permanent, so it is untouched by the `b998924` source-by-id-LKI issue and by
the fact that Mindslaver has been sacrificed as a cost before it resolves. An
illegal target (the player has left the game) is re-judged at CR 608.2b and the
ability fizzles (the shared `resolveEffects` path).

**The scheduling seam is one place — `handoffTurn`.** It is the sole actual-turn-
start (CR 723.1b — "the next turn that the affected player actually takes"). It
gains:

```haskell
GameState.activeControl = Map.lookup newActive (GameState.pendingControl gs),
GameState.pendingControl = Map.delete newActive (GameState.pendingControl gs)
```

alongside the existing `activePlayer` / `turnNumber` / `phase` / `remaining`
updates. Because `activeControl` is overwritten every turn (to the next active
player's pending entry, or `Nothing`), control **auto-expires at the beginning of
the next turn** — exactly CR 723.1. Skipped turns fall out for free (a skipped
player is never `newActive`, so their pending entry waits); pawl has no turn-skips
yet, so this is correct-by-construction and named. The first turn is set up by
`Setup.newGame`, before any Mindslaver can have resolved, so only `handoffTurn`
needs the promotion.

**`Decide.deciderFor` is the sole reader:**

```haskell
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid gs = case GameState.activeControl gs of
  Just decider | pid == GameState.activePlayer gs -> decider
  _ -> Decider.MkDecider pid
```

The active-player guard is what makes a single `Maybe` correct: only the active
player is ever controlled during their turn (CR 723.3), so a controlled player's
prompts route to the controller while every other player (e.g. Alice declaring
blockers on her own account during Bob's turn) still decides for themselves
(CR 723.8).

**Phase 1 gate:** Alice activates Mindslaver targeting Bob; the ability resolves
and installs pending control; on Bob's next turn `activeControl` promotes; the
scripted interpreter's Alice-branch drives Bob's decisions while Bob's resources
move; control expires at the following turn.

## 3. Phase 2 — Panglacial Wurm and re-entrancy (casting during resolution)

**The claim.** Resolution is not atomic. `resolveSpell` / `resolveEffects` /
`applyEffect` are already `Game`-monadic (M3e made them so for `Search`), and
`Cast.castSpell` is `Game`-monadic, so calling `castSpell` from *inside*
`applyEffect (Effect.Search …)` is an ordinary nested call. The suspension model
carries the nested cast's own prompts (targets, mana) in stride; an atomic
resolver (mtg-pure's IO callbacks; Argentum) cannot.

**`castSpell` is already zone-generic.** It moves the object with
`Event.changeZone oid Zone.Stack`, which reads the object's *current* zone — so
casting from the library needs no change to the move itself. Only the *enumeration*
is hand-specific (`castable` / `castableSpells` hardcode `Zone.Hand`) and the
*timing gate* would reject a mid-search cast (`timingOk` demands sorcery speed with
an empty stack). Both are bypassed by a dedicated enumerator, because the
permission **is** the CR 601.3 timing exception ("follows all normal rules… except
for timing," per the ruling).

**The enumerator and classifier** (in `Pawl.Cast`):

```haskell
-- CR 601.3: the library cards this player may cast while searching their own
-- library -- permitted, affordable, and with a fillable target set. Deliberately
-- omits timingOk: the permission is the timing exception. Casing on
-- CastingPermission is a classification, never card identity.
permitsCastWhileSearching :: Card -> Bool
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
```

`castableWhileSearching pid gs` filters the player's library to cards whose
`castingPermissions` include `CastFromLibraryWhileSearching`, then keeps those that
are affordable (`Mana.canPay`) and fillable (`Cast.targetable` — Panglacial has no
slots, so trivially true).

**The one new step in `Effect.Search`,** before the find (per the ruling: "you must
do so before you find any cards"):

```
Effect.Search crit:
  castWhileSearching controller        -- NEW: the re-entrant loop
  matches <- filter (matchesCriterion crit) library
  found   <- prompt (SearchLibrary … matches)
  putTapped found; shuffle             -- unchanged
```

`castWhileSearching controller` loops: compute `castableWhileSearching controller
gs`; if empty, stop; else prompt `CastWhileSearching decider controller options`;
on `Nothing`, stop; on `Just oid`, call `Cast.castSpell controller oid` (the
re-entrant call — moves library→stack, prompts and pays mana via CR 605.3a's
mid-resolution allowance) and repeat. The loop terminates because each cast removes
a card from the library (fewer options) and a `Nothing` ends it; multiple copies
may be cast while mana lasts (per the ruling).

**Why the elisions hold.** Pre-filtering to affordable + fillable means a legal
`CastWhileSearching` answer cannot fail after the prompt, so the reject-not-repair
posture stands and no CR 733 rewind is pulled in (the `castSpell` comment's named
expiry — honored by pre-filtering, not built). Mana payment stays inside the
`65ce714` source elision by fixturing indistinguishable Forests.

**The sequence (from the rulings).** The searching ability (Evolving Wilds) is
mid-resolution when the Wurm is cast, so the Wurm lands on the stack **above** it;
the search then finishes resolving (find, shuffle) and the ability ceases
(CR 608.2n); the CR 117.5 boundary runs; the active player receives priority with
the Wurm on the stack; on the next resolution it enters the battlefield as a 9/5
trampler. The Wurm has left the library, so the search's find cannot see it, and
the shuffle does not touch it.

**Phase 2 gate:** activate Evolving Wilds; cast Panglacial during the search;
assert the interleave and the eventual resolution; and the negative control
(decline → normal search, Wurm stays in library).

## 4. Invariants preserved

- **The two-halves invariant holds.** No closed-half module cases on a card's or an
  effect's *identity*. `Resolve` remains the sole home of `case effect of` (now
  including `ControlPlayerNextTurn`); the closed half *asks* classifications — does
  this target spec admit players? does this card permit casting while searching? —
  and never names Mindslaver or Panglacial. `ControlPlayerNextTurn` installs a
  decider keyed on a chosen player; the Search loop reads
  `permitsCastWhileSearching`; `deciderFor` reads a store. Identity appears nowhere.
- **The engine makes the right choices and elides only indistinguishable ones.**
  Mindslaver's target is prompted (`ChooseTargets` with `PlayerTarget`); the
  controlled player's every decision is prompted through the controller
  (CR 723.5); the re-entrant cast is prompted (`CastWhileSearching`) with its own
  target and mana prompts. The `65ce714` mana-source elision is held within by
  fixturing indistinguishable lands; CR 723.1a overwrite, CR 723.4 visibility, and
  the legend rule are genuinely absent from the fixtures (not silently elided) and
  named as expiries.
- **Conventions.** One type per module (`CastingPermission` new; `PlayerTarget`,
  `ControlPlayerNextTurn`, `CastFromLibraryWhileSearching`, `CastWhileSearching`
  are new constructors on existing types); `NamedFieldPuns` per the M3b amendment;
  new sum-type constructors take no `Mk`-pun; `AbilityCost` keeps its
  `MkAbilityCost` record constructor as it grows from `newtype` to `data`.

## 5. Setup, decks, and testing

Testing follows the phased spine (§0), deterministic fixtures only (the white/blue
posture of M3a–M3f; Mindslaver is colorless, Panglacial green; the random-game
coverage tail is the post-M3 work, §7).

**Phase 1 — Mindslaver.** A two-player fixture (Alice, Bob). The scripted
interpreter answers `ChooseAction (MkDecider d) player actions` by branching on
`d`: an Alice-strategy and a Bob-strategy that differ observably.

- **Routing + resources.** Alice controls a Mindslaver (untapped, settled) with
  `{4}` available; she activates it targeting Bob. It resolves; `pendingControl`
  holds `Bob ↦ Alice`. On Bob's next turn, assert: (1) `activeControl == Just
  (MkDecider Alice)` and `deciderFor Bob gs == MkDecider Alice`; (2) the interpreter's
  Alice-branch makes Bob cast Bob's Lightning Bolt at Bob, and after resolution the
  Bolt is in **Bob's** graveyard and **Bob's** life is 17 (paid from Bob's mana);
  (3) `deciderFor Alice gs == MkDecider Alice` throughout (Alice is not controlled).
  Falsifier in a comment: an engine that never routes leaves Bob on his own branch
  (passes), so no Bolt is cast. Cite CR 723.3 / 723.5 / 723.5a.
- **Scheduling + expiry.** Assert `deciderFor Bob` is Bob before the activation and
  again on the turn *after* the controlled one (control auto-expired at that turn's
  start). Cite CR 723.1.
- **Legend rule elided.** Mindslaver is singleton and sacrificed as a cost; assert
  no second copy is ever manufactured. (No 704.5j SBA exists to assert against.)

**Phase 2 — Panglacial.** A green fixture: the searcher controls an Evolving Wilds
(untapped), enough Forests to pay `{5}{G}{G}` (indistinguishable sources), and has
Panglacial Wurm in the library.

- **Re-entrant cast.** Activate Evolving Wilds; its ability resolves; the
  interpreter's `CastWhileSearching` answer casts Panglacial. Assert: after the
  search resolves, Panglacial is on the stack (not on the battlefield yet) and is
  **not** in the library; the Forests paying for it are tapped; then it resolves and
  a 9/5 with Trample is on the battlefield under the searcher's control. Cite the
  rulings and CR 605.3a / 608.2n.
- **Negative control.** Same setup; the `CastWhileSearching` answer is `Nothing`.
  Assert Evolving Wilds' search completes normally (a basic land fetched, library
  shuffled) and Panglacial remains in the library.
- **Multiple (per the ruling), if cheap.** With enough Forests for two, cast two
  Panglacials in one search; assert both reach the stack.

**Setup and decks.** `emptyGame` unchanged. `Setup` initializes `pendingControl =
Map.empty`, `activeControl = Nothing`. Mindslaver and Panglacial are deterministic
fixtures assigned timestamps from `freshTimestamp` as M3c–M3f do, **out of the
random decks**. Lightning Bolt (M3a) supplies the controlled-turn action; Evolving
Wilds (M3e) supplies the search.

**Properties** (`runMatch`, both matchups): every M2d/M3a–M3f invariant as it
stands — conservation, termination, ids, no floating mana at end of step, life
never increases (unchanged — the Mindslaver scenario only *decreases* Bob's life),
combat happens, green-black engagement. Replay determinism now covers the
Mindslaver activation and target, the routed decisions of the controlled turn, and
the cast-while-searching path. The benchmark stays on `redDeck`; throughput is
unaffected (the control store is a `Map` lookup per turn start; the search loop is
gated on the permission).

## 6. The shared executor and payment paths

`ControlPlayerNextTurn` resolves through the existing shared executor
(`Resolve.resolveEffects`, M3f) — Mindslaver's activated ability is placed by
`Activate.activateAbility` (M3e) and resolved like any other, with the source
permanent as the effect source (CR 608.2g), except the effect reads the
*controller* not the source. The mana half of `AbilityCost` reuses `Mana.canPay` /
`Mana.payCost` (the `Cast` payment path), so no new payment machinery is
introduced. The re-entrant cast reuses `Cast.castSpell` verbatim; the only new code
is the enumerator, the classifier, and the loop.

## 7. What M3g preserves, and the expiries it opens

**Preserves:** the two invariants (§4), the numeric/mana/timestamp models, the
M3b–M3f projection shape and source-liveness, the deterministic-fixture posture,
M3e's activation and shared executor, and M3f's event pipeline and CR 117.5 loop.

**Expiries this milestone opens:**

- **Information filtering (CR 723.4) — the headline deferral.** "You see all cards
  that player could see" needs a `PlayerView` that filters hidden information per
  observer, which pawl does not have (all state is visible to the deterministic
  interpreter). Due with the PlayerView / hidden-information work (M7); until then
  control routes decisions with full information, which is observationally correct
  for a full-information interpreter.
- **Limited-duration control (CR 723.2).** Word of Command and Opposition Agent
  control a player for a bounded window rather than a scheduled turn — a different
  duration shape than `pendingControl` / `activeControl`. Due with the first such
  card.
- **Control restrictions and compulsions (CR 723.7); self-control (CR 723.9).**
  Mindslaver neither restricts nor forces actions and targets an opponent. Due with
  a card that does.
- **CR 723.1a overwrite scenario.** The `Map.insert` gives last-created-wins by
  construction, but a gate test needs two control sources on one player. Due with a
  second controlling source (or a non-singleton fixture).
- **The legend rule (CR 704.5j).** Deferred; must land **suppressible** (Mirror
  Gallery, "the legend rule doesn't apply") — a projected classification a static
  ability can switch off, never a hardcoded same-name check. Due when two same-name
  legendaries can coexist.
- **Re-entrant casting generality.** `CastFromLibraryWhileSearching` is Panglacial's
  exact permission. Cast-from-top-of-library (Garruk's Horde, Melek),
  cast-from-exile, and other CR 601.3 permissions generalize `CastingPermission`
  with their first card. Multiple simultaneous searchers (CR 701.23i, APNAP find
  order) are single-searcher-only here.
- **Mid-announcement failure / CR 733.** Held off by pre-filtering the
  `CastWhileSearching` options to affordable + fillable. The general rewind (a cast
  that becomes illegal mid-announcement) is due when an announced action can fail
  after its prompt — the `Cast.castSpell` comment's standing expiry.
- **git-bug `65ce714`** (mana-source prompt) stays open — fixtures use
  indistinguishable Forests for Panglacial and generic-payable lands for
  Mindslaver's `{4}`. Unchanged by M3g.

**Explicitly deferred past M3g (the close of M3):**

- **A dedicated random-game matchup for M3 gate coverage** — the post-M3 tail
  (M2d-style), covering the deterministic fixtures M3a–M3g accreted.
- **M3.5** — cards as data files (the JSON codec and round-trip), the next
  milestone.
- **X, modes, counterspells, Auras/Equipment (Attach), new card types, the AST
  version field, tokens, and the unified event log** — M4+ vocabulary.
