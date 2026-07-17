# M2a keywords — design

Design for milestone **M2a**: the keyword seam, and the five keywords that prove
it. This is the first slice of `docs/design.md`'s **M2** ("French vanilla"),
which is split into **M2a** (the seam + blocking/attacking legality), **M2b**
(first strike + the conditional turn structure) and **M2c** (deathtouch +
trample). See `docs/design.md` for the split and the full 702 triage.

M1b made creatures fight. M2a is where they start being *different from each
other* — without a single opcode.

This is a types-and-architecture spec, not an implementation plan.

## M2a goal and scope

The same game — 36 Mountain / 24 Goblin Piker per player, **still zero opcodes**
— plus a second creature that carries keywords, so that the seam has something to
be tested against. Rule 702 is comprehensive rules, not card text: `K:Flying` is
a **citation**, and the closed half is allowed to know what it cites.

Five keywords, chosen because each is a *different shape*, not because each is
easy:

| Keyword | CR | Shape |
|---|---|---|
| Flying | 702.9b | a **relation** between blocker and attacker |
| Reach | 702.17b | the same relation, from the other side |
| Defender | 702.3b | a **unary** attack legality |
| Vigilance | 702.20b | a **modifier on an action** (CR 508.1f's tap) |
| Haste | 702.10b | an **override of existing state** (M1b's `Sickness`) |

All five are a few lines each. Individually they prove nothing; together they are
the minimum that tests the *seam* rather than one instance of it.

**Reach is why flying is not enough.** CR 702.9b: a creature with flying "can't
be blocked except by creatures with flying and/or reach." Flying alone can be
satisfied by a check that asks "does the blocker have flying?" — which passes its
own test and is wrong. Reach falsifies it immediately: a reach creature blocks a
flyer without having flying. A milestone whose only keyword is flying cannot
distinguish the right implementation from the wrong one.

**Vigilance and haste are why the seam cannot be only a query.** Flying, reach
and defender are all questions the rules core asks before permitting something.
Vigilance is not: it changes what declaring an attacker *does*. Haste is not
either: it overrides a field M1b already stores. A seam designed against flying
alone plausibly handles neither.

**Exit criterion:** a creature that cannot be blocked by a ground creature, a
creature that cannot attack at all, and a creature that attacks without tapping
all coexist in one game — and `legalAttackers`/`canBlock` never ask which card
anything is.

## Conventions

Inherits M0's, M1a's and M1b's (`Mk` prefix, boot libraries only, `Eq`/`Show`
everywhere, shape-now-cases-later, every elision names the milestone that kills
it). No additions.

## 1. Why casing on a keyword is not a violation

The project's central invariant is that the closed half depends on a
*classification* of effects and **never** on the *identity* of an effect — the
rules core must never `case effect of DealDamage{} -> …`.

`case keyword of Flying -> …` looks like exactly that, and is not. **A keyword is
not an effect; it is a numbered rule.** Rule 702 is part of the comprehensive
rules, the same as rule 506 (the combat phase) or rule 302 (creatures). Casing on
`Keyword` is the same kind of act as casing on `Phase`, which the engine already
does everywhere and must.

This section exists because the resemblance is close enough that someone — quite
possibly the author, three milestones from now — will read `case keyword of` as a
violation and "fix" it into a classification, destroying the thing. The test to
apply is the one the design doc gives: **is it in the rulebook?** Flying is
702.9. Goblin Piker is not in the rulebook. The line falls between them, not
between "concrete" and "abstract".

The open half never gains a `Keyword`. When M4's effect DSL says *target creature
gains flying until end of turn*, the effect is `GrantKeyword Flying` — an opcode
carrying a citation as **data**. The closed half reads the citation; it still
never reads the opcode.

## 2. The seam

```hs
-- Pawl.Type.Keyword — CR 702. Ordered by rule number, not by arrival.
data Keyword
  = Defender   -- 702.3
  | Flying     -- 702.9
  | Haste      -- 702.10
  | Reach      -- 702.17
  | Vigilance  -- 702.20
```

Five constructors, because five have consumers. M2b inserts `FirstStrike`
(702.7) and `DoubleStrike` (702.4); M2c inserts `Deathtouch` (702.2) and
`Trample` (702.19) — each in rule-number order, in the milestone that uses it. A
constructor with no consumer is dead weight, not a placeholder, and the project
has already made this call twice: M1b did not pre-declare `AttackTarget`'s
planeswalker case, and did not add a `controller` field before anything could
change control.

The ordering convention is worth stating because it is not the arrival order and
will look arbitrary otherwise: `Keyword`'s constructors read in CR order, so the
type is diffable against rule 702 itself.

```hs
-- Pawl.Type.Card gains:
keywords :: Set Keyword
```

A `Set`, because CR 702.9c and 702.3c say multiple instances are **redundant**.
That is a per-keyword fact and not a general one: two instances of Ward both
trigger, and Rampage stacks. It is true of all five here, and of every keyword in
M2b and M2c. **EXPIRES** when the first non-redundant keyword lands (§6).

### The query is a function, not a field read

```hs
-- Pawl.Game
-- The keywords an object currently has (CR 702).
--
-- EXPIRES at M3: layer 6 grants and removes abilities, at which point this stops
-- being a card read and consults the layer system. Everything that needs a
-- keyword calls this and never Card.keywords, so that change is one function
-- rather than every call site.
keywordsOf :: ObjectId -> GameState -> Set Keyword

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
```

This is `controllerOf`'s situation exactly, and it is settled the same way and
for the same reason. Today `keywordsOf` is provably `Card.keywords` of the
object's printing — nothing in M2a grants or removes an ability. Reading the
field directly from `Combat` would compile, pass every test, and be correct until
the exact moment Magical Hack and Humility arrive, at which point it would be
wrong in a dozen call sites at once.

M1a did this with `Mana.manaSources`; M1b did it with `Game.controllerOf`. The
fix should be local when it comes.

### Keywords take parameters, and none of M2a's do

`Keyword` is a sum type whose constructors happen to all be nullary today. It
must not be an enum that has to be reshaped the moment a parameterized keyword
lands, and that is not far off — the M2a punchlist alone contains
`Landwalk Subtype` (CR 702.14; pawl already has Mountains, so Mountainwalk is
immediately real) and intimidate, which reads colors. Protection is
`Protection Quality` and Ward is `Ward Cost`.

This is the project's shape-now-cases-later convention. No parameterized
constructor is written in M2a, because none has a consumer; the *shape* is a
`data` with room for them.

## 3. What each keyword touches

Four of the five are a clause inside a function `Pawl.Combat` already has.
Flying and reach are the exception, and they are the reason this section is long:
they land on CR 509.1's structure, and getting that structure wrong is how M2a
would paint the engine into a corner for menace.

No prompt changes. `Pawl.Type.Prompt`, `Pawl.Type.Response`, `Pawl.Replay` and
all eight interpreters are untouched — M2a is additive across the decision seam,
unlike M1b's Task 5.

**Flying and reach — CR 702.9b, 702.17b.** Evasion is **not** a filter on who may
block. CR 509.1b is explicit about what it is:

> The defending player checks each creature they control to see whether it's
> affected by any **restrictions** (effects that say a creature can't block, or
> that it can't block unless some condition is met). If any restrictions are being
> disobeyed, **the declaration of blockers is illegal**.
>
> A restriction may be created by an **evasion ability** (a static ability an
> attacking creature has that restricts what can block it). … Different evasion
> abilities are **cumulative**.

So the rules validate the **whole declaration**, and the unit of legality is the
declaration, not the pair. This distinction decides whether the milestone paints
the engine into a corner, because the very next keyword on the punchlist does not
fit the pair-shaped version: **menace** (702.111) says a creature "can't be
blocked except by two or more creatures" — a constraint on the *set* blocking one
attacker, which no per-pair predicate can express. CR 509.1c's own example is
menace. Landwalk reads the defending player's lands; protection reads a quality.
Only flying and reach are pairwise, and designing to them would be designing to
the one case that misleads.

Therefore the split follows the rules' own seam:

```hs
-- Pawl.Combat

-- CR 509.1a: about the blocker ALONE -- untapped, controlled by the defending
-- player, a creature. Unchanged from M1b. These creatures can block SOMETHING;
-- whether a given declaration is legal is a different question.
canBlock :: PlayerId -> ObjectId -> GameState -> Bool

-- CR 509.1b: restrictions, checked over the declaration AS A WHOLE. Evasion
-- lives here. A conjunction of independent restriction checks, because 509.1b
-- says different evasion abilities are cumulative -- flying and shadow together
-- admit only blockers that have both.
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
```

**`Prompt.DeclareBlockers` does not change.** It already offers the 509.1a
candidates and the attackers, and already returns a whole
`Map ObjectId ObjectId` — which is exactly the rules' unit of legality. The flat
candidate list stays honest: those creatures really can block *something*. An
interpreter that wants per-attacker legality can call `legalBlockDeclaration`
itself; the library has no export lists, and the engine must validate the answer
regardless of what the interpreter believed.

**An illegal declaration is rejected whole, and no creature blocks.** M1b already
settled this exact question for CR 510.1e and the argument transfers without
modification:

> CR 510.1e checks the assignment AS A WHOLE, so this cannot be repaired by
> filtering the way a discard can.

Filtering is not merely inelegant here, it is *unsound*: drop one blocker from a
menace pair and the survivor is an illegal single block, so the filter would
manufacture the illegality it was meant to remove. M1b's `declareBlockers`
filters per-pair today (`Map.filterWithKey legal chosen`), which is correct only
because pairwise legality is currently the whole of 509.1b. **M2a replaces that
filter with whole-declaration validation** — the one behavioral change to
existing code, and the reason it is worth making now rather than at menace.

As with 510.1e, this is not CR 733's rewind: an enforcing engine never offers the
illegal declaration, so only a broken interpreter arrives here, and re-prompting a
pure `Prompt r -> r` returns the identical wrong answer. Declining to block is
always legal today, so "no blocks" is a legal state to fall back to — see §6 for
the milestone that stops making that true.

Note the asymmetry the rules state explicitly and that is easy to get backwards:
702.9b's second sentence says "a creature with flying can block a creature with or
without flying." Flying restricts *being blocked*, never *blocking*.

**Defender — CR 702.3b.** One clause in `canAttack`. It is the only keyword here
that needs nothing but a `hasKeyword` call, which is why it is worth having: it
proves the attack-side query exists at all.

**Vigilance — CR 702.20b.** "Attacking doesn't cause creatures with vigilance to
tap." Not a legality check — `declareAttackers` still declares it as an attacker;
it simply does not apply CR 508.1f's tap to it. The keyword modifies an *action*.

**Haste — CR 702.10b.** "It can attack even if it hasn't been controlled by its
controller continuously since their most recent turn began." `canAttack`'s
`Object.sickness == Settled` check becomes `Settled || hasKeyword Haste`.

Note 702.10c: haste also frees activated abilities with the tap symbol from CR
302.6. There are no activated abilities yet, so it has no consumer. It is not
implemented — unlike CR 704.5f in M1b, omitting it makes nothing partial, and it
would be a branch nothing can reach and no test can falsify.

## 4. The test cards

The seam cannot be tested with Mountains and Pikers: a Piker has no keywords, and
a fixture that hand-sets `Card.keywords` on a Piker would be testing the field
rather than a card. Each keyword needs a real printing that carries it.

**A printing is not a deck-list entry, and the two decisions are separate.** All
five keywords get a printing, because a printing is data in `Pawl.Card` and costs
nothing; only **one** joins the deck list, because that is what changes the shape
of a random game and gives a deck-composition bug somewhere to hide.

Every card below is mono-red, castable from the existing 36 Mountains with no
mana changes, and genuinely vanilla-plus-one-keyword — each **verified against
Scryfall** (`api.scryfall.com/cards/named?exact=…`), not recalled. The dumps in
`_scratch/` are other projects' working data and are fine for *finding*
candidates, but never for confirming one:

| Card | Cost | P/T | Type | Rules text |
|---|---|---|---|---|
| **Bird Maiden** | `{2}{R}` | 1/2 | Creature — Human Bird | Flying |
| **Nimble Birdsticker** | `{2}{R}` | 2/3 | Creature — Goblin | Reach |
| **Ogre Sentry** | `{1}{R}` | 3/3 | Creature — Ogre Warrior | Defender |
| **Windseeker Centaur** | `{1}{R}{R}` | 2/2 | Creature — Centaur | Vigilance |
| **Goblin Chariot** | `{2}{R}` | 2/2 | Creature — Goblin Warrior | Haste |

Notes on the choices, since each was constrained:

- **There is no vanilla red flier at `{1}{R}`.** Bird Maiden is the cheapest that
  exists. Its 1/2 body is better than a 2/2 would be: it is distinguishable from
  a Piker's 2/1 by P/T alone, so a test can tell them apart without consulting
  keywords — and a Bird Maiden and a Piker *trade* when they meet, which the CR
  510.2 machinery already handles.
- **Ogre Sentry is a 3/3**, which is the point. A defender that dies to
  everything would let "defender can still block" pass vacuously.
- **Windseeker Centaur over Yotian Soldier** (`{3}`, 1/4, also vigilance): the
  Soldier is an *artifact* creature, which would drag in the artifact card type
  and colorless casting — a new axis, in the milestone that is supposed to be
  proving one thing.
- **Nimble Birdsticker is a Goblin with reach**, which is faintly ridiculous and
  entirely real.

**Bird Maiden is the one that joins the deck**, because flying is the only one of
the five whose effect is visible across a whole random game, and the "fliers get
through" property (§5) needs it in there. It **replaces** Pikers rather than
adding to the deck: the list stays at 60, so conservation stays at 120 objects
and M1b's property is unchanged rather than retuned.

`Pawl.Type.Subtype` gains `Human`, `Bird`, `Ogre` and `Centaur` (it has
`Mountain`, `Goblin` and `Warrior` today). Enum growth, no shape change.

## 5. Testing approach

Following M0, M1a and M1b: one growing `testTree` in `source/test-suite/Main.hs`,
favoring property-based and integration-style tests that assert `GameState` after
actions.

**Rule-numbered scenarios**, matching the `ruleTests` convention:

- *CR 702.9b* — a declaration in which a ground creature blocks a flier is
  illegal.
- *CR 702.9b* — a flier may block a ground creature. (The asymmetry. This is the
  test that fails if flying is implemented as a symmetric predicate.)
- *CR 702.17b* — a reach creature may block a flier. **The falsifier.** It fails
  against any implementation that asks "does the blocker have flying?"
- *CR 702.9b* — a flier may block a flier.
- *CR 509.1a* — a ground creature is still offered as a legal blocker while a
  flier attacks. It can block *something*; 509.1a is about the blocker alone.
  This is the test that fails if evasion is wrongly implemented as a filter on
  the candidate list.
- *CR 509.1b* — **an illegal declaration is rejected whole, not repaired.** The
  interpreter answers with one legal block (a reach creature on the flier) *and*
  one illegal one (a Piker on the flier); **neither** creature ends up blocking.
  This is the test that pins reject-versus-filter, and it is worth having now
  rather than at menace: it is the only M2a test that fails against M1b's
  surviving `Map.filterWithKey`, which would drop the Piker and let the reach
  block stand.
- *CR 702.3b* — a creature with defender is not offered as a legal attacker.
- *CR 702.3b* — a creature with defender may still block.
- *CR 702.20b* — a creature with vigilance is declared as an attacker and is
  **not** tapped; a creature without it, in the same declaration, **is**. (One
  test, both creatures, so that a blanket "nothing taps" bug cannot pass.)
- *CR 702.10b* — a creature with haste attacks the turn it arrives.
- *CR 702.10b* — the same creature without haste cannot. (The control. M1b's
  sickness test already covers the negative, but not with haste in the type.)

**The classification test, which is the point of the milestone:** a Mountain, a
Piker and a flier all pass through `canBlock` and `canAttack`, and no call site
in `Pawl.Combat` mentions a card name. This is asserted the way M1b asserted it —
by the Mountain-with-damage-marked test — i.e. by a case the identity-based
implementation gets wrong.

**Properties.** All six of M1b's survive unchanged and must keep passing:
conservation (120 objects), termination, ids minted ≥ 120, no mana floats, life
never increases, and combat happens. **M2a retires no property** — it is the
first milestone since M0 that does not, which is itself a signal that the seam is
additive.

Conservation is the one to watch: it asserts the constant 120, so Bird Maiden
must *replace* Pikers rather than join them (§4). If that constant needs editing,
something is wrong with the deck change, not with the property.

**New:**

- **fliers get through** — across seeds, a game exists in which a flier deals
  combat damage to a player while the defender controlled an untapped ground
  creature. Without this, flying could be implemented as "can never be blocked"
  or "can never block" and every scenario above would still pass.

## 6. Expiries

Every elision in M2a, and the milestone that kills it. Inherits M1b's table,
which is unchanged and still owed.

| Elision | Why it is legitimate now | Killed by |
|---|---|---|
| `keywordsOf` reads `Card.keywords` | Nothing grants or removes an ability | **M3** — layer 6 |
| `Keyword` has only nullary constructors | None of M2a's five take a parameter | **Punchlist** — `Landwalk Subtype` |
| `Set Keyword` assumes redundancy | True of every keyword through M2c | Ward, Rampage (**tail**) |
| CR 702.10c (haste and tap abilities) not implemented | No activated abilities exist | **M4** |
| CR 509.1c **requirements** not implemented | Nothing says a creature must block | **Punchlist/M4** — see below |
| CR 509.1b's "gains or loses evasion after a legal block has been declared" | Nothing can change abilities mid-combat | **M3** — layer 6 |

### The two that matter for not painting ourselves in

**CR 509.1c requirements are the harder half of 509.1, and M2a implements none of
it.** Restrictions ("can't block") are a predicate over a declaration.
Requirements ("must block if able") are not:

> If the number of requirements that are being obeyed is fewer than the **maximum
> possible number** of requirements that could be obeyed without disobeying any
> restrictions, the declaration of blockers is illegal.

That is a combinatorial optimization over the space of legal declarations, not a
check — the engine must know the maximum achievable before it can judge the
answer it was given. Nothing in M2a or its punchlist creates a requirement, so
there is nothing to obey and the maximum is trivially zero. `legalBlockDeclaration`
is named for restrictions deliberately, so that requirements arrive as a *second*
function rather than as a surprise inside this one.

**Requirements also invalidate M2a's fallback**, which is why it is flagged here
rather than discovered later. Rejecting an illegal declaration yields "no blocks",
which is legal *only* while nothing says a creature must block. Once a requirement
exists, "no blocks" can itself be illegal, and the engine will not be able to fall
back to it. That is a real design problem and it belongs to the milestone that
introduces the first requirement, not to this one.

**Banding needs no restriction machinery, and needs three other things.** Worth
recording now so the shape is not mistaken for a corner:

- CR 702.22c declares **bands** at declare-attackers — a *grouping* of attackers
  that `Combat.attackers :: Map ObjectId AttackTarget` has no room for. A new
  field on the `Combat` record, which is what that record exists to absorb.
- CR 702.22h **propagates** blocks: "if an attacking creature becomes blocked by a
  creature, each other creature in the same band … becomes blocked by that same
  blocking creature." So the stored blockers are not the declared blockers.
  `Map ObjectId (Set ObjectId)` already accommodates that — propagation only adds
  entries — which is one more reason the M1b decision to key blockers per attacker
  and store a `Set` was right.
- CR 702.22j/k **invert the chooser**. That is the `Decider` problem (§7 of
  `docs/design.md`), and it is why banding cannot land before M3.

Banding adds no entry to 509.1b. It is not a blocking restriction, and no part of
M2a needs to anticipate it beyond leaving the `Combat` record extensible, which it
is.

## 7. Known deviation this milestone does not fix

**CR 508.8 is violated on `main` today** (`git-bug 5f50eec`). `Turn.grantsPriority`
returns `True` for the declare blockers and combat damage steps unconditionally,
so on every turn where nobody attacks, pawl grants two priority rounds that CR
508.8 says to skip — and CR 500.11 defines skipping as "proceed past it as though
it didn't exist."

It is unobservable in game state at M2a for the same reason it is unobservable at
M1b: nothing can be cast at instant speed, so the extra windows can only be
passed. It already makes the `DecisionLog` diverge from a faithful engine.

**It is fixed in M2b, not here.** CR 506.1 states 508.8 and first strike's second
damage step as one requirement — conditional turn structure — and they want one
mechanism, not two. Fixing it in M2a would mean building that mechanism with only
one consumer and no way to prove it generalizes.

## Explicitly deferred past M2a

- **First strike, double strike** — M2b, with the CR 506.1 turn restructure.
- **Deathtouch, trample** — M2c, with their CR 702.2c interaction.
- **The axis-1 punchlist** — menace (702.111), intimidate (702.13), landwalk
  (702.14). Pure volume once the seam exists; landwalk brings the first
  parameterized `Keyword`.
- **Protection (702.16), hexproof (702.11), shroud (702.18), ward (702.21),
  enchant (702.5), equip (702.6), flash (702.8)** — blocked on machinery that
  does not exist: CR 615 prevention, targeting, `Attach`, instant-speed timing.
  Seven of the evergreen twenty. Not a judgment call.
- **The 702.22+ tail** — 174 entries, led by banding, whose 702.22j hands damage
  assignment to the *defending* player and is therefore a `Decider` problem
  (M3+), not a combat problem.
- **Granting and removing keywords** — that is layer 6, and it is M3.
