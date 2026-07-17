# M2c deathtouch + trample — design

Design for milestone **M2c**: deathtouch and trample, the third and final slice of
`docs/design.md`'s **M2** ("French vanilla"), after **M2a** (the keyword seam:
flying, reach, defender, vigilance, haste) and **M2b** (first strike, double
strike, and the CR 506.1 conditional turn structure). See `docs/design.md` §M2c
for the brief. This spec discharges it.

M2a made creatures different from each other; M2b made the *turn* differ from one
game to the next. M2c makes **combat damage itself** carry more than a number:
where it came from (deathtouch reads the source) and how it is apportioned
(trample restructures assignment). Deathtouch is rule 702.2; trample is rule
702.19 — both citations, not card text, so this milestone is **still zero
opcodes**.

This is a types-and-architecture spec, not an implementation plan.

## M2c goal and scope

The same game — 36 Mountain / 16 Goblin Piker / 8 Bird Maiden per player, still
zero opcodes — with two keywords added on **two different structural axes** whose
*interaction* is the falsifier:

- **Deathtouch (702.2)** is a **damage-source** axis. It adds a state-based action
  (CR 704.5h) and is the **first reader of a damage event**: the SBA must know
  which creatures were dealt damage *by a source with deathtouch* since the last
  time SBAs were checked — information M1b's `applyCombatDamage` computes and then
  throws away.
- **Trample (702.19)** is a **damage-assignment** axis. A blocked trampler assigns
  lethal to each blocker and then overflows the excess onto what it is attacking.
  It brings lethal-per-blocker back **inside the keyword** (702.19b), where M1b's
  `Type/Combat.hs` note said it belonged, and it falsifies M1b's "a single blocker
  takes all of it, so it is not asked" shortcut.

**The interaction is the point of pairing them.** CR 702.2c: "Any nonzero amount
of combat damage assigned to a creature by a source with deathtouch is considered
to be lethal damage for the purposes of determining if excess damage is being
dealt." That is *precisely* trample's calculation. A trampler that also has
deathtouch needs to assign only **1** to each blocker before the rest tramples
over, where a plain trampler must assign full toughness. The two keywords are not
wired together by special-case code — they meet in one place, the lethal-damage
minimum, and getting that meeting right is what M2c exists to prove.

**Exit criterion.** A 1/1 deathtoucher destroys a 3/3 it deals one damage to
(where a 2/1 without deathtouch leaves the 3/3 alive); a 3/3 trampler blocked by a
2/1 assigns one and deals two to the defending player; and a 3/3 that has *both*
tramples two over a 3/3 blocker where a plain 3/3 trampler tramples nothing — all
without any function asking which card a creature is.

## Conventions

Inherits M0's, M1a's, M1b's, M2a's and M2b's (`Mk` prefix, boot libraries only,
`Eq`/`Show` everywhere, shape-now-cases-later, every elision names the milestone
that kills it, keyword constructors in CR-number order, test names cite rule
numbers, combat fixtures place creatures directly on the battlefield). One
convention **ends** here and the spec says why: **mono-red** (see §6).

## 1. Keyword additions

`Pawl.Type.Keyword` gains two constructors, inserted **by rule number** so the
type stays diffable against rule 702 (M2a's ordering discipline):

```hs
data Keyword
  = Deathtouch   -- 702.2   (new)
  | Defender     -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike  -- 702.7
  | Flying       -- 702.9
  | Haste        -- 702.10
  | Reach        -- 702.17
  | Trample      -- 702.19  (new)
  | Vigilance    -- 702.20
```

Casing on either is closed-half, per the standing note at the top of the module
(a keyword is a numbered rule, not an effect's identity). The reads happen through
`Game.keywordsOf` / `Game.hasKeyword`, M2a's projection — never `Card.keywords`
directly — so M3's layer system localizes to that one function.

## 2. Deathtouch, part one: the damage event and the change-and-emit helper

Deathtouch's SBA (§3) asks a question about the **past**: was this creature dealt
damage by a deathtouch source *since the last SBA check*? M1b's damage path cannot
answer it. `gatherCombatDamage` computes, per participant, a list of `(target,
amount)` pairs and immediately **drops the source**; `applyCombatDamage` then folds
those into marked damage and life loss. The source — the one fact deathtouch needs
— is gone before anything could read it.

So M2c makes the source survive to the point of application, and turns application
into the engine's first **change-and-emit** helper. This is the funnel M2b's §7
deferred here by name ("deathtouch's bit is the first damage-event reader; the
atom/event funnel earns its first consumer there"), and the fork this spec settles
deliberately: **build the event, with a minimal payload, not just a bit.**

### The event

A new one-type module `Pawl.Type.DamageEvent`:

```hs
data DamageEvent = MkDamageEvent
  { source :: ObjectId,    -- the creature that dealt it
    target :: Recipient,   -- the creature or player it was dealt to
    amount :: Natural
  }
```

`Recipient` is the sum introduced in §4 (`ToCreature ObjectId | ToDefender
PlayerId`), shared with trample's assignment so damage to creatures and damage to
players are one type. The payload is deliberately **minimal — exactly what
application already computes today.** It carries no "was the source a deathtouch
source" flag: deathtouch is derived at read time from `keywordsOf source`, the
same projection M2b reads live at a boundary (see §3 and the 702.2e expiry in §8).
`amount` and the full `source`/`target` are more than deathtouch strictly reads
(it needs only "a creature was dealt nonzero damage by a deathtouch source"), but
they are the fields the apply step already has in hand, and the next readers —
lifelink (`amount`, `source`), then M4 combat-damage triggers — grow the payload
rather than reshape it. Designing a richer event now, before a reader constrains
its shape, is the mistake this keeps narrow.

### The helper

`GameState` gains `damageEvents :: [DamageEvent]`. `gatherCombatDamage` returns
`[DamageEvent]` (source-carrying) instead of M1b's source-less `([(ObjectId,
Natural)], [(PlayerId, Natural)])`, and `applyCombatDamage` becomes the **single
choke point** through which combat damage flows: for each event it (a) marks the
damage on the creature or subtracts the life from the player, exactly as M1b did,
**and** (b) appends the event to `GameState.damageEvents`. Change *and* emit, in
one place.

This is the shape M3 generalizes to every observable mutation; M2c builds it for
damage only, which is all that has a reader. The emission is **uniform** — every
combat damage instance produces an event, including damage to players — because a
funnel with a special case is not a funnel; deathtouch simply filters to the
creature-targeted ones.

## 3. Deathtouch, part two: the state-based action (CR 704.5h)

`Pawl.Sba.creatureDies` today checks CR 704.5f (toughness ≤ 0) and 704.5g (marked
damage ≥ toughness). M2c adds the third clause, **CR 704.5h**: a creature with
toughness **greater than 0** that has been dealt damage by a source with deathtouch
since the last SBA check is destroyed.

The predicate reads `GameState.damageEvents`: the creature is destroyed if some
event targets it (`ToCreature oid`) with `amount > 0` and `Game.hasKeyword
Deathtouch (source event)`. Toughness > 0 is required by the rule and is not
redundant — a creature already at toughness ≤ 0 dies by 704.5f, and 704.5h is
specifically the case marked damage (704.5g) would *miss* because one point is not
lethal by the numbers.

**Draining the events is what makes "since the last SBA check" precise.**
`checkStateBasedActions` computes the dying set (from 704.5f/g/h together, against
the state before any of them apply — SBAs are simultaneous), buries them, and then
**clears `damageEvents`**. The window resets at each check. This is well-defined
because an SBA check always follows combat damage (CR 510 / 704.3: a player gets
priority after the turn-based damage action, and SBAs are checked first), so every
event a deal produces is read by the very next check and then discarded — the log
does not grow without bound, and no event is read twice.

**This aligns with M2b's between-steps check for free.** M2b runs `checkSba`
between the first-strike and regular combat damage steps. A first-striking
deathtoucher therefore destroys its blocker in that between-steps check — by
704.5h, exactly as a first striker with high enough power destroys its blocker by
704.5g — and the first wave's events are drained there, so the second wave starts
clean. The drain and the between-steps SBA were built for different reasons and
compose without special handling.

**Still single-pass.** `checkStateBasedActions` remains M1b's one-pass loop; a
deathtouch death causes no further SBA in M2c (nothing gains life or triggers on a
death yet). This inherits M2b's expiry: **M4**'s death triggers make CR 704.3's
"repeat until stable" load-bearing.

## 4. Trample: assignment restructured, through a generalized prompt

A blocked trampler (702.19b) assigns lethal to each blocking creature first, then
"any excess damage is assigned as its controller chooses among those blocking
creatures and the player … the creature is attacking." Two things this breaks in
M1b:

1. **A player becomes a damage recipient during assignment.** M1b's
   `AssignCombatDamage` returns `Map ObjectId Natural` — creatures only.
2. **A single blocker is now a real choice.** M1b treats one blocker as forced
   ("exactly one blocker takes ALL of it … not asked"). A 3/3 trampler blocked by
   one 2/1 may assign 1-and-2, 2-and-1, or 3-and-0 — three legal answers. The
   shortcut is falsified.

Rather than bolt a trample special case onto the prompt, M2c **generalizes the
prompt to be keyword-agnostic**, parameterized on the two things the *rules*
compute: the legal recipients and a per-recipient **lethal minimum**.

### The recipient type

A new one-type module `Pawl.Type.Recipient`:

```hs
data Recipient
  = ToCreature ObjectId   -- a blocking creature
  | ToDefender PlayerId   -- the player being attacked
```

Shared with §2's `DamageEvent`. It grows toward attack targets generally
(planeswalkers, battles) when those exist; `ToDefender PlayerId` is all M2c's
single-opponent, planeswalker-free board can produce.

### The generalized prompt

```hs
AssignCombatDamage
  :: Decider -> PlayerId
  -> ObjectId                  -- the attacker assigning
  -> Map Recipient Natural     -- legal recipients -> minimum each must receive
  -> Natural                   -- power to distribute
  -> Prompt (Map Recipient Natural)
```

The prompt **never mentions trample**. The rules compute its `Map Recipient
Natural` of minimums, and the two cases fall out of that one argument:

- **Non-trample blocked attacker (CR 510.1c):** recipients are the blockers, every
  minimum `0` — free division, exactly M1b. A single blocker is still forced (one
  recipient, minimum 0, total must equal power) and so, as in M1b, **not asked**;
  2+ blockers are asked, as in M1b.
- **Trample (CR 702.19b):** recipients are the blockers, each with minimum =
  **lethal**, *plus* `ToDefender` with minimum `0`. A single blocker with excess
  power now has a non-forced distribution and **is** asked — the falsified
  shortcut, corrected by the same generic "ask only when the answer is not forced"
  test that M1b and the engine-makes-no-choices rule already use.

**Lethal-first needs no ordering rule.** 702.19b's "once all those blocking
creatures are assigned lethal damage, any excess …" is captured entirely by "each
blocker receives ≥ its minimum." The rule that damage may reach the defender only
after every blocker has lethal is exactly "every blocker's minimum is met"; an
answer that feeds the defender while a blocker is short leaves that blocker below
its minimum and is rejected. There is no separate ordering constraint and no
damage-assignment *order* (M1b established that order was removed from the game);
lethal-in-order lives only here, inside the keyword, as a per-recipient floor.

**Lethal accounts for damage already marked (702.19b).** A blocker's minimum is
`max(0, toughness − alreadyMarked)`, not bare toughness. This is load-bearing
against M2b: a blocker damaged in the first-strike step carries that marked damage
into the regular step, lowering the trampler's obligation to it. "Damage from
other creatures being assigned during the same step" (also 702.19b) is inert in
M2c — a blocker blocks exactly one attacker — but the phrasing is noted so the M4
multi-block-of-one-blocker case has a home.

### Validation

`Damage.attackerAssignment` generalizes M1b's `onlyBlockers && totalsPower` to
**`everyRecipientMeetsMinimum && onlyLegalRecipients && totalsPower`**, checked
against the assignment as a whole (CR 510.1e). M1b's **reject-not-repair**
discipline carries over verbatim: an illegal answer yields no assignment (the
rules' own degenerate case), and this is explicitly not the CR 733 human-error
rewind — the comment M1b wrote stays.

**Unblocked and no-blockers-remain.** An unblocked trampler assigns to the
defender as normal (trample "has no effect" until it is blocked). 702.19d — blocked
but no blockers remain at assignment — assigns to the defender "as though all
blocking creatures have been assigned lethal damage"; in M2c nothing removes a
blocker between declaration and damage, so this is unreachable and noted as such,
its expiry being the first effect that removes a blocker mid-combat (M3+).

### The chooser is still the attacker's controller — recorded, not fixed

702.19b says the excess is assigned "as its controller chooses," so M1b's
`Decide.deciderFor (controllerOf attacker)` is **correct** for trample and M2c
keeps it. But it is now the *only* damage-assignment chooser, and two things
falsify "the chooser is always the attacker's controller": **banding** (702.22j —
the *defending* player chooses how the attacking creature's damage is assigned) and
**Mindslaver** (the chooser is a `Decider`, not a `PlayerId`). Per the brief, M2c
**records** this — an expiry in §8 and a comment at the assignment site — and does
not fix it; it is a `Decider` problem for **M3**, not a combat problem.

## 5. The interaction (CR 702.2c) — the falsifier

Deathtouch changes trample's lethal minimum. 702.2c makes any nonzero assignment to
a creature by a deathtouch source count as lethal "for the purposes of determining
if excess damage is being dealt" — which is the exact calculation §4's minimum
performs. So when the trampler's source has deathtouch, each blocker's minimum
collapses from `max(0, toughness − alreadyMarked)` to **1** (or 0 if the blocker
already has lethal marked).

This must **not** be special-case code joining two keywords. The minimum is
computed in one function, and it consults `Game.hasKeyword Deathtouch (the
attacker)` when it computes the floor — the same projection §3's SBA reads. A
trample+deathtouch 3/3 into a 3/3 blocker assigns 1 and tramples 2; a plain
trample 3/3 into the same blocker must assign all 3 and tramples nothing. One data
point on the lethal floor, read through the keyword projection, produces the whole
difference. If the two keywords were implemented as independent branches, this case
is where they would disagree — which is why the milestone pairs them.

## 6. The test cards, and the one convention that ends here

Two printings, **verified against Scryfall**
(`api.scryfall.com/cards/named?exact=…`, checked 2026-07-17; the dumps under
`_scratch/` are other projects' data, fine only for *finding* a candidate), plus
one **synthetic fixture** the engine cannot yet express with a real card.

| Card | Cost | P/T | Type | Rules text | Rulings |
|---|---|---|---|---|---|
| **Typhoid Rats** | `{B}` | 1/1 | Creature — Rat | Deathtouch | 0 |
| **War Mammoth** | `{3}{G}` | 3/3 | Creature — Elephant | Trample | 0 |

Both are genuinely vanilla-plus-one-keyword (their entire behavior is a type line,
cost, P/T and one 702 citation — zero opcodes) and both carry **zero** Gatherer
rulings (french-vanilla keywords' oracle is the CR itself, per M2b's §5).
`Pawl.Type.Subtype` gains `Rat` and `Elephant` (enum growth, no shape change, like
M2a's and M2b's subtypes). Neither joins the deck; both are exercised by fixtures,
placed directly on the battlefield (`combatBoardOf`), as M2b's cards were.

**The P/T are chosen for isolation.** Typhoid Rats is **1/1**: one point of power,
so when it destroys a 3/3 the *only* possible cause is deathtouch (its damage is
not otherwise lethal). War Mammoth is **3/3**: power 3 tramples cleanly over a 2/1
(assign 1, spill 2) and its toughness 3 survives a 2/1 blocker, so the trampler
lives to make the overflow observable rather than trading.

**Mono-red ends here, and it has to.** M2a and M2b kept every card mono-red to
reuse the 36-Mountain mana base and keep other colors' complexity out of a keyword
milestone. M2c cannot: **deathtouch has zero mono-red cards in the entire corpus**
(Scryfall `keyword:deathtouch c=r` → none), and clean vanilla-plus-trample lives in
green, not red. Because the combat fixtures place creatures directly and never cast
them, **cost and color are cosmetic** — they are for faithfulness, not for the
fixtures — so the cards go to their true colors (black deathtouch, green trample)
with no new mana base and no casting test. This note is mono-red's expiry; the
discipline it served (one mana base, no incidental color complexity) is preserved
because nothing is cast.

### The synthetic fixture, and why it is unavoidable

The 702.2c falsifier (§5) requires **one creature with both deathtouch and
trample**. No printed Magic card has both as static abilities — in any color
(Scryfall `keyword:deathtouch keyword:trample` → only *Odric, Blood-Cursed*, which
merely *counts* those abilities, and has neither) — and M2c has no granting effect
to put deathtouch onto a trampler (Basilisk Collar is Equipment, M4; a layer-6
grant is M3). So the falsifier is tested with a **clearly-labeled synthetic
fixture**: a test-local 3/3 carrying `{Deathtouch, Trample}`, defined in
`source/test-suite/Main.hs` (not in the engine's `Pawl.Card` library), with a
comment stating that no printed card combines these and that M2c lacks the granting
that M3 will use to replace it.

The **rule** under test is real and cited; only the fixture is hypothetical. This
is an explicit, temporary departure from the corpus's real-cards discipline,
justified solely by an engine limitation, and it carries an expiry (§8): **M3**
grants deathtouch to a real trampler (War Mammoth under Basilisk Collar, or a
layer-6 grant) and the synthetic fixture is deleted. Real, recognizable cards are
strongly preferred so a Magic player can read a test and understand it; a synthetic
fixture is a labeled crutch, never a resting place.

## 7. Testing approach

One growing `testTree` in `source/test-suite/Main.hs`, property- and
integration-style, asserting `GameState` after actions — the M0–M2b convention.
Deathtouch and trample are only observable *after* combat damage, so, like M2b,
these are driven **through the engine** so the change-and-emit helper, the 704.5h
SBA, the event drain, and the generalized assignment all run.

**Rule-numbered scenarios.** Each names the case a naive or independent
implementation gets wrong:

- *CR 704.5h — deathtouch destroys where numbers do not.* Typhoid Rats (1/1
  deathtouch) attacks; Ogre Sentry (3/3) blocks. The Rat deals 1, and the Ogre is
  destroyed by the between/after SBA despite toughness 3; the Ogre's 3 kills the
  Rat. **The deathtouch falsifier** — one damage is not lethal by 704.5g, so a
  build that reads only marked-vs-toughness leaves the Ogre alive.
- *CR 704.5g — the control: no deathtouch, no destruction.* Goblin Piker (2/1)
  attacks; Ogre Sentry (3/3) blocks. The Ogre takes 2, survives. Isolates
  deathtouch as the sole cause and keeps M1b's marked-damage behavior honest.
- *CR 702.19b — trample spills the excess.* War Mammoth (3/3 trample) attacks;
  Goblin Piker (2/1) blocks. Assign 1 (lethal) to the Piker, 2 to the defending
  player: the Piker dies, `bob` loses 2, the Mammoth survives on 2 marked. The
  overflow is the thing M1b's blocked-attacker path (all damage to the blocker,
  `bob` loses 0) cannot produce — that existing behavior is the control.
- *CR 702.19b — single blocker is now asked.* The same board asserts the assignment
  is **prompted** (a distribution exists other than all-to-blocker), where M1b's
  single-blocker path issues no prompt. The falsified shortcut.
- *CR 510.1e — lethal-first is enforced.* A decider that assigns 0 to the Piker and
  3 to `bob` is **rejected** (the blocker is below its lethal minimum); the
  attacker then assigns nothing (reject-not-repair). A property over legal
  assignments: the blocker always receives ≥ its minimum before the defender
  receives anything.
- *CR 702.2c — the interaction falsifier.* The synthetic trample+deathtouch 3/3
  attacks; Ogre Sentry (3/3) blocks. Lethal to the Ogre is **1** (deathtouch), so
  1 is assigned and **2 tramples** to `bob`; the Ogre still dies. Against the plain
  War Mammoth (3/3 trample, no deathtouch) into the same Ogre, lethal is 3, all 3
  are assigned, and **0 tramples**. The with-vs-without pair isolates 702.2c: only
  deathtouch on the source changes what spills.
- *CR 702.19b × 510.4 — already-marked damage lowers the floor.* A trampler that
  survives a first-strike step (or a board where the blocker entered the regular
  step with marked damage) needs less to reach the blocker's lethal, so more
  tramples over. Exercises the `alreadyMarked` term and the M2b×M2c seam.

**The classification test, the point of the milestone.** The 704.5h predicate, the
lethal minimum, and the 702.2c floor all read `keywordsOf` / `hasKeyword` and the
damage events, and **no call site in `Pawl.Sba` or `Pawl.Damage` names a card**.
Asserted the M2a/M2b way — by boards the identity-based implementation gets wrong:
the interaction falsifier above, whose outcome depends only on which citations the
source carries.

**Properties.** All of M1b's, M2a's and M2b's survive and keep passing:
conservation (120 objects — the deck is unchanged, so the constant holds),
termination, ids minted ≥ 120, no mana floats, combat happens, fliers get through,
no priority in a skipped step. M2c retires **none** of them — in particular **"life
never increases" survives**, because M2c adds no lifegain (lifelink is deferred,
§9); that property is lifelink's to kill, not M2c's. The new-card tests are
fixtures outside the deck, so no landed property is disturbed.

**New properties.**

- *deathtouch destroys across seeds* — over property games, any creature that an
  event records as dealt nonzero damage by a deathtouch source, and whose toughness
  is > 0, is in the graveyard after the following SBA check. (Inert until the deck
  contains a deathtoucher, so this rides the fixture scenarios in M2c and becomes a
  random-game property when a deathtoucher joins a deck — its own future deck
  change, noted.)
- *no trample assignment leaves a blocker short* — over any trample assignment the
  engine accepts, every blocker received ≥ its lethal minimum. The property form of
  CR 702.19b's floor.

## 8. Expiries

Every elision in M2c, and the milestone that kills it. Inherits M2b's table (still
owed), and M2a's and M1b's beneath it.

| Elision | Why it is legitimate now | Killed by |
|---|---|---|
| Deathtouch on the source is read live from `keywordsOf` at SBA time, not captured at deal time | Keywords are printed and survive in the graveyard, so "had it when it dealt" equals "has it now"; CR 702.2e's last-known-information never differs | **M3** — a granted/removed deathtouch (layer 6) makes "had it then" and "has it now" diverge, and 702.2e's last-known-information becomes load-bearing |
| `DamageEvent` payload is the minimal `(source, target, amount)` | Only deathtouch reads it, and it reads less than that | **M3** generalizes the change-and-emit helper to all mutations; **lifelink** and **M4** combat-damage triggers grow the payload (they add readers, not reshape it) |
| Damage-assignment chooser hardcoded to `controllerOf attacker` | 702.19b says the attacker's controller chooses, so it is correct for trample; nothing inverts it yet | **M3** — banding 702.22j (the *defending* player chooses) and Mindslaver (the chooser is a `Decider`, not a `PlayerId`) |
| The 702.2c falsifier uses a **synthetic** trample+deathtouch fixture | No printed card has both keywords, and M2c has no granting to combine them on a real card | **M3** — grant deathtouch to a real trampler (Basilisk Collar / layer-6); delete the fixture, test with real cards |
| Mono-red retired; deathtouch/trample cards are off-color | Fixtures place creatures directly and never cast them, so color is cosmetic; mono-red deathtouch does not exist | Nothing — this is a permanent, deliberate end of the convention, recorded so it is not silently rebuilt |
| `checkStateBasedActions` is single-pass, including 704.5h deaths | A deathtouch death causes no further SBA yet | **M4** — death triggers make CR 704.3's "repeat until stable" load-bearing (inherited from M2b) |
| 702.19d (blocked, no blockers remain at assignment) unimplemented | Nothing removes a blocker between declaration and damage | **M3+** — the first effect that removes a blocker mid-combat |
| `event drain` clears all `damageEvents` at each SBA check | Only deathtouch reads them, within one check's window | **M4** — triggers that fire on damage read the events before they drain, ordering the drain relative to trigger collection |

## 9. Explicitly deferred past M2c

- **Lifelink (702.15)** — the second damage-event reader, and the punchlist
  keyword nearest to M2c. Deferred deliberately: it would give the event a second
  consumer (a good thing) but expands scope past the brief's two keywords. The
  event is shaped so lifelink is a pure *reader* addition (`amount` + `source`), not
  a reshape. It is the reason M1b's "life never increases" property finally dies —
  at lifelink, not here.
- **The rest of the punchlist** — indestructible, intimidate, landwalk (same axes,
  no new machinery). Indestructible interacts with M2c directly (it ignores 704.5g
  and, being "can't be destroyed," 704.5h) and is the natural next SBA-axis card,
  but it is not deathtouch or trample.
- **The blocked keywords** — enchant, equip, flash, hexproof, protection, shroud,
  ward — unchanged from M2a's triage: machinery that does not exist yet.
- **Banding (702.22)** and the damage-assignment chooser inversion — a `Decider`
  problem, **M3+**, per §8 and `docs/design.md` §M2c.
- **Trample over planeswalkers (702.19c), 702.19e/f** — no planeswalkers exist;
  the whole planeswalker variant waits on the card type.
- **Layer-6 grant/removal of deathtouch or trample mid-combat** — **M3**, the same
  boundary-read seam M2b built for first strike.
