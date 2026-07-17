# M1b combat — design

Design for milestone **M1b**: fighting with a creature. This is the second half
of `docs/design.md`'s **M1** ("vanilla creatures"), split from **M1a**
(casting) so that a failure in the mana model surfaces on its own rather than
tangled with combat bugs.

M1a put creatures on the battlefield, where they sat. M1b is where they act.

This is a types-and-architecture spec, not an implementation plan.

## M1b goal and scope

The same game as M1a — 36 Mountain / 24 Goblin Piker per player, **still zero
opcodes**. Pikers now attack, block, deal damage, and die. The game can still end
by decking out; it can now also end by damage.

**Goblin Piker** — `{1}{R}`, Creature — Goblin Warrior, 2/1, no rules text.
Its 2/1 body is doing real work here: power 2 against toughness 1 makes the
damage-division choice non-trivial (see §3), and a 2/1 trading with a 2/1 is what
proves damage is simultaneous.

What M1b exercises that M1a did not:

- summoning sickness (CR 302.6, 508.1a) — nothing could observe it in M1a,
- declaring attackers (CR 508) and blockers (CR 509) as **real decisions**,
- combat damage assignment (CR 510.1a) and its simultaneity (CR 510.2),
- the first damage in the engine's life (CR 120.3e),
- state-based actions for creatures: zero toughness (CR 704.5f) and lethal
  damage (CR 704.5g),
- the first real use of `Quantity`'s `Maybe` — reading a toughness.

**Exit criterion:** M0's "no life changes" property finally fails. That property
has passed since M0 and its own comment names this milestone as its executioner:
*"This property FAILING is precisely how M1b announces itself."* Deleting it is
not weakening the suite — it is the milestone landing. It is replaced by two
stronger properties (§6).

This is the combat **skeleton**. M2 is where combat gets genuinely hard, and
several decisions below are shaped by what M2 is known to bring: flying/reach/
menace (blocking restrictions), first strike (a *second* damage step),
deathtouch/trample (damage assignment).

## Conventions

Inherits M0's and M1a's conventions (`Mk` prefix, boot libraries only, `Eq`/`Show`
everywhere, no `Num` on `Quantity`, shape-now-cases-later). One addition:

- **Every elision names the milestone that kills it.** M1a introduced documented
  expiries; M1b has two, and they are collected in §7 rather than scattered, so
  that M2 has a checklist rather than an archaeology problem.

## 1. Controller, and why `owner` still stands in for it

M1a's self-review note 6 flagged that `Object.owner` stands in for controller
throughout `Pawl.Mana`, and that it must become a real field before Mindslaver
(M3). Combat is defined entirely in controller terms (CR 508.1a: attackers are
declared from creatures *you control*), so M1b is the obvious place to pay that
debt.

**It is not paid here, deliberately.** Nothing in M1b can change control — no
auras, no theft, no Mindslaver — so owner and controller are *provably* identical.
A `controller` field would be dead state that every fixture must set and every
zone change must maintain, with no card able to observe it drifting out of sync.
Dead state that nothing can falsify is not a safety net; it is a second thing to
keep right.

What M1b does instead is deny combat the ability to hard-code the assumption:

```hs
-- Pawl.Game
-- Nothing when the object does not exist.
--
-- EXPIRES at M3 (Mindslaver): the moment anything can change control, this stops
-- being owner and becomes a real field. Combat calls this, never Object.owner,
-- so that change is one function rather than every call site.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId
```

This is the same move M1a made with `Mana.manaSources`, and the same reason:
the fix should be local when it comes.

## 2. New state

### Marked damage is a count, and mana was not — deliberately

```hs
-- Pawl.Type.Object gains:
damage :: Natural       -- CR 120.3e. Wears off at cleanup (CR 514.2).
sickness :: Sickness    -- CR 302.6.
```

M1a chose mana **units** over counts because counts discard provenance, and mana
is not fungible: snow `{S}` and Cavern-style restrictions interrogate a unit at
*spend* time, long after it was produced. The obvious question is why damage does
not get the same treatment, given deathtouch arrives in M2.

**Because the rules themselves model marked damage as a number, and every rider
is consumed at deal time.** CR 120.3e: damage "causes that much damage to be
*marked* on that creature". CR 704.5g reads "the *total* damage marked on it".
The riders divert rather than annotate — wither/infect become −1/−1 counters
(120.3d), lifelink gains life (120.3f), toxic gives poison counters (120.3g). None
of them leaves a tagged point of damage behind for anything to inspect later.

The one genuinely delayed reader is deathtouch:

> **CR 704.5h** If a creature has toughness greater than 0, and it's been dealt
> damage by a source with deathtouch **since the last time state-based actions
> were checked**, that creature is destroyed.

That is a delayed read, and it is worth being precise about what it reads: an
**event** ("was dealt damage by a deathtouch source since the last SBA check"),
not the marked-damage pile. It is a transient bit, cleared at every SBA check,
that sits *beside* `damage` rather than inside it. The same is true of the other
delayed-damage cards — Bloodthirst-style "was an opponent dealt damage this turn"
is turn-scoped event history about a *player*, and the objects involved may have
left the battlefield entirely. Those want an event log, which the engine needs
anyway once triggers exist.

**The bounded-blast-radius argument, which is the real justification.** Marked
damage has exactly one reader: the CR 704.5g lethal check. If this call is wrong,
it is a one-field, one-call-site change. That is precisely what made mana
different — counts→units would have rewritten *every payment site*, so it had to
be right up front. Damage has no such property. The cost of being wrong here is
small and local, and this paragraph exists so the decision stays re-examinable
rather than being inherited as folklore.

### Summoning sickness

```hs
-- Pawl.Type.Sickness — CR 302.6. A sum type, not a Bool: no boolean blindness.
data Sickness = Sick | Settled
```

Set to `Sick` when an object enters the battlefield; cleared to `Settled` during
the untap step for the active player's permanents, hooking into the `untapAll`
pass `Engine` already runs there. This models the rule's actual mechanism rather
than encoding it as arithmetic over turn numbers.

Rejected: `enteredTurn :: Natural` with `enteredTurn < turnNumber`. Storing a fact
is usually better than maintaining state, but that predicate is correct *only*
because nothing changes control yet — CR 302.6 is about control held
*continuously*, so a control change resets the clock. The arithmetic version would
keep compiling and quietly return wrong answers at M3. The flag breaks loudly.

### Combat

```hs
-- Pawl.Type.AttackTarget
-- Grows: OfPlaneswalker (CR 306), OfBattle (CR 310).
newtype AttackTarget = OfPlayer PlayerId

-- Pawl.Type.Combat — CR 508/509. Cleared at end of combat (CR 511).
data Combat = MkCombat
  { attackers :: Map ObjectId AttackTarget,
    -- A Set, not a list: blockers are UNORDERED. See below.
    blockers :: Map ObjectId (Set ObjectId)
  }
```

`GameState` gains `combat :: Combat`. A dedicated record rather than flat fields,
because combat state has a *lifetime* the other `GameState` fields do not — one
combat phase — and something has to reset it as a unit.

`blockers` is a set per attacker, not one blocker per attacker: multi-blocking is
legal from day one, and fixed arity is §2.11's recurring root cause.

**Unordered, and this is worth stating because it contradicts folklore.** Damage
assignment order was *removed from the game*; the rules glossary lists "Damage
Assignment Order" as **Obsolete**, and CR 509.2 is now simply "the active player
gets priority". CR 510.1c divides damage among multiple blockers "as its
controller chooses among them" — freely, with no ordering and no lethal-in-order
requirement. Storing an order would be modeling a rule that does not exist, and
would invite code that enforces a constraint the game does not have.

Lethal-in-order survives in exactly one place — trample (CR 702.19b) — where it is
a property of *that keyword*, not of combat. It arrives with M2, in the keyword,
where it belongs.

## 3. Combat's choices, and the one that is forced

M1b has **no combat elisions**. This section originally elided a blocker
ordering; checking the rules removed the need, which is the better outcome — the
cleanest elision is the one that turns out not to exist.

**Declaring attackers and blockers are real decisions.** No canonicalization
argument is available: attackers are never indistinguishable, because attacking
taps a creature and exposes it, and choosing *which* Pikers to send is the entire
game. Both are prompts.

**Dividing damage among two or more blockers is a real decision, and a free one.**
CR 510.1c: a creature blocked by two or more creatures "assigns its combat damage
to those creatures divided as its controller chooses among them." There is no
ordering and no lethal-in-order rule — the rules' own example has a 4/3 blocked by
a 2/3 and a 1/1 assigning all 4 damage to the 1/1 if it likes. A Piker (power 2)
blocked by two Pikers (toughness 1) may assign `2/0`, `1/1`, or `0/2`; `1/1` kills
both blockers and `2/0` kills one. The outcomes differ, so the choice is the
player's:

```hs
AssignCombatDamage
  :: Decider
  -> PlayerId
  -> ObjectId          -- the attacker
  -> Set ObjectId      -- its blockers (two or more; unordered)
  -> Natural           -- damage to divide, equal to its power (CR 510.1a)
  -> Prompt (Map ObjectId Natural)
```

**A single blocker is forced, and must not be prompted.** CR 510.1c: "If exactly
one creature is blocking it, it assigns all its combat damage to that creature."
That is not a choice with one good answer — it is not a choice at all, and asking
would be the engine inventing a decision the rules do not offer. `AssignCombatDamage`
fires only for two or more blockers. This is the mirror image of an elision: not
*declining* to ask about indistinguishable options, but declining to ask where the
rules leave nothing to ask.

`Prompt` also gains `DeclareAttackers` and `DeclareBlockers`; `Response` gains
their mirrors. `Pawl.Replay`'s `encode`/`decode`/`defaultAnswer` are exhaustive
matches, so `-Weverything -Werror` forces all three through the replay layer.
`defaultAnswer` must stay total.

**Validation, not trust.** Every prompted answer is filtered to what is legal: an
attacker must be the prompted player's, untapped, `Settled`, and a creature; a
blocker must be theirs, untapped, and blocking a real attacker. Illegal entries
are dropped, exactly as M1a's `discardToHandSize` filters to cards actually in
hand.

### CR 510.1e is unreachable, and is not implemented

CR 510.1e says an illegal damage assignment is rewound — "the game returns to the
moment before that player began to assign combat damage" — via CR 733. It is
tempting to implement that rewind. **It would be a category error.**

Rule 733.1 opens: "If a **player** takes an illegal action or starts to take an
action but can't legally complete it…". It is a rule for humans at a table, about
physically doing something wrong; 733.2's "the player … may redo the reversed
action in a legal way" only means anything to someone who can be told they erred.
An enforcing engine gives a player no way to take the illegal action in the first
place: the interpreter is offered a prompt, and a UI built on it presents only
legal divisions.

**M1a already settled this**, and the argument transfers verbatim:

> CR 601.2 puts the card on the stack BEFORE costs are paid, rewinding the whole
> cast if it turns out to be illegal. M1a pays first because `legalActions` only
> ever offers an affordable cast, so there is nothing to rewind.

733's rewind is unreachable for exactly the reason 601.2's rewind is.

What *can* still arrive is an interpreter returning an illegal
`Map ObjectId Natural`. That is not a game event and the rules have nothing to say
about it — it is a **software contract violation**: a bug, or a hostile client.
The two must not be conflated, because the rules' remedy (rewind and re-ask) is
meaningless against the real cause.

**Retrying is provably useless.** The seam is
`runGamePure :: (forall r. Prompt r -> r) -> …` — a *pure function*. The same
prompt yields the same answer by definition, so a re-prompt returns the identical
illegal division. A retry is not caution; it is dead code, and it would write
duplicate responses into the `DecisionLog` for `Replay` to faithfully reproduce.

So the engine's only obligation is to stay total. An illegal division is rejected
and the attacker assigns no combat damage. That is not a punishment invented by
the engine: it is what CR 510.1b/c already prescribe for a creature with nothing
legal to assign to, and it fails *loudly* — creatures conspicuously do not die —
which is how an interpreter bug should present. Repairing an illegal division into
a legal one would mean the engine choosing which creature dies. Per M1a's
`discardToHandSize`: "that is its bug, and inventing a fallback here would put the
policy back."

Rejected: enumerating the legal divisions so an illegal one is unrepresentable.
It matches `ChooseAction`'s shape and is trivial for M1b (2 damage among ≤4
blockers is 10 options), but it does not scale — divisions of N damage among K
blockers number `C(N+K-1, K-1)`, so a 20/20 trampler against 5 blockers is ~10,600.
Enumerate when the space is small and finite; validate when it is not.

## 4. The combat sequence

The five `CombatStep`s already exist and `Turn` already walks them; M0 built the
skeleton and left it inert. M1b gives them work, through `runTurnBasedActions`:

| Step | What happens |
|---|---|
| `BeginningOfCombat` | nothing (CR 507) |
| `DeclareAttackers` | prompt, validate, tap attackers (CR 508.1f), record |
| `DeclareBlockers` | prompt each defending player, validate, record |
| `CombatDamage` | assign and deal, simultaneously (CR 510.2) |
| `EndOfCombat` | clear `Combat` (CR 511) |

**Damage is simultaneous, and this is load-bearing.** CR 510.2: all combat damage
is dealt at once. The whole assignment is collected, then applied, then SBAs run
once. Applying attacker-by-attacker with SBAs interleaved would mean a 2/1 trading
with a 2/1 kills only one of them — the second creature would already be in the
graveyard before it dealt its damage. The simultaneity is exactly what the trade
test proves.

An unblocked attacker deals its damage to its `AttackTarget`; a blocked attacker
divides among its blockers. Blockers deal their damage to the attacker they block.

**Note:** an attacker whose blockers all leave is still *blocked* (CR 509.1h) and
deals no damage to the player. M1b cannot construct that state — nothing can
remove a blocker mid-combat without instant-speed interaction — so `blocked` is
derived as "has a non-empty blocker set" rather than stored as a status. This
**EXPIRES at M2** (§7).

## 5. State-based actions

`Pawl.Sba` gains two creature checks, joining M0's 704.5a (life ≤ 0) and 704.5b
(deck-out):

- **CR 704.5f** — toughness 0 or less: put into owner's graveyard.
- **CR 704.5g** — damage marked ≥ toughness (and toughness > 0): destroyed.

Both must read a toughness, which makes them **the first real consumer of
`Pawl.Quantity.evaluate`'s `Maybe`** — the shape M1a landed for `Star` and never
exercised. A toughness that cannot be evaluated yields *no* state-based action,
not a crash. In M1b every toughness is a `Literal` so the `Nothing` branch is
unreachable; it is written because the function is total and because M3's layer
system will make it reachable.

Order matters: 704.5f before 704.5g, per the rules' own numbering.

**704.5f is unreachable in M1b.** Every creature is a printed 2/1 and nothing can
modify toughness, so nothing can reach 0. It is implemented anyway, for the same
reason M1a implemented `resolveTop`'s non-permanent branch: it is the rule, and
omitting it would make the function partial. It becomes reachable at M2 (a −1/−1
counter, via wither) and thoroughly reachable at M3 (the layer system).

## 6. Testing approach for M1b

Following M0 and M1a: one growing `testTree` in `source/test-suite/Main.hs`,
favoring property-based and integration-style tests that assert `GameState` after
actions.

**Rule-numbered scenarios**, matching the `ruleTests` convention:

- *CR 302.6* — a Piker cannot attack the turn it resolves, and can on the next.
- *CR 508.1f* — declaring an attacker taps it.
- *CR 509* — a blocked attacker deals no damage to the defending player.
- *CR 510.1c* — a single blocker takes all the damage, with no prompt at all.
- *CR 510.1c* — a free division: 2 damage across two blockers may be `1/1`
  (killing both) or `2/0` (killing one), and the prompted answer is honored.
- *CR 510.1e* — a division that does not total the attacker's power is rejected,
  and the attacker deals no combat damage. This tests the engine's defense against
  a broken interpreter, not a reachable game state (§3).
- *CR 510.2* — simultaneity: a 2/1 attacker and a 2/1 blocker trade, and **both**
  die.
- *CR 704.5g* — a creature with lethal damage marked is destroyed.
- *CR 514.2* — marked damage wears off at cleanup.

The 510.2 trade test is the one to write first and the one to distrust last: it is
the only test that fails if damage is applied sequentially, and sequential damage
is the natural way to write the code.

**Properties.** Of M0's four, three survive unchanged and must keep passing:

- conservation — still exactly 120 objects (dead creatures move to the graveyard;
  they do not vanish),
- termination — every game reaches a result,
- ids minted ≥ 120.

**Retired:** "no life changes before combat". This is the exit criterion, not a
regression.

**New, replacing it:**

- **life only ever decreases** — nothing in M1b gains life, so any increase is a
  bug. This is the honest successor: it is what remains true of the old property
  after damage exists, and it dies at lifelink (M2), which is the next thing to
  announce itself.
- **a game still terminates under random play** — this is the property that
  matters most now. Combat is the first thing that can end a game *before* the
  library runs out, and the first that can end it early.

## 7. Expiries

Every elision in M1b, and the milestone that kills it. M2 should treat this as a
checklist.

| Elision | Why it is legitimate now | Killed by |
|---|---|---|
| `blocked` derived from a non-empty blocker set | Nothing can remove a blocker mid-combat | **M2** — CR 509.1h needs a stored status |
| `controllerOf` returns `owner` | Nothing can change control | **M3** — Mindslaver |
| Marked damage is a `Natural` | Every damage rider is consumed at deal time | Re-examine at **M2** (deathtouch); see §2 |
| One combat damage step | No first strike | **M2** — CR 510.4 splits it in two |

## Explicitly deferred past M1b

- **Keyword abilities** — all of them. M2. Flying, menace, first strike,
  deathtouch, trample, lifelink, protection.
- **Instant-speed interaction** in combat — nothing can be cast in response, so
  no combat trick can remove a blocker or pump an attacker.
- **Attacking planeswalkers or battles** — `AttackTarget` is shaped for them;
  neither exists.
- **Multiple combat phases**, extra combats.
- **Regeneration** — 704.5f/g both mention it; nothing can regenerate.
- **Damage to players from anything but combat** — no burn until M4.
