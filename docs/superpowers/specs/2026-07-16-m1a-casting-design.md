# M1a casting — design

Design for milestone **M1a**: casting a creature spell. This is the first half of
`docs/design.md`'s **M1** ("vanilla creatures"), which bundles two
independent subsystems. They are split here:

- **M1a (this spec)** — mana, the stack, casting, and resolution. Creatures reach
  the battlefield and sit there.
- **M1b (later spec)** — combat: declare attackers, declare blockers, combat
  damage, and the state-based actions that follow.

The split exists because a failure in the mana model should surface on its own,
not tangled with combat bugs. M0's stack field was built but only ever exercised
*empty*; M1a is where it carries something.

This is a types-and-architecture spec, not an implementation plan.

## M1a goal and scope

A game of 36 Mountain / 24 Goblin Piker per player. Players draw, play lands, tap
for mana, cast Pikers, and resolve them onto the battlefield. Nothing attacks;
the game still ends by decking out.

**Goblin Piker** — `{1}{R}`, Creature — Goblin Warrior, 2/1, no rules text.
Genuinely vanilla. Chosen over the design doc's Grizzly Bears (`{1}{G}` 2/2)
because it reuses M0's Mountain mana base and every existing test fixture, while
`{1}{R}` still exercises generic *and* colored payment. **Still zero opcodes.**

What M1a exercises that M0 did not:

- the mana pool (CR 106.4) and its emptying at each step's end (CR 500.4),
- intrinsic mana abilities derived from a subtype (CR 305.6),
- casting a spell (CR 601): hand → stack, cost payment, priority retention,
- the stack carrying an object, and priority under a non-empty stack (CR 117.4),
- resolution of a permanent spell (CR 608.3),
- cleanup discard as a real decision (CR 514.2).

**Exit criterion:** a game in which creature spells are cast and resolve
completes, and its `DecisionLog` replays deterministically.

## Conventions

Inherits M0's conventions (`Mk` prefix, boot libraries only, `Eq`/`Show`
everywhere). Two additions specific to this milestone:

- **No `Num` instance on `Quantity`.** "Numeric tower" (§2.12) names the *problem
  domain*, not a class hierarchy. `Num` would be lawless and partial the moment
  `Star`/`Infinite` exist (`signum Star`? `negate Infinite`?), which collides with
  the no-partial-functions rule, and `fromInteger` would silently erase the
  distinction the type exists to draw. Combining is explicit named functions.
- **Shape now, cases later.** Where a type's *shape* is expensive to change but
  its *cases* are not, land the shape early with a comment naming the growth.

## 1. The numeric model

§2.12 requires that P/T and mana not be `Int`, and notes that deciding this
before M4 is nearly free. M1a is the first milestone where P/T and mana costs
both appear, so the shape lands here.

### Naming

The type is **`Quantity`**, not `Characteristic`. CR 109.3 already defines an
object's *characteristics* as the whole set — name, mana cost, color, color
indicator, card type, subtype, supertype, rules text, abilities, power,
toughness, loyalty, defense, hand modifier, life modifier. Naming a bare number
`Characteristic` collides with the rules' own vocabulary in a project whose
naming discipline is rules fidelity.

```hs
-- A number that may not be a number yet.
-- Grows: Star (CDA), Plus Quantity Quantity, Half, Infinite, Variable.
data Quantity
  = Literal Integer

-- Callers go through this, so new constructors don't touch them.
evaluate :: GameState -> ObjectId -> Quantity -> Maybe Integer
```

`Plus` is binary and recursive rather than a flat enum, so composition covers the
awkward printed values without new cases: `1+*` (Tarmogoyf's toughness, and the
exact value §2.12 cites MTGJSON surrendering to) is `Plus (Literal 1) Star`;
`1+X` is `Plus (Literal 1) Variable`.

**Caveat:** `Star` can be *shaped* now but not *evaluated* now — a
characteristic-defining ability resolves in layer 7a, and the layer system does
not exist until M3. This is why only `Literal` is implemented.

### Users of `Quantity`

Newtypes, one per module: `Power`, `Toughness` in M1a; `Loyalty` (CR 306) and
`Defense` (CR 310) join them when planeswalkers and battles arrive. They are all
the same numeric model, which is the second reason the type isn't named for P/T.

## 2. Mana

### Pool contents

The pool is a multiset of **units**, not a count per type:

```hs
newtype Mana = MkMana [ManaUnit]

data ManaUnit = MkManaUnit
  { manaType :: ManaType
  }

data ManaType = Colored Color | Colorless
data Color = White | Blue | Black | Red | Green   -- CR 105.1, closed and finite
```

**Why units rather than `Map ManaType Natural`.** Counts discard provenance by
construction, and mana is not fungible in general:

- **Snow.** `{S}` is payable only with mana *produced by a snow source*, so a unit
  must remember that about its origin.
- **Restrictions.** Cavern of Souls-style "spend this mana only to cast a creature
  spell of the chosen type."

Counts cannot express either. This is not a shape that extends — it is one that
breaks: adding riders to counts means rewriting every payment call site.
Units→richer-units is a field addition; counts→units is a rewrite. M1a produces
only plain red units and reads nothing but `manaType`, so nothing is gained
*today*; the point is that nothing must be *redone*.

### What is deferred, and why

`ManaUnit` ships with `manaType` alone. It grows a set of production-time tags and
a list of spending restrictions — **never fixed fields**, since one unit can carry
several at once (§2.11: fixed arity is the recurring root cause). See "Two kinds of
rider" below for why those are two collections and not one.

The restrictions are deferred because **they are open half.** "Spend this only on
Bard creatures" is a predicate over spells. If payment ever reads
`case restriction of OnlyBards -> …`, that is the rules core learning an effect's
identity — the same fusion failure as `case effect of DealDamage{} -> …`, and the
single failure mode the project is organized against. Payment must ask a
*classification* ("may this unit pay for this spell?") and let the open half
answer. Typing those honestly needs M4's effect DSL, so inventing them now would
mean designing that DSL early and badly, in the place it is most dangerous.

**No source `ObjectId` on a unit.** Snow cares about a *property* of the source,
not its identity. Storing a reference would also be fragile in pawl specifically:
mana outlives its source (tap a land, the land is destroyed in response, the mana
remains), and CR 400.7 mints a fresh id on every zone change, so the reference
dangles by construction. Properties are captured at production time.

*Yawgmoth's Day Planner* was checked as the likeliest counterexample and turns out
to **confirm** this:

> {T}, Pay 2 life: Add {B}{B}. You may cast spells from your graveyard. Only mana
> produced by abilities that caused you to lose life may be spent to cast spells
> this way. (Damage causes loss of life.)

It does not care which object produced the mana — it cares whether the *production
event* caused life loss. That is fixed at production time, like snow. (Per the
reminder text, damage is life loss, so City of Brass's mana qualifies as readily as
the Planner's own; it is a property of the activation, not of a permanent.)

### Two kinds of rider, and only one is open half

The Planner draws a line worth stating explicitly, because it is easy to blur:

- **Production-time tags — closed half.** Snow-ness; "this ability's activation
  caused you to lose life". These are *observable facts about the production
  event*: the engine can determine them itself, at production, with no card
  knowledge. The tag set is open-ended, so it is a set of tags rather than fixed
  fields.
- **Spending restrictions — open half.** Cavern's "spend this mana only to cast a
  creature spell of the chosen type" is a *predicate over spells*. The engine must
  never case on these.

Both attach to a unit, and both are deferred from M1a, but they are not the same
thing and should not share a representation by accident.

### Costs

```hs
newtype ManaCost = MkManaCost [ManaSymbol]      -- a list: no fixed arity
data ManaSymbol = Generic Natural | OfType ManaType
```

Goblin Piker's cost is `[Generic 1, OfType (Colored Red)]`. `ManaSymbol` grows
hybrid, Phyrexian, snow, and X.

### `Pawl.Mana`

Intrinsic mana abilities (CR 305.6): a Mountain's `{T}: Add {R}` is **derived from
its subtype**, never stored on the card — the card data stays a type line, exactly
as §M0 promised. Plus pool produce/spend and `emptyManaPools` (CR 500.4).

### Payment is canonical, not clever

Paying `{1}{R}` means tapping 2 of N untapped Mountains. They are
indistinguishable, so the engine taps front-of-list without prompting.

**This is choice elision, not engine intelligence.** The engine makes no player
choices; that is what the `Program`/`Prompt` seam is for, and strategy belongs to
the interpreter. Eliding is legitimate *only* because the options are
indistinguishable — picking one is canonicalization, since there is no policy to
have. The moment mana sources differ in any way a player could care about, this
must become a real prompt. The elision has an expiry, and helpers are named for
what they do (canonical/deterministic), never "auto", which implies smarts.

Payment is nonetheless modeled as **produce-then-spend** through the pool, not as
an atomic shortcut, so the shape holds when mana abilities multiply.

## 3. The stack and casting

### `Pawl.Cast`

`Action.Cast ObjectId` is legal only at sorcery speed (CR 302.1 / 307.1): a main phase,
the active player, an **empty stack**, and an affordable cost. This is the same
timing gate as the land special action, and is factored as such.

Casting (CR 601) moves the card hand → stack via `changeZone` (fresh id per
CR 400.7), taps lands canonically, spends from the pool, and leaves the **caster**
holding priority.

**M0 correction folded in.** `priorityLoop` currently hands priority to
`GameState.activePlayer` after a land play (`Engine.hs:145`). That is only
accidentally correct, because lands can only be played by the active player.
CR 117.3c gives priority to the player who *took the action*. Using the actor is
both correct and no harder.

### `Pawl.Stack` — resolution by classification

`resolveTop` reads the stack object's type line, asks whether it is a **permanent
spell**, and if so changes its zone to the battlefield (CR 608.3); otherwise it
goes to the graveyard.

This needs `Pawl.Type.CardType` to gain `Creature`, plus two classification
queries in **`Pawl.Card`** — not in `Pawl.Type.CardType`, which per the
one-type-per-module rule holds the type and its instances only:

```hs
Pawl.Card.isPermanentType :: CardType -> Bool   -- CR 110.1
Pawl.Card.isPermanent     :: Card -> Bool       -- any permanent type on the type line
```

CR 110.1's permanent types are an enumeration: closed half, finite. **The engine
never learns which card it is.** Resolution dispatches on *is-it-a-permanent*, the
same classification shape as *is-it-a-mana-ability*. Zero opcodes.

Rejected: a per-card resolution tag on `Printing`, or casing on card name. Both
smuggle identity into the closed half.

### Object churn

Casting a Piker mints a new object on the stack; resolving it mints another on the
battlefield. `objectCount` is unchanged by `changeZone`, so M0's conservation
property still holds at 120, while minted ids climb well past 120 — which the
existing "at least 120 ids minted" property already asserts correctly.

## 4. Engine changes

### Priority under a non-empty stack (CR 117.4)

M0's `priorityLoop` ends the step on a full round of passes, which is only valid
because the stack is always empty. The rule: when all players pass in succession,
**if the stack is non-empty**, the top object resolves, passes reset, and the
active player receives priority; **only if the stack is empty** does the step end.

This is the milestone's real risk — it is live code with existing tests on it.

### Mana pools empty at step end (CR 500.4)

In `runStep`, after the priority loop. Nothing floats mana in M1a, so this is
unobservable today; it is a one-liner and a known-correct rule, so it lands now
rather than as a documented deviation.

### Cleanup discard becomes a prompt (CR 514.2)

M0 trims from the front of hand without prompting — an explicit simplification
justified *only* because every card was an identical Mountain (M0 plan,
self-review note 6: "revisit when non-identical cards arrive"). **M1a is when they
arrive.** Hands now hold Mountains and Pikers, so front-of-hand trimming stops
being canonicalization and becomes the engine deciding you would rather pitch a
land than a creature — policy in the rules core.

```hs
data Prompt r where
  ...
  ChooseDiscard :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
```

`Response` gains its mirror. `Pawl.Replay`'s `encode`/`decode`/`defaultAnswer` are
exhaustive matches on `Prompt`, so the compiler forces the new constructor through
the replay layer under `-Weverything -Werror`. `defaultAnswer` must stay total.

## 5. Module layout

New modules, keeping each small. `Engine` retains only the turn/priority loop and
orchestrates the rest.

| Module | Responsibility |
|---|---|
| `Pawl.Type.Quantity` | the numeric model |
| `Pawl.Type.Power`, `Pawl.Type.Toughness` | newtypes over `Quantity` |
| `Pawl.Type.Color`, `Pawl.Type.ManaType` | mana taxonomy |
| `Pawl.Type.ManaUnit`, `Pawl.Type.Mana` | pool contents |
| `Pawl.Type.ManaSymbol`, `Pawl.Type.ManaCost` | costs |
| `Pawl.Mana` | intrinsic abilities, produce/spend, CR 500.4 |
| `Pawl.Cast` | cast legality and the CR 601 sequence |
| `Pawl.Stack` | `resolveTop`, classification dispatch |

Modified: `Pawl.Type.Card` (gains `manaCost :: Maybe ManaCost`, `power`,
`toughness`; lands have no mana cost per CR 202.1), `Pawl.Type.CardType` (gains
`Creature`), `Pawl.Type.Subtype` (gains `Goblin`, `Warrior`),
`Pawl.Type.GameState` (gains `manaPool :: Map PlayerId Mana`), `Pawl.Type.Action`
(gains `Cast`), `Pawl.Type.Prompt`/`Response` (gain `ChooseDiscard`), `Pawl.Card`
(gains `pikerPrinting`, `isPermanent`, `isCreature`), `Pawl.Action`, `Pawl.Setup`
(deck composition), `Pawl.Engine`, `Pawl.Replay`.

## Explicitly deferred past M1a

- **Combat** — all of it. M1b.
- **Summoning sickness** (CR 302.6) — no creature can act in M1a, so nothing can
  observe it. Lands in M1b with combat.
- **Instant-speed casting**, the split second of responding to a spell on the
  stack. Piker is sorcery-speed; nothing in M1a can be cast in response.
- **Mana unit tags** (snow, caused-life-loss) — closed half and safe to model
  early, but dead state until a card reads them.
- **Mana spending restrictions** — open half, needs M4's effect DSL.
- **`Star`/`X` evaluation** — needs the layer system (M3).
- **Non-canonical mana payment** — until sources differ.

## Testing approach for M1a

Following M0: one growing `testTree` in `source/test-suite/Main.hs`, favoring
property-based and integration-style tests that assert `GameState` after actions.

**Rule-numbered scenarios**, matching M0's `ruleTests` convention:

- *CR 305.6* — a Mountain's red mana ability is derived from its subtype.
- *CR 500.4* — the pool is empty at the end of each step.
- *CR 302.1* — a creature spell is illegal in the upkeep, and illegal with a
  non-empty stack.
- *CR 117.3c* — the caster retains priority after casting.
- *CR 117.4* — a full round of passes resolves the top of the stack rather than
  ending the step.
- *CR 608.3* — a resolved creature spell becomes a permanent: a new object, on the
  battlefield, whose type line contains `Creature`.
- *CR 514.2* — discard is prompted, and the prompted choice is honored.

**Properties.** All four M0 properties survive M1a unchanged and must keep
passing:

- conservation — still exactly 120 objects,
- termination — every game reaches a result (casting cannot prevent decking out),
- ids minted ≥ 120 — now climbing well past it,
- **no life changes** — still true, because damage does not exist until M1b.
  That property failing is precisely how M1b announces itself.

New: mana pools are empty in the final state.
