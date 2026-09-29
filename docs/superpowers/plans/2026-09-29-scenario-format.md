# Scenario format, piece 1: the format itself

Related to #146. That issue splits into two large pieces: first the scenario
format, then migrating the existing specs into it. This plan is the first
piece. Migration (the combat spec first, per #146's exit criterion) is the
second and is not started here.

## Research (against `origin/main` @ 07b8f1a)

- #3050's keyed harness exists, but only as test code: `Pawl.Support` holds
  `Board`, `PlayerSetup`, `ObjectSetup`, `ObjectRef`, `ActionChoices`, `When`,
  `Entry`, `Timed`, `HarnessFailure`, `buildBoard` and `runScript` (about a
  thousand lines with its renderers). Nothing ships it, nothing encodes it, and
  it has no checks: every assertion is Haskell run on the final state.
- 27 `S.play`/`S.runScriptOrFail` call sites in 16 specs drive it, plus
  `Pawl.SupportSpec`'s 21 cases on the harness itself. Of 11435 cases, that is
  well under one percent, so a lift now is cheap and a lift after migration is
  not.
- Its seats are `PlayerId`s. A JSON document cannot carry one meaningfully,
  and a codec is context-free, so a name has to be the reference and the
  runner has to assign the ids. Every board in the tree lists alice first but
  one (`Pawl.CombatSpec`'s Siege Muster blocking twin, where the order is
  unobservable).
- The cast selector (`Maybe (CardName, Facing)`) and the land-play face have
  no caller outside `Pawl.Support`. They are dropped rather than lifted.
- `Pawl.JsonCodec.Arm.tagged` writes `{"type": ..., "value": ...}`, which is
  what the card corpus uses and what #2171 wants shortened. Hand-written
  scenarios are the case #2171 names, so the scenario codecs use shorter
  shapes where the default is noisiest (references and steps as strings,
  verbs as keys) and the stock combinators everywhere else.
- `Pawl.Executable` dispatches `schema | deck | bench | test`. `data/cards` is
  cabal `data-files` resolved through `Paths_pawl` in `pawl:registry`.

## Design

### Names and references

A board names every seat, and may name (alias) any object it places. Seats and
aliases share ONE namespace, a `Pawl.Types.Label`, so a reference to either is
written the same way:

| JSON              | Meaning                                              |
| ----------------- | ---------------------------------------------------- |
| `"@bear"`         | the object the board labelled `bear`, or seat `bear` |
| `"Grizzly Bears"` | the first live object with that card name            |
| `"Grizzly Bears#2"` | the second, in creation order                      |

`Pawl.Types.Reference` is `Labelled Label | Printed CardName Natural`. A
card-name reference counts live objects in every zone in creation order, as
the harness already does. Where a reference must be an object (an attacker, a
mana source), a label naming a seat is a failure; where it may be a player (a
target, a damage recipient, an attack target), it resolves to one. A label
used twice on one board is a board failure.

Seat order is turn order and assigns the ids: the first seat is `PlayerId 0`.

### The document

```json
{
  "description": "CR 510.1b an unblocked attacker damages the defending player",
  "board": {
    "active": "alice",
    "step": "BeginningOfCombat",
    "seats": [
      { "name": "alice", "battlefield": [{ "card": "Goblin Piker", "label": "attacker", "readiness": "Ready" }] },
      { "name": "bob" }
    ]
  },
  "timeline": [
    { "turn": 1, "step": "DeclareAttackers", "player": "alice", "do": { "Attack": ["@attacker"] } },
    { "turn": 1, "step": "EndOfCombat", "player": "alice", "check": { "Life": { "player": "bob", "life": 18 } } }
  ]
}
```

- A step is one flat name (`Untap` ... `Cleanup`), not the nested `Phase`
  encoding.
- A timeline entry is keyed `(turn, step, player)` and carries exactly one of
  `do` (a `Move`) or `check` (a `Check`), plus an optional `source` qualifier.
- `Move` and `Check` encode externally tagged: a nullary arm is its bare tag
  (`"Pass"`), a payload arm a one-key object (`{"Attack": [...]}`). This is a
  new `Arm.keyed` beside `Arm.tagged`, with its schema, and `Arm.keyedEnum`
  gives an all-nullary type (`TapState`, `Zone`, `Readiness`) the same bare
  strings without touching that type's card codec.
- `final` is an optional list of checks run on the state the run stops at,
  which is the only place a finished game can be asserted on. It replaces
  #146's `AtEnd` key: as a separate field, a `do` at the end cannot be
  written, rather than being a runtime error.

### Semantics (#146 decisions 2 and 3, kept)

- An unscheduled `ChooseAction` passes. Any other unscheduled prompt fails.
- An entry whose key never arrives fails. A check that never ran fails with
  its OWN constructor, never spelled like a check that ran false.
- A check is evaluated when its player is next offered priority at its key,
  in timeline order with the moves at that key. `Pass` is an explicit move, so
  "with the spell on the stack" and "after it resolved" are both writable. A
  non-priority prompt skips the checks queued at its key, since a check waits
  for priority by definition.
- A JSON scenario has one entry point, `Pawl.Scenario.play`: whole steps
  (`Engine.runStep`) from the board's step until the timeline is spent, the
  game is over, or the turn passes the last turn the timeline names. A Haskell
  caller keeps passing any `Game a`, as today.
- Setup is a state, not a history: objects are placed directly, firing
  nothing (#146 decision 5).

### Vocabulary

Lifted as the harness has it, minus the unused selectors: `Cast`,
`PlayLand`, `Activate`, `Attack`, `Block`, `AssignDamage`, `ChooseDefender`,
`ChooseAttackTarget`, `Concede`, and new `Pass`. A cast or activation carries
its targets, modes, X, cost, cost order, mana sources and mana yields as
fields of the same object; an activation also carries its ability index.

`Check` starts with the nouns piece 1's scenarios use: `Life`, `Count` (cards
of one name in one of a player's zones; the controller's, for the
battlefield), `Damage` and `Tapped`. It grows on demand in piece 2.

### Where it lives

- `pawl:types`: `Pawl.Types.{Label, Reference, Readiness, Placement, Seat,
  Board, Choices, Casting, Activation, Move, LifeIs, CountIs, DamageIs,
  TappedIs, Check, When, Entry, Timed, Scenario, ScenarioFailure}`.
- `pawl:codec`: a codec and spec for each but `ScenarioFailure`, which is
  never encoded, and a flat step codec beside `Pawl.Codec.Phase`'s.
- `pawl:json-codec`: `Arm.keyed`, and `Fields.contramap` so a `Choices` record
  can share its parent's JSON object.
- New `pawl:scenario`: `Pawl.Scenario` (build, run, check, render) and
  `Pawl.Scenario.Load` (read `data/scenarios`).
- `pawl:engine`: `Setup.placeCard`, the direct placement `Pawl.Support` and
  `Setup.createCard` both spell out as a sixty-field literal today.
- `pawl:test`: `Pawl.Support` keeps its builders (`S.duel`, `S.settled`,
  `S.turn`, `S.on`, `S.play`, ...) over the lifted types and loses the rest.
  Every JSON file under `data/scenarios/` becomes one case.
- `pawl:executable`: `pawl scenario FILE...` runs files and reports each, and
  `pawl schema scenario` emits the schema. This is the first way to drive the
  engine that is not the test suite.

## Tasks

1. `Arm.keyed` and `Fields.contramap`, with specs.
2. The types, their codecs and codec specs, the flat step codec.
3. `Setup.placeCard`; `Setup.createCard` and `Pawl.Support.addObjectIn` over it.
4. `pawl:scenario`: lift build and run out of `Pawl.Support`, retyped over
   labels; add `Pass`, checks, `final`, the unrun-check failure, `play` and
   the loader.
5. Rewrite `Pawl.Support`'s harness as builders over the library; move every
   call site and `Pawl.SupportSpec` onto it.
6. `data/scenarios/`: one scenario per verb and per check, each checked
   against the schema; the loader wired into `Pawl.Test`.
7. `pawl scenario` and `pawl schema scenario`.
8. Docs: `docs/agents/implementing.md`'s harness section, and a README for
   `data/scenarios/`.

## Verification

- Suite count before and after.
- Mutations: a check evaluated false must fail as a false check, and a
  check never reached as an unrun one (each on its own scenario); drop the
  "unscheduled prompt fails" guard and a scenario must fail; make `Pass`
  answer the first offered action and the explicit-pass scenario must fail.
- Each JSON scenario validates against the emitted schema.

## Deferred to piece 2

Migrating specs, starting with `Pawl.CombatSpec`; growing `Check` and the
board (library, graveyard, exile, counters on players, monarch) as the
migrated tests need them; `Prompt.OrderTriggers` and the other prompts the
harness cannot answer; the XMage one-way conversion.
