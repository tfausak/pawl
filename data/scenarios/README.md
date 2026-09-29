# Scenarios

Each `.json` file here is one gameplay test: a board, a timeline of decisions
and checks, and optional final checks. The test suite runs every one, and
`pawl scenario FILE...` runs any. `pawl schema scenario` prints the schema.

- **Board.** `seats` in turn order, each with a `name`, `life` (default 20)
  and `battlefield` and `hand` placements. A placement names its `card` and
  may give a `label`, `tapped`, `ready` (CR 302.6), `damage`, `counters` and a
  `controller`. `active` names a seat; `step` is where turn 1 starts. Setup is
  a state, not a history: nothing placed triggers anything.
- **References.** `"@name"` is a seat or a labelled card. `"Goblin Piker"` is
  the first live object with that name, `"Goblin Piker#2"` the second, in
  creation order across every zone.
- **Timeline.** Each entry is keyed by `turn`, `step` and the `player` who
  decides, and carries `do` (a move) or `check`. Entries sharing a key are
  taken in order. An unscheduled priority prompt passes; any other
  unscheduled prompt fails the scenario, as does an entry whose moment never
  comes. A check runs when its player is next offered priority at its key, so
  put a `"Pass"` between a check with a spell on the stack and one after it
  resolves. `source` picks among prompts sharing a key, such as one combat
  damage assignment per attacker.
- **Final.** The run plays whole steps until the timeline is spent, the game
  ends, or the turn passes the last one named; `final` checks that state.

A label names one object, not a card: CR 400.7 makes a card that changes zones
a new object, so a label given in hand goes stale once that card is played.
Name it by card afterwards.
