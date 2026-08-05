# CR 104.4b: a loop of mandatory actions is a draw

**Issue:** #338 (#484 was closed as a duplicate of it).

## The rule

> 104.4b If a game that's not using the limited range of influence option
> (including a two-player game) somehow enters a "loop" of mandatory actions,
> repeating a sequence of events with no way to stop, the game is a draw. Loops
> that contain an optional action don't result in a draw.

## The problem

`Engine.playGame` has no progress bound. Its comment carries a termination
*argument* — libraries are finite, each turn draws a card, drawing from an empty
library loses — which rests on the draw step being reached. Anything that stops
that (an unbounded skip, a graveyard shuffled back in, a repetition that never
reaches a draw step at all) leaves the engine spinning on a game that cannot
progress.

Two of the three loops the engine can spin never reach a draw step:

- **The extra-cleanup chain.** `cleanupException` schedules a successor cleanup
  step whenever CR 514.3a finds work. That is bounded for today's pool, not in
  general — its comment says so.
- **The stack-resolution cycle.** A pair of permanents whose triggers re-create
  each other's conditions resolves forever inside one priority round.

The turn-side loop — a Stasis lock, an absurdly large deck — is explicitly *not*
what this closes. Two players willingly playing forever is their business.

## Why the obvious heuristic cannot work

The suggestion on the issue was: draw the game once the timestamp has advanced
_N_ times without prompting a player for anything. Taken literally, that never
fires. `priorityLoop` prompts twice, unconditionally, on every priority grant:

- `Prompt.Concede` (`Engine.hs:669`)
- `Prompt.ChooseAction` (`Engine.hs:698`), even when `Pass` is the only legal
  action

Every loop the engine can spin passes through a priority round, so "was a player
prompted?" is always yes.

The rule has to be about **choices**, not prompts. That is also the reading CR
104.4b itself asks for: its second sentence exempts loops that contain an
optional action, and a prompt with one answer is not an optional action.

## Design

### 1. `GameState.lastChoice`

A new field beside `nextTimestamp`:

```haskell
-- CR 104.4b: the timestamp as of the last time a player was offered an optional
-- action. The gap between this and nextTimestamp is how many events have
-- happened with no player able to decide anything.
lastChoice :: Timestamp.Timestamp
```

Initialized to `MkTimestamp 0` wherever `nextTimestamp` is (`Setup.emptyGame`).
Two subgame sites need it set rather than copied, since a stale marker would
draw a game on entry for events that happened at another level:

- `Setup.subgameStateFrom` sets the subgame's `lastChoice` to the subgame's own
  starting `nextTimestamp`.
- `Setup.funnelBack` sets the parent's `lastChoice` to the merged
  `nextTimestamp`, the same line that maxes `nextTimestamp` today. A whole
  subgame's worth of events is not a stretch during which the parent's players
  could not act.

### 2. One choke point for prompting

`Pawl.Engine.Game` is the lowest module every prompt site already imports, so
the wrapper lives there:

```haskell
choose :: Prompt.Prompt a -> Game a
choose p = do
  State.modify' (\gs -> gs {GameState.lastChoice = GameState.nextTimestamp gs})
  Trans.lift (Program.prompt p)
```

Every `Trans.lift (Program.prompt …)` in the engine becomes `Game.choose …`,
with three sites deliberately left as bare prompts:

| Site | Why it does not reset |
|---|---|
| `Prompt.Concede` | CR 104.3a: conceding is something a player does *to* the game, not an action *in* it. If it reset the marker, no loop would ever be mandatory. |
| `Prompt.ChooseAction` with `legalActions == [Pass]` | Passing is not a decision. Only the `length actions > 1` branch resets. |
| `Prompt.RandomFirstPlayer` | CR 729.2's die roll is not a player's choice. |

Every other prompt site already elides when the answer is forced, so only the
branch that genuinely asks resets the marker.

### 3. The check

```haskell
-- Detecting a mandatory loop in general is the halting problem, so this is a
-- heuristic: draw a game whose events have repeated far past any point at which
-- a player could have interrupted them.
mandatoryLoopLimit :: Natural.Natural
mandatoryLoopLimit = 1000

checkMandatoryLoop :: Game ()
```

Sets `GameState.result = Just Result.Drawn` when the game has no result yet and
`nextTimestamp - lastChoice >= mandatoryLoopLimit`.

Called at two loop heads:

- `playGame`'s loop — covers the turn cycle and the extra-cleanup chain
- `priorityLoop`'s inner loop — covers the stack-resolution cycle

Both already read `GameState.result` at those points, so the draw unwinds along
the existing path rather than a new one. Nothing is checked mid-resolution,
where a half-applied effect would be observable.

### 4. Why the timestamp is the clock

CR 104.4b's own words are "repeating a sequence of **events**", and
`nextTimestamp` advances on exactly those: an object entering a zone (CR 613.7d)
and a continuous effect beginning (CR 613.7a). A quiet game issues roughly one
per turn, so a game that ends slowly by decking sits three orders of magnitude
below the threshold. A two-card recursion loop issues several per cycle and
trips in a few hundred.

A count of engine iterations was rejected for exactly that margin: a game in
which both players pass every step to decking visits a loop head on the order of
a thousand times, which is the same order as the threshold.

## Proving it

### The board

The canonical fixture — Worldgorger Dragon reanimated by Animate Dead — is far
out of reach. Animate Dead needs an Aura that enchants a card in a graveyard,
rewrites its own enchant clause on entry, re-attaches itself, and sacrifices the
creature on leave. Endless Whispers, the other textbook loop, needs a static
that grants a triggered ability, and `Modification` has no such arm.

The fixture is therefore **Aether Flash** (already in `data/cards/`) plus a small
real creature plus **one labeled synthetic** enchantment: "Whenever a creature
dies, return it to the battlefield under its owner's control." The creature
enters, takes 2, dies, returns, enters. Mandatory, no player choice, no life or
library drain, repeating forever — a genuine CR 104.4b loop with exactly one
card of synthetic surface, following the `synthetic-restart` /
`synthetic-subgame` precedent.

### The tests

Both in `Pawl.GameSpec`, both seeded with `nextTimestamp` already near
`mandatoryLoopLimit` so they cost tens of iterations rather than a thousand, and
both driven by an answer function that passes for a fixed number of prompts and
then concedes — so the *exempt* case terminates instead of hanging, which is
what the rule says it should do.

1. **Draws.** Empty hands, no lands, so every `ChooseAction` offers only `Pass`.
   `playGame` returns `Result.Drawn`, reached before the concession.
2. **Does not draw** — CR 104.4b's exemption. The same board plus one untapped
   Mountain under a player's control. Activating its mana ability is an optional
   action, so every priority round's menu has more than `Pass` and resets
   `lastChoice`. The loop runs past the threshold's worth of events untroubled,
   and the game ends `Result.Won` at the concession rather than `Drawn`.

The second test is the discriminator, and the two differ by exactly one
permanent. Without it the first passes for the wrong reason — a detector that
drew every game would satisfy it.

## Rejected alternatives

- **A configurable limit.** Threading a tuning knob through `GameState` or a
  reader environment buys nothing today: there is one caller and one sensible
  value. Revisit if a real game is ever found that trips it.
- **Repeated-state detection.** Hashing `GameState` and drawing on a repeat is
  the precise answer, but it is expensive on every loop head and fragile against
  fields that differ without mattering (event log, timestamps themselves).
- **Checking inside `Game.freshTimestamp`.** One line instead of two call sites,
  but it sets a result mid-resolution, where the surrounding code has not yet
  finished applying an effect.

## Out of scope

- The turn-side liveness half of #338 (a Stasis lock, a million-card deck). A
  game two players are willingly playing forever is not a mandatory loop.
- Retiring the synthetic. That waits on Worldgorger Dragon and Animate Dead, and
  gets its own issue with a card-driven expiry trigger.
