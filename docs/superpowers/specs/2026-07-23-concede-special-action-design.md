# Concede — the special action (CR 104.3a / 405.6g)

**Issue:** #133. **Status:** design approved 2026-07-23, not yet implemented.

Every CR number below was checked against `docs/rules.txt` while writing this
document, not recalled.

## 0. Why this is hard

`Pawl.Type.Prompt` is a *pull* channel. The engine learns a player's intent only
by issuing a prompt and blocking on the answer; the `Program` / `Replay`
interpreter is driven by the engine, never by the player. CR 104.3a says

> A player can concede the game at any time. A player who concedes leaves the
> game immediately. That player loses the game.

"At any time" means *asynchronously, between any two engine actions*. A pull
channel cannot express that. So "at any time" necessarily becomes "at every
point the engine stops to ask", and the whole design is a choice of which points
those are.

Two things that sound hard are not:

- **"Can't be responded to."** Concede does not use the stack, so there is
  nothing to respond to. The engine models this by applying it immediately and
  pushing nothing. No machinery.
- **The consequence.** `Departure.Conceded` already exists (never constructed),
  and `Pawl.Sba` already computes `leaving` / `departed` / `remaining` and sets
  `Status.Departed`. What is missing is the *channel*, not the effect.

The genuinely hard part is CR 723.6.

## 1. CR 723.6 is what rules out the obvious design

The obvious design is a new `Action.Concede` in `Action.legalActions`. It is
wrong, and not for a subtle reason.

`Engine.priorityLoop` prompts `Prompt.ChooseAction (Decide.deciderFor p gs) p
actions`. Under Mindslaver, `deciderFor bob` is alice — so when alice controls
bob, **bob is never asked anything at all**. CR 723.6 says a controller may not
make the controlled player concede, but the controlled player may still concede
themselves. An `Action.Concede` would hand alice exactly the power the rule
forbids, and give bob no channel whatsoever.

Filtering a bad answer ("reject `Concede` when `decider /= p`") does not fix it:
bob still has no way to say anything. The fix must be a *separate ask keyed to
the true player*. That requirement, not prompt aesthetics, is what forces a new
prompt constructor.

## 2. The channel

```haskell
-- Pawl.Type.Concession
data Concession = Concedes | Continues

-- Pawl.Type.Prompt
Concede :: PlayerId -> Prompt Concession
```

**`Prompt.Concede` deliberately carries no `Decider`.** Every other constructor
in the GADT is `Decider -> PlayerId -> …`. This asymmetry is the CR 723.6
mechanism: there is nowhere to put a controller, so no interpreter and no future
edit can route a concession through one. The guard comment presently at the
`ChooseAction` site in `Engine.priorityLoop` is deleted in the same commit — the
type now enforces what that comment asked readers to remember.

`Concession` is a sum type rather than a `Bool` per CLAUDE.md: the two states are
an outcome, not a predicate.

## 3. Where it is polled

In `Engine.priorityLoop`'s inner loop, immediately before the existing
`ChooseAction`, keyed to the true player `p` and never to `decider`:

```haskell
Just p -> do
  concession <- Trans.lift (Program.prompt (Prompt.Concede p))
  case concession of
    Concession.Concedes -> do
      Departure.leaveGame Departure.Conceded p
      State.modify' (\g -> g {GameState.priority = Just (nextStillPlaying g p)})
      loop
    Concession.Continues -> …the existing ChooseAction path…
```

Polled *before* legal actions are computed, so a player may concede regardless of
what they could otherwise do.

The re-`loop` is dead code at two players — `leaveGame` sets a result, and the
loop's existing `finished` check unwinds on the next iteration — but it is the
correct multiplayer continuation and costs one line.

## 4. The consequence: a new `Pawl.Departure`

CR 104.3a (concede) is **immediate**. CR 104.3b (life ≤ 0) is a **state-based
action**. The rules draw that line deliberately, and the code should not blur it
by having `Engine` call into `Pawl.Sba` for something that is not a state-based
action.

So the departure machinery moves out of `Pawl.Sba` into `Pawl.Departure`, beside
`Pawl.Type.Departure` exactly as `Pawl.Expiry` / `Pawl.Filter` pair with their
types:

```haskell
stillPlaying        :: GameState -> [PlayerId]
depart              :: Departure -> PlayerId -> GameState -> GameState
outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result
leaveGame           :: Departure -> PlayerId -> Game ()
```

- `depart` gains its reason. It hardcodes `Departure.Lost` today, which is why
  `Departure.Conceded` has never been constructed.
- `outcomeAfterLeaving leaving gs` is CR 104.2a ("a player still in the game wins
  if that player's opponents have all left"), lifted verbatim out of `Sba`'s
  inline `outcome`. `gs` is the state *after* the departures have been applied,
  and it reads the survivors as `stillPlaying gs`; `leaving` is who just left, and
  is needed only to distinguish "nobody is playing because everyone left
  simultaneously" (a draw) from "nobody was playing to begin with" (no result) —
  the `[] -> if null leaving then Nothing else Drawn` arm. A single concede passes
  `[p]`.
- `leaveGame` is the immediate door: depart, then set the result **now** rather
  than at the next SBA pass. It preserves `Sba`'s existing
  `outcome <|> existing` precedence so an already-decided result is never
  overwritten.

`Pawl.Sba` imports `Pawl.Departure` and calls `depart Departure.Lost`. Five call
sites of `stillPlaying` across `Target`, `Engine`, and `Combat` re-point.

## 5. What falls out for free

- **Unwinding.** `GameState.result` is *already* the signal `priorityLoop`,
  `runStep`, and `playGame` all check. Unlike the CR 727.4 restart (#134), no new
  transient signal is needed — setting the result is sufficient and the existing
  paths do the rest.
- **CR 729 subgames.** A subgame runs its own `playGame` over its own
  `GameState`, so conceding a subgame concedes the subgame and leaves the main
  game alone.
- **CR 405.6g** is a cross-reference to 104.3a; it needs nothing of its own.

## 6. Cost, stated plainly

Every prompt has a `Response` arm, two `Replay` arms (record and fold), and a
`Codec` arm. This change therefore touches: `Pawl.Type.Concession` (new),
`Pawl.Type.Prompt`, `Pawl.Type.Response`, `Pawl.Replay` ×3 (record, fold, and the
deterministic short-transcript fallback, which answers `Continues` as the least
eventful choice), `Pawl.Codec` ×2, `Pawl.Departure` (new), `Pawl.Sba`,
`Pawl.Engine`, plus `Pawl.Support` and every spec-local and benchmark answer
function.

Prompt volume roughly doubles — the benchmark games issue 650–1800 `ChooseAction`
prompts — and replay logs grow accordingly. This is the honest price. Conceding
is a *distinguishable* option, so pawl's own invariant forbids eliding the ask;
only the set of points at which it is offered may be narrowed, which is §7.

**Reversibility.** The cost above is sunk into the *channel*, not the
*frequency*, and the two are worth keeping apart. Every item in the list — the
GADT arm, `Response`, both `Codec` arms, all three `Replay` arms, the arm in each
answer function — is frequency-independent: an answer function does not care
whether it is called six times or six hundred. The frequency lives at exactly one
site, the poll in `Engine.priorityLoop`. Narrowing it later (only when the player
is controlled, only at certain steps) is moving or guarding that one call, and
nothing else changes. What is *not* cheap is deleting `Prompt.Concede` and
folding concede back into `ChooseAction`, which re-touches everything and reopens
the CR 723.6 hole — but that is the one outcome this design exists to prevent, so
the irreversibility points the right way.

One caveat with a deadline: `Replay.replay` is **positional**, consuming
`[Response]` in order. Reducing the frequency later invalidates any transcript
recorded at the old frequency — the engine would ask fewer prompts than the log
has entries, and `decode` failing on a mismatched entry falls through to
`defaultAnswer` rather than resyncing. That costs nothing today, because nothing
persists a transcript (#126: no `GameState` / `Object` / `Source` codec) and
CLAUDE.md disclaims API stability. It becomes a real migration cost once a
save/replay feature ships. Tuning the volume is therefore free before #126 lands
and not after.

## 7. The one elision: "at any time" becomes "at each priority grant"

A player may want to concede while an **opponent** holds priority: they have
realised they cannot win, or the opponent has just revealed something decisive.
The engine does not offer that.

This is largely a client concern rather than an engine one. A client can record
the player's intent the moment they express it, and the engine can apply the
buffered concession when that player next holds priority. The channel in §2 needs
no change to support that.

It is **not**, however, strictly outcome-preserving, and the tracking issue must
say so. `settleForPriority` runs state-based actions before every priority grant.
If the opponent is themselves about to lose inside that window — at 1 life under
an upkeep trigger, or about to draw from an empty library — then:

- conceding *immediately* makes that opponent the winner (CR 104.2a); but
- conceding at the conceder's *next* priority lets the opponent depart first, and
  the conceder wins, or the game is drawn.

Narrow, but it decides the game rather than merely delaying it. The elision is
therefore a real rules gap, not latency, and must not be filed as cosmetic.

**Expiry trigger:** a card or subsystem that requires conceding outside one's own
priority, or any client that surfaces the race above.

## 8. Also out of scope

- **CR 800.4** — objects leaving, ceasing to exist, or changing control when a
  player leaves a multiplayer game. Two-player pawl never observes it; related to
  #87 (CR 800.4g), already labelled `expires:subsystem`.
- **CR 104.3c–104.3j** and the other loss conditions. Untouched.

## 9. Tests — the gate

Concede is a special action, not a card, so the gate is gameplay-level rather
than a gate card.

1. **CR 723.6 — the central test.** alice controls bob via Mindslaver; bob
   concedes on his own controlled turn. Proves the concession reaches bob and
   that alice cannot answer it. This is the test the whole design exists for.
2. **CR 104.3a / 104.2a.** A concede ends the game immediately with the opponent
   winning.
3. **`Departure.Conceded` is recorded**, distinct from `Departure.Lost` — the
   constructor stops being dead.
4. **Not on the stack.** Conceding with a spell on the stack ends the game
   without resolving it.

## 10. Exit criterion

A player may concede at any priority they hold; a controlled player may concede
themselves while their controller cannot concede for them; the conceder leaves
immediately with `Departure.Conceded`, and their opponent wins by CR 104.2a
without waiting for a state-based action check. The `Action.Concede` design is
foreclosed by the type. #133 closes; the "at any time" elision is filed with the
race in §7 recorded.
