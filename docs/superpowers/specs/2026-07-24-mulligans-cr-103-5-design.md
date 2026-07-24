# Mulligans (CR 103.5) in the opening-hand path

*Design pass 2026-07-24. Closes GitHub issue #141 ("Mulligans (CR 103.4) are not
implemented"). This is a **phase-sized** spec: one new logic module, two new
prompt channels, and a restructure of the two setup entry points that draw
opening hands. Every rules claim is checked against `docs/rules.txt` and cited by
number. The next step is a `writing-plans` plan derived from this spec.*

## 0. Why this exists

`Setup.newGame` (the main game) and `Setup.startGameFromCards` (shared by restart
CR 727 and subgames CR 729) both draw an unconditional seven-card opening hand:

```haskell
Monad.replicateM_ openingHand (Event.drawCard pid)
```

There is no mulligan step, so no player may ever mulligan in a main game, a
restarted game, or a subgame. Both sites already carry a `(#141)` elision
comment. CR 729.3 explicitly name-checks mulligans ("regardless of any mulligans
that player takes"), and the CR 727.3 / 729.3 short-deck loss currently fires
without any mulligan opportunity having been offered.

The fix belongs in one shared place: both setup paths draw opening hands, and the
duplicated elision comment is exactly the drift this issue documents.

**The correct rule is CR 103.5, not 103.4.** 103.4 is the starting life total;
103.5 is the mulligan process. The issue title and both `Setup.hs` comments
mis-cite 103.4; `startGameFromCards`'s own header already cites 103.5 correctly.
The mis-citations are fixed silently as the elision comments are rewritten (per
the citation-fixes-need-no-reporting norm).

## 1. The rule (CR 103.5, verbatim ground truth)

> Each player draws a number of cards equal to their starting hand size, which is
> normally seven. (Some effects can modify a player's starting hand size.) A
> player who is dissatisfied with their initial hand may take a mulligan. First,
> the starting player declares whether they will take a mulligan. Then each other
> player in turn order does the same. Once each player has made a declaration, all
> players who decided to take mulligans do so at the same time. To take a
> mulligan, a player shuffles the cards in their hand back into their library,
> draws a new hand of cards equal to their starting hand size, then puts a number
> of those cards equal to the number of times that player has taken a mulligan on
> the bottom of their library in any order. Once a player chooses not to take a
> mulligan, the remaining cards become that player's opening hand, and that
> player may not take any further mulligans. This process is then repeated until
> no player takes a mulligan. A player can take mulligans until their opening hand
> would be zero cards, after which they may not take further mulligans.

Five load-bearing facts pulled from that text:

1. **Bottoming is part of taking the mulligan**, not a post-keep step. The player
   redraws a full hand and *then* bottoms `count` cards, where `count` is the
   number of mulligans they have taken so far. This is the "London mulligan" and
   differs from the older table procedure some readers remember — worth stating
   because it will look wrong from memory.
2. **Declare-all-then-take-all.** Every player declares (starting player first,
   then each other in turn order) before *any* mulligan is taken; then all who
   chose to mulligan do so "at the same time."
3. **Keeping is terminal.** Once a player keeps, their hand is their opening hand
   and they take no further mulligans.
4. **Repeat until a round has zero mulligans.**
5. **Zero-card floor.** A player may mulligan only while their opening hand would
   be more than zero cards.

## 2. Scope

**In scope.** The full two-player CR 103.5 process for all three opening-hand
paths (main game, restart, subgame), wired through one shared entry point.

**Explicitly deferred** (file each as a GitHub issue, cite `(#N)` at the code
site — never write an expiry into the comment):

- **CR 103.5c** — in a multiplayer game (one beginning with more than two players,
  CR 800.1) and in any Brawl game, the first mulligan a player takes doesn't count
  toward the bottom count or the mulligan limit. pawl does not model formats or
  variants, so the Brawl half has nowhere to read from; the multiplayer half is
  cheap but is deferred with the rest of the multiplayer backlog (#87, #143, #147).
  Labels: `gap, rules-correctness, area:multiplayer`. The two-player counter this
  spec builds carries this issue number where 103.5c would offset it.
- **CR 103.6** — opening-hand actions (Leyline of the Void's "begin the game with
  this on the battlefield", Gemstone Caverns). Adjacent to 103.5 but a separate
  mechanism with no producer today. Labels: `gap, expires:card-driven`.
- **CR 103.2a / 103.2b** — sideboards and companions set aside before the game.
  Adjacent to game start, out of scope for opening hands. Labels:
  `gap, expires:card-driven`.
- **Starting-hand-size modifiers** ("some effects can modify a player's starting
  hand size") — no producer exists; `openingHand` stays the constant seven. This
  is the same posture `startGameFromCards` already takes and needs no new issue
  beyond the note that the constant is deliberately not parameterized yet. If a
  card ever demands it, the constant becomes a per-player lookup; the round loop
  already reads hand size dynamically, so only the *target* seven would change.

**Out of scope, unchanged:** the CR 727.3 / 729.3 short-deck loss itself — it is
already implemented (a short library sets `drewFromEmpty` on the initial draw, and
`Sba.losesNow` reads it at the first upkeep). This spec must keep it working
through the mulligan path, not rebuild it.

## 3. Architecture

### 3.1 A new logic module, `Pawl.Mulligan`

Owns the entire CR 103.5 process. Exposes:

```haskell
-- The starting hand size (CR 103.5, "normally seven"). Moves here from Setup;
-- Pawl.hs's re-export updates from Setup.openingHand to Mulligan.openingHand.
openingHand :: Int

-- Run the whole CR 103.5 process for these players, in turn order. Assumes each
-- player's library is already built and shuffled. Draws initial hands, then runs
-- the declaration/mulligan round loop to completion. The single entry point both
-- setup paths call.
openingHands :: [PlayerId] -> Game ()
```

The module fits the codebase's small-logic-module norm (`Decide` is 17 lines,
`Departure` 59, `Monarch` 166) and keeps the prompt-driven 103.5 loop out of
`Setup`, which stays a pure state builder. It depends on `Event` (`drawCard`,
`changeZone`), `Game` (`zoneMembers`), `Decide` (`deciderFor`),
`Setup.shuffleLibrary`, and `Program.prompt` — the same seam `shuffleLibrary`
already uses.

> **Import-direction check.** `Setup` currently owns both `openingHand` and
> `shuffleLibrary`, and `Mulligan` needs both while `Setup` needs `openingHands`.
> That is a cycle. Resolve it by moving `shuffleLibrary` (and the `openingHand`
> constant) *into* `Mulligan`, so the dependency is one-way `Setup → Mulligan`.
> `shuffleLibrary` has one other caller, `Engine.playSubgame`'s reshuffle
> (`Engine.hs:613`), which already imports `Setup`; it changes to
> `Mulligan.shuffleLibrary`. Confirm no other `Setup.shuffleLibrary` callers
> exist before moving (grep at plan time). If moving `shuffleLibrary` proves
> awkward, the fallback is to keep it in `Setup` and have `Setup` pass it into
> `openingHands` as an argument — but the move is cleaner and is the default.

### 3.2 No `GameState` field

The per-player mulligan count lives in a local `Map PlayerId Natural` threaded
through the round loop, dying when setup ends. `GameState` stays clean, and the
three state-reset sites (`emptyGame`, `restartGame`, `subgameStateFrom`) gain no
new field to remember. This matches the rules: no in-game effect asks how many
mulligans a player took. (Serum Powder and CR 103.5c would want it observable;
both are deferred, and if either ever lands, promoting the local map to a field
is a localized change.)

### 3.3 Setup restructure

- **`newGame`** currently interleaves create → shuffle → draw *per player* in one
  `forM_`. CR 103.5 requires every library built and shuffled before any
  declaration (the initial draws are all part of one "draws a number of cards"
  step, and declarations follow in turn order over settled hands). Split into: a
  create+shuffle pass over the matchup, then `Mulligan.openingHands owners`.
- **`startGameFromCards`** already builds every library in one `State.put` before
  its draw loop, so only the `forM_ owners (... shuffle ... draw ...)` loop is
  replaced. Shuffle stays per-player (order among shuffles is unobservable), then
  `Mulligan.openingHands owners` runs once over all owners.

## 4. The two prompts

Two constructors on `Prompt` (GADT), each carrying a `Decider` like every other
player-facing prompt. `Decide.deciderFor pid gs` supplies it; at setup
`activeControl` is `Nothing`, so it degenerates to the player themselves and CR
723 is satisfied for free — the seam is ready if a card ever controls a player
during setup.

```haskell
-- CR 103.5: whether this player takes a mulligan. The Natural is how many
-- mulligans they have ALREADY taken (0 on the first round), so an interpreter can
-- show "you will bottom N if you mulligan". Asked in turn order, once per round,
-- only while the player's current hand is > 0 cards (CR 103.5 final sentence: no
-- mulligans past a zero-card hand). A kept player is never asked again (CR 103.5:
-- keeping is terminal).
DeclareMulligan :: Decider -> PlayerId -> Natural -> Prompt MulliganDecision

-- CR 103.5: after redrawing, put `count` cards from `hand` on the bottom of the
-- library, in the player's chosen order. The [ObjectId] is the redrawn hand; the
-- answer is an ordered list of exactly `count` of those ids (first-listed ends up
-- higher in the library, i.e. drawn sooner). Bottom order IS future draw order,
-- so it is a real choice even when the subset is forced (count == hand size).
-- Asked only when the hand has >= 2 cards; with 0 or 1 there is exactly one
-- possible outcome, and where the rules leave nothing to ask, don't prompt.
Bottom :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
```

New types (one per module, per conventions):

- `Pawl.Type.MulliganDecision` — `data MulliganDecision = Mulligan | Keep`
  deriving `(Eq, Show)`. A sum type, not `Bool` (no boolean blindness).
- `Response` gains `DeclaredMulligan MulliganDecision` and `PutOnBottom [ObjectId]`.

**Why the `Bottom` answer is `[ObjectId]`, never `Set`:** the order the cards go
to the bottom sets the order they are later drawn. It is a distinguishable choice
even when *which* cards are forced, so the answer must preserve order.

### 4.1 Replay integration

`Pawl.Replay` gains the four mechanical additions:

- `encode`: `DeclareMulligan {} -> Response.DeclaredMulligan answer`;
  `Bottom {} -> Response.PutOnBottom answer`.
- `decode`: the two matching branches (wildcard for the rest, as today).
- `defaultAnswer`:
  - `DeclareMulligan _ _ _ -> MulliganDecision.Keep` — keeping is always legal and
    the least-eventful fallback when a transcript runs short (mirrors
    `Concede -> Continues`).
  - `Bottom _ _ hand count -> take (fromIntegral count) hand` — a legal ordered
    subset of the redrawn hand; least-eventful.

## 5. Control flow of `openingHands`

```
openingHands owners:
  1. Draw pass: every owner draws `openingHand` cards (their first hand,
     CR 103.5 sentence 1). A short library sets drewFromEmpty here; that flag
     survives every step below — which is exactly what CR 727.3 / 729.3's
     "regardless of any mulligans" means.
  2. counts := empty   (Map PlayerId Natural; absent key = 0)
     deciding := owners   (players who have NOT yet kept — see the keep-terminal
     note below; keeping is terminal, CR 103.5, so a kept player leaves this pool)
  3. Round loop (repeats until a round produces zero mulligans — CR 103.5
     "repeated until no player takes a mulligan"):
       a. Declaration sub-pass, `deciding` in TURN ORDER (it is filtered from the
          original `owners`, so turn order is preserved): for each still-deciding
          player whose current hand size > 0, prompt DeclareMulligan (decider, pid,
          counts[pid]); collect Mulligan/Keep. A player with a 0-card hand is not
          asked and is treated as Keep (CR 103.5 final sentence).
       b. If every collected decision is Keep -> STOP; current hands are the
          opening hands.
       c. Simultaneous mulligans (CR 103.5 "all players who decided ... at the same
          time"), for each still-deciding player who chose Mulligan, in turn order:
            - shuffle the hand back into the library: changeZone every hand card to
              Zone.Library, then shuffleLibrary pid;
            - draw `openingHand` fresh cards;
            - counts[pid] += 1;
            - bottom N = counts[pid]: let handSize = current hand size,
              N' = min(N, handSize);
                * if handSize >= 2: prompt Bottom (decider, pid, hand, N') and
                  changeZone the answered ids to Zone.Library IN ORDER;
                * if handSize <= 1: bottom the whole hand (0 or 1 card) with no
                  prompt.
       d. Repeat the round over the mulliganers only (`deciding := this round's
          mulliganers`): everyone who kept has dropped out.
```

### 5.1 Fidelity notes

- **Keeping is terminal (CR 103.5: "that player may not take any further
  mulligans").** The loop must recurse only over the still-deciding players, NOT
  re-ask everyone each round — a player who keeps drops out of the pool
  permanently. An earlier draft of this control flow (and the plan derived from
  it) re-asked every player whose hand was > 0, which would let a player who kept
  in round 1 illegally mulligan in round 2 once an opponent kept the round alive;
  the Task 3 review caught it. The fix cannot be pushed onto the interpreter: the
  DeclareMulligan prompt carries only `counts[pid]`, and `counts[pid] == 0` cannot
  distinguish "never decided" from "kept immediately," so a stateless decider
  cannot tell it already kept. The engine threads the pool.
- **Declare-all-then-take-all** (3a fully before 3c): pawl is sequential, so CR
  103.5's "at the same time" is modeled as *collect every declaration, then apply
  every mulligan*. Because a hand is hidden information, no player's redraw is
  observable to another player's still-pending declaration in the same round, so
  this is observably equivalent to true simultaneity (the observable-equivalence
  bar).
- **Bottoming with no new primitive.** `changeZone Hand → Library` appends to the
  bottom (`Game.insertIntoZone`'s `flip (Seq.><)`), and `Event.drawCard` takes the
  top. Replaying the answered list in order therefore reproduces the chosen bottom
  order, using only the existing zone machinery.
- **Termination.** The zero-card floor (3a's `hand size > 0` guard) bounds the
  loop: each mulligan bottoms at least `counts[pid]` cards, so a player's opening
  hand strictly shrinks once counts exceed the hand's growth, and a 0-card hand is
  never asked. The round loop ends the first round everyone keeps, which is forced
  once every player has bottomed down to zero.
- **CR 103.5 sentence "draws a new hand ... equal to their starting hand size":**
  the redraw is a full `openingHand` draw even on later mulligans; the reduction
  is entirely in the bottoming count, never in the draw count.

## 6. Testing (`Pawl.MulliganSpec`, new; wired into `Main.hs`)

Real fixtures over synthetic ones (tests-prefer-real-cards): Mountain/Piker over a
`Setup.mirror` deck, as the existing setup tests do.

1. **Keep-first is a no-op.** An all-`Keep` interpreter reproduces today's
   behavior: every player draws exactly 7, library shrinks by exactly 7. Pins that
   a zero-mulligan round loop equals the old unconditional draw — guards the
   `newGame` restructure.
2. **One mulligan bottoms one.** A "mulligan once, then keep" interpreter →
   redrawn 7, bottomed 1, opening hand of 6; assert the bottomed card is at the
   library bottom (drawn last).
3. **Two mulligans bottom two, in chosen order.** Distinguishes the ordered
   `[ObjectId]` answer: assert the two bottomed cards land in the answered order,
   i.e. they are the next two draws in that order.
4. **Zero-card floor (CR 103.5 final sentence).** A player mulliganing down to a
   0-card opening hand is not prompted further and the loop terminates.
5. **`Bottom` elision.** A hand of ≤ 1 card issues no `Bottom` prompt — assert via
   a recording interpreter or a DecisionLog length.
6. **Replay determinism.** A `record` / `replay` round-trip over a game containing
   mulligans reproduces the final state byte-identically (extends the `ReplaySpec`
   pattern; this is why both prompts serialize).
7. **Short-deck loss survives mulligans (CR 727.3 / 729.3).** A ≤ 6-card library
   still loses at the first upkeep after mulligans were offered — assert
   `drewFromEmpty` is set through the mulligan path. Likely a tweak to the existing
   `SetupSpec` / `GameSpec` short-deck assertions rather than net-new tests.

The exhaustive-`Prompt` answerers that gain the two new arms (all answering Keep /
take-N) — 6 interpreters in `Support.hs`, 3 in `benchmark/Main.hs`, and the
`CastSpec` / `GameSpec` locals — are mechanical; `-Werror` turns each omission
into a compile error, so none can be silently missed.

## 7. Blast radius

- **New:** `Pawl.Type.MulliganDecision`, `Pawl.Mulligan`, `Pawl.MulliganSpec`
  (added to test-suite `other-modules` and `Main.hs`'s `testTree`).
- **Modified:** `Pawl.Type.Prompt` (+2 constructors), `Pawl.Type.Response`
  (+2 constructors), `Pawl.Replay` (encode/decode/defaultAnswer),
  `Pawl.Setup` (`newGame` restructure; `startGameFromCards` draw-loop swap;
  `shuffleLibrary` + `openingHand` move out), `Pawl.hs` (re-export update),
  `Pawl.Engine` (`playSubgame` reshuffle → `Mulligan.shuffleLibrary`), every
  exhaustive `Prompt` answerer (Support, benchmark, CastSpec, GameSpec locals).
- **Comments:** the `#141` elision notes in `Setup.hs` are removed; the incorrect
  `CR 103.4` citations are corrected to CR 103.5 in the same edit. The
  `Setup.openingHand` reference in `PlayerEffect.hs:180`'s comment is repointed to
  `Mulligan.openingHand`.

## 8. Definition of done

1. `cabal build all --enable-tests --enable-benchmarks` is warning-clean under
   `+pedantic`.
2. `hooky fix` applied, `hooky run` passes; HLint clean.
3. `Pawl.MulliganSpec` passes; the CR 727.3 / 729.3 short-deck tests still pass.
4. Every rules claim cites CR 103.5 (or the specific sub-rule) and was checked
   against `docs/rules.txt`.
5. Issues filed for the CR 103.5c, 103.6, and 103.2a/b deferrals; `(#N)` citations
   at the code sites.
6. Issue #141 closed; `progress.md` gains a completion entry; the `Setup.hs`
   elision comments are gone.
