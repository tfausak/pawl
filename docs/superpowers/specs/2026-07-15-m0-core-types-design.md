# M0 core types — design

Design for the foundational types of milestone **M0** ("a complete game with zero
cards", per `docs/design.md`). Scope is deliberately the three interlocking
type families that are expensive to change later: the **entity-identity model**,
the **`GameState`**, and the **suspension monad** (`Program Prompt`).

This is a types-and-architecture spec, not an implementation plan. Field-level
minutiae and the module tree will firm up during planning.

## M0 goal and scope

A complete game of 60 Mountains vs. 60 Mountains: draw each turn, play up to one
land, pass through the phases, deck out. No effects, no oracle text, no card
vocabulary.

**A pure-Mountains game never uses the stack** — lands are special actions
(CR 116.2a) and a Mountain's mana ability doesn't use the stack (605.3). So M0
builds the `stack` field but exercises it *empty*. What M0 actually exercises:

- turn structure and the phase/step machinery (CR 500–514),
- the priority / pass / advance state machine (CR 117),
- turn-based actions: untap, draw (starting player skips their first, 103.7a),
  and cleanup's discard-to-hand-size (7),
- the land special action (≤ 1 per turn),
- state-based actions (CR 704): life ≤ 0, drew-from-empty-library, game end,
- the draw-from-empty-library loss condition.

**Exit criterion:** a game completes and produces a `DecisionLog` that replays
deterministically.

## Conventions

- **Non-punning constructors.** `newtype`s and single-constructor records use a
  `Mk` prefix (`newtype ObjectId = MkObjectId Int`, `data Object = MkObject {…}`).
  Smart-constructor newtypes that guard an invariant use the `Unsafe` prefix
  instead. Multi-constructor ADTs don't pun and are written plainly.
- **Boot libraries only** (plus `random`). Consequences below: the operational
  monad is hand-rolled rather than from `free`/`operational`; ordered zones use
  `Data.Sequence` rather than `vector`.
- Every type derives at least `Eq` and `Show` (omitted from sketches for brevity).

## 1. Entity identity

Forced by CR 400.7 (an object changing zones becomes a *new* object with no
memory) plus targeting legality (608.2b): **every game object gets an `ObjectId`
minted from a monotonic counter every time it comes into existence, including
every zone change.** The Mountain in the graveyard is a different object from the
Mountain that was on the battlefield.

This makes two hard things fall out for free:
- **Targeting legality** — a stale `ObjectId` from before a creature left simply
  doesn't resolve to the current object.
- **Last known information** — an older `Object` value someone kept a reference
  to (Haskell sharing); no defensive copying.

```hs
newtype ObjectId = MkObjectId Natural   -- monotonic, unsigned; a Map key (not IntMap) for clarity & type safety
newtype PlayerId = MkPlayerId Natural

data Object = MkObject
  { owner  :: PlayerId   -- persists across incarnations (tokens & emblems too)
  , source :: Source     -- where base characteristics come from
  , zone   :: Zone
  , tapped :: TapState    -- a real type, not Bool
  }
  -- damage, counters, controller, etc. are additive fields for later milestones

data TapState = Untapped | Tapped   -- type name differs from the Tapped constructor
data Zone     = Library | Hand | Graveyard | Battlefield | Stack | Exile  -- command zone: later

-- The source of an object's base characteristics. A sum, not a bare Printing,
-- so tokens / copies / emblems / abilities-on-the-stack all fit by adding cases.
data Source
  = OfCard Printing     -- the only case M0 needs
  -- OfToken Token       -- effect-defined characteristics
  -- OfCopy  Copy
  -- OfEmblem Emblem     -- abilities only; lives in the command zone
  -- OfAbility Ability   -- an activated/triggered ability on the stack
```

**Counters are not objects** — they're markers *on* objects (and players), so
they become a field (`Map CounterKind Natural`) when effects arrive, never a
`Source` case. **Emblems and abilities on the stack _are_ objects** — new `Source`
cases; an ability on the stack is just another `Object` in the `stack` zone,
which is why the central store (below) pays off.

**Deferred: a stable `CardId`.** A physical-card identity distinct from the
ephemeral `ObjectId` is not needed for M0. Ownership persists as a field; "what
card is this" is the `Printing`; an effect that follows a card across a zone
change just holds the destination `ObjectId` (the new object). A `CardId` index
only earns its keep well past M0 (commander identity, leaves-the-game,
cast-this-specific-card-from-exile) and is purely additive to add then.

`Printing` and `Card` follow the §2.8 split: characteristics live on the atomic
`Card`; the `Printing` wraps it with printing-specific metadata (set, artist,
collector number) that objects carry but the rules don't treat as
characteristics. For M0 both are minimal — enough to be a Mountain:

```hs
data Printing = MkPrinting
  { card :: Card }
  -- set, artist, collectorNumber, …: printing metadata, unused in M0

data Card = MkCard
  { name     :: Text        -- "Mountain"
  , typeLine :: TypeLine
  }
  -- manaCost, power/toughness, colors, rules text, abilities: added as milestones need them

-- Minimal for "Basic Land — Mountain"; the full taxonomy grows.
data TypeLine = MkTypeLine
  { supertypes :: Set Supertype   -- { Basic }
  , types      :: Set CardType    -- { Land }
  , subtypes   :: Set Subtype     -- { Mountain }
  }

data Supertype = Basic        -- grows: Legendary, Snow, World, …
data CardType  = Land         -- grows: Creature, Instant, Sorcery, …
data Subtype   = Mountain     -- grows: other land types, creature types, …
```

A Mountain carries no rules text: its red mana ability is granted from
`subtypes ∋ Mountain` by CR 305.6 — derived by the engine (closed half), not
stored on the card. So the M0 card data really is just a name and a type line.
The full-arity `faces :: NonEmpty Face` + layout shape (§2.11) is out of M0 scope.

## 2. GameState

A **central object store** keyed by id; zones hold only ordered/unordered ids.
Resolving an arbitrary `ObjectId` (a target, a trigger's saved reference) is a
single O(log n) lookup and uniform, and the base/projected split stays clean —
one place to look up an object, one place the future layer pass reads from. Zones
are pure topology.

```hs
data GameState = MkGameState
  { objects      :: Map ObjectId Object          -- every live object (Data.Map.Strict)
  , library      :: Map PlayerId (Seq ObjectId)  -- per player, ordered
  , hand         :: Map PlayerId (Seq ObjectId)  -- per player
  , graveyard    :: Map PlayerId (Seq ObjectId)  -- per player, ordered (top matters, rarely)
  , battlefield  :: Set ObjectId                 -- shared, unordered
  , stack        :: [ObjectId]                   -- LIFO; empty throughout M0
  , exile        :: Set ObjectId
  , players      :: Map PlayerId Player
  , turnOrder    :: [PlayerId]               -- APNAP base order (multiplayer-ready)
  , activePlayer :: PlayerId
  , phase        :: Phase
  , priority     :: Maybe PlayerId           -- Nothing in no-priority steps
  , passes       :: Natural                  -- consecutive passes since the last action
  , turnNumber   :: Natural
  , result       :: Maybe Result             -- Nothing = game ongoing
  , nextObjectId :: ObjectId
  }
```

**Zone structures** (boot-only; `vector` excluded, `array` is wrong for
per-turn-resized zones):

| Zone | Structure | Why |
|---|---|---|
| stack | `[ObjectId]` | genuinely LIFO — a list is correct, not a compromise |
| library, hand, graveyard | `Seq ObjectId` | `Data.Sequence` (containers): O(1) both ends, O(log n) split/index |
| battlefield, exile | `Set ObjectId` | unordered |
| id-keyed maps (`objects`, per-player zones, `players`) | `Data.Map.Strict`, keyed by the `newtype` id | type-safe keys — can't index by the wrong id kind; ids can be `Natural`. `IntMap`+`Int` is a proven-perf fallback |

Caveat: at M0's scale (`n` ≈ 60) `Seq`'s constant factor may lose to a plain
list. `docs/design.md`'s risk register already mandates profiling the
goldfish loop at M0; `Seq` is the principled default and the profile decides.
It's a one-line change behind the zone accessors either way.

The id-keyed maps use `Data.Map.Strict` keyed by the `newtype` ids rather than
`IntMap`, by the same "clarity first, optimize on evidence" logic: the key type
stays distinct (you can't index the object store with a `PlayerId`) and the ids
can be `Natural`, a more principled start than a raw `Int`. `IntMap` with `Int`
ids is the documented fallback if profiling shows map lookups dominate — note
that switch would also diverge from `design.md`'s risk register, which currently
assumes `Data.IntMap.Strict`.

### Object lifecycle invariant

`objects` holds *exactly* the objects that currently exist in some zone — it is
maintained by construction, never by a garbage-collection sweep. Every zone
transition removes the defunct entry (400.7: the object ceases) and, for a move,
mints a fresh-id object at the destination. So the map is bounded by the objects
concurrently in play, not by game history.

All transitions go through **one `changeZone` primitive** (the "atom" pattern
from the prior-art notes) that owns remove-mint-insert and emits the zone-change
event. One choke point keeps the invariant from drifting. Consequences:

- **Dangling `ObjectId`s** held by effects (a stored target whose creature died)
  are intentional — a stale id just `lookup`s to `Nothing` = "it's gone," which
  is how 608.2b targeting legality and left-the-zone detection work. We never
  chase them down.
- **LKI** is a retained `Object` *value* held by whatever needs it (a dying
  trigger's snapshot), reclaimed by GHC once dropped; it never lives in
  `objects`.

For M0 the transitions are draw (library→hand), play-land (hand→battlefield),
discard-to-hand-size (hand→graveyard), and deck-out (the failed draw).

### Turn position

Nested `Phase`/step ADTs (the rules distinguish phase from step constantly —
triggers "at beginning of upkeep," durations "until end of turn/phase"):

```hs
data Phase
  = Beginning BeginningStep
  | PrecombatMain
  | Combat CombatStep
  | PostcombatMain
  | Ending EndingStep

data BeginningStep = Untap | Upkeep | DrawStep
data CombatStep    = BeginningOfCombat | DeclareAttackers | DeclareBlockers | CombatDamage | EndOfCombat
data EndingStep    = EndStep | Cleanup
```

A `next :: Phase -> Maybe Phase` encodes the CR 500–514 ordering once and returns
`Nothing` at the turn boundary (after `Ending Cleanup`) instead of silently
wrapping — so the caller cannot miss end-of-turn, which carries its own
bookkeeping (rotate the active player through `turnOrder`, bump `turnNumber`,
reset to `firstPhase`). The `Combat` phase runs in M0 with no attackers.

```hs
next :: Phase -> Maybe Phase   -- Nothing = the turn ended
firstPhase :: Phase            -- restart point, referenced by setup and each new turn
firstPhase = Beginning Untap

-- turn loop, in spirit:
--   case next (phase gs) of
--     Just p  -> gs { phase = p }
--     Nothing -> beginNewTurn gs   -- rotate activePlayer, bump turnNumber, phase = firstPhase
```

`Maybe` is a light sentinel; a custom `data Advance = ToPhase Phase | TurnEnds`
would be marginally more self-documenting if preferred. Future wrinkle, not M0:
`Ending Cleanup` can loop back into another cleanup step when SBAs/triggers occur
(514.3) — a state-dependent choice made at exactly this boundary, another reason
not to auto-wrap.

### Priority loop

Enter a step → run its turn-based actions → if the step grants priority, the
active player receives it → players act or pass. Any action resets `passes` and
returns priority to the active player. When `passes` reaches the player count
with an **empty stack**, advance to the next step; with a non-empty stack (never
in M0), resolve the top and reset. Untap and (usually) Cleanup grant no priority
(`priority = Nothing`).

### Players, status, and results

Player-departure and game-outcome are **two different things** (CR 104), so they
are two types — this is what lets the design represent draws.

```hs
data Player = MkPlayer
  { life   :: Integer   -- arbitrary precision, goes ±; widens to Rational with the numeric tower (Little Girl ½) — see Deferred
  , status :: Status
  }

data Status    = Playing | Departed Departure   -- Departed, not Left (Data.Either)
data Departure = Lost | Conceded | Drew         -- the ways a player leaves (104.3)

data Result = Won PlayerId | Drawn              -- the game outcome (104.4)
```

The two-level split buys: normal win/loss, multiplayer last-player-standing,
individual concede/draw leaving the game while it continues (a player can be
`Departed Drew` while `result` is still `Nothing`), the 104.4a simultaneous-loss
draw, explicit "the game is a draw" effects, and the 104.4b mandatory-loop draw.

### State-based actions (M0 subset)

An SBA pass, run at each priority grant (CR 704.3):

1. A player at `life ≤ 0` departs `Lost` (704.5a).
2. A player who attempted to draw from an empty library departs `Lost` — the
   draw action sets a pending marker the SBA consumes (704.5c / 120.3).
3. **Game end:** batch every player who departed in this pass (they leave
   simultaneously, 104.4a); then if one player remains set `result = Won them`,
   if zero remain set `result = Drawn`.

Creature/token SBAs (lethal damage, 0 toughness, token-ceases-to-exist) are M1+.

## 3. The suspension monad

The engine is a pure function; everything it cannot decide for itself becomes a
suspension. One pure core, swappable interpreters.

### Instruction set (`Prompt`)

M0 needs two constructors. Player choices carry both the `PlayerId` whose action
it is (resource owner) and the `Decider` who actually chooses (input authority,
CR 722):

```hs
data Prompt r where
  ChooseAction :: Decider -> PlayerId -> [Action] -> Prompt Action
  Shuffle      :: [ObjectId] -> Prompt [ObjectId]

data Action = Pass | Play ObjectId   -- M0: pass, or play a land. Grows: Cast, Activate…
```

`Shuffle` stays a prompt because library order **is** hidden information — it is
the seam Monte Carlo Tree Search (MCTS) determinization rides on (the search
harness samples a consistent order; replay reads the recorded one). `ChooseTargets`, `OrderTriggers`,
`OrderGraveyard` arrive with the milestones that need them.

**Randomness split.** Genuine hidden-information randomness (`Shuffle`) is a
prompt. Pure chance with no hidden information (coin flip, die roll) will be a
**seeded RNG threaded in `GameState`** (`random`'s `StdGen`), recorded once as a
seed in the `DecisionLog` — *not* a prompt. Neither coin nor die occurs in M0, so
the RNG field is added with the first card that needs it (YAGNI); the split is
decided now.

### Decider ≠ player (CR 722)

```hs
newtype Decider = MkDecider PlayerId
deciderFor :: PlayerId -> Game Decider   -- M0: pure identity; consults control effects later
```

The interpreter routes a prompt to the `Decider`'s client; the decision is
attributed to the `PlayerId`'s resources. In M0 `deciderFor p = MkDecider p`
always. Building this seam now is what makes Mindslaver/Word of Command an
addition rather than a rewrite.

### The operational monad (hand-rolled, boot-only)

No `free`/`operational` dependency — ~15 lines over `GADTs`:

```hs
data Program instr a where
  Return :: a -> Program instr a
  Then   :: instr b -> (b -> Program instr a) -> Program instr a
-- Monad: Return a >>= f = f a ; Then i k >>= f = Then i (\b -> k b >>= f)
-- prompt i = Then i Return

type Game = StateT GameState (Program Prompt)   -- StateT from transformers (boot)
```

The engine is pure `Game a`; a choice is `prompt (ChooseAction …)`. Suspension is
automatic — `Then` holds the continuation.

### Interpreter seam

```hs
-- answer any prompt in some effect m  (needs RankNTypes)
runGame :: Monad m => (forall r. Prompt r -> m r) -> GameState -> Game a -> m (a, GameState)
```

- Humans: `m = IO`, ask the client.
- Replay / tests: `m = State DecisionLog`, pop the recorded response.
- MCTS: hold the `Then i k` continuation and invoke `k` with different responses
  — "resume twice for free," the edge over deep-copying engines.

### Serialization

A continuation is a function — never serialized. The serializable replay artifact
is the **`DecisionLog`: a seed plus the ordered list of prompt responses**.
Because the engine is pure it re-emits prompts in identical order on replay, and
recorded responses are fed back. We persist *inputs*, not continuations. Response
encoding is a sum that grows with `Prompt`; its detailed shape is deferred to
implementation.

## Extension budget

The engine core needs exactly two extensions, both load-bearing:

- **`GADTs`** — `Prompt` and `Program`.
- **`RankNTypes`** — the interpreter's `forall r`.

Everything else stays Haskell 2010, per the style guide.

## Proposed module layout

**One type per module**, namespaced `Pawl.Type.<TypeName>`, each holding the type
and its instances. This keeps recompilation units small (a change to one type
doesn't rebuild its neighbours) and aids readability. Short exported names,
disambiguated by module (`Pawl.Type.Object.Object`, imported as `Object`); a
module never imports its parents, but importing a sibling `Pawl.Type.*` is fine.

Type modules (M0): `ObjectId`, `PlayerId`, `Object`, `Source`, `TapState`,
`Zone`, `Printing`, `Card`, `TypeLine`, `Supertype`, `CardType`, `Subtype`,
`Phase`, `BeginningStep`, `CombatStep`, `EndingStep`, `Player`, `Status`,
`Departure`, `Result`, `GameState`, `Prompt`, `Decider`, `Action`, `Program`
(the generic operational monad), `Game` (the `StateT` alias), `DecisionLog` —
each as `Pawl.Type.<Name>`.

Logic modules (layout flexible, not one-per-anything): turn advancement
(`next` / `firstPhase`), the `changeZone` primitive, state-based actions,
legal-action generation, the interpreter / `runGame`, and the engine loop. A
top-level `Pawl` re-exports the public API.

## Explicitly deferred past M0

`CardId`; the numeric tower — P/T isn't needed with zero creatures, and player
`life` stays `Integer` (fractional life is silver-border-only: Little Girl's
½ power → ½ damage → ½ life is the sole driver, so life joins the tower's
fractional amount type, `Rational`, then — not now); mana pool
(nothing to spend on); the seeded coin/die RNG field; `ChooseTargets` /
`OrderTriggers` / `OrderGraveyard`; damage, counters, controller, and face-state
on `Object`; the command zone; the full type taxonomy (supertype/type/subtype
beyond Basic/Land/Mountain, mana cost, colors, P/T, rules text, `faces`/layout);
and the layer/projection pass (there are no continuous effects in M0).

**Canonical id relabeling for state-equality.** Because `ObjectId`s are
monotonic, a game that cycles back to the same board has objects with higher ids
each loop, so naive structural `hash-and-compare` sees the states as distinct.
The 104.4b mandatory-loop-draw (M5) and MCTS transposition tables (M7) therefore
need a canonical relabeling — a deterministic traversal (players in turn order →
zones in fixed order → objects in order → relabel 0,1,2…, dropping the raw
counter) computed as a *view* for the comparison, never a mutation of the live id
space. This qualifies `design.md` §2.5's "hash and compare, nearly free": it's
cheap, but the relabeling is the part that isn't free. Not needed until M5/M7.

## Testing approach for M0

**Framework: the `tasty` ecosystem** — `tasty` with `tasty-hunit` (example-based
cases), `tasty-quickcheck` (properties), and `tasty-bench` for benchmarks. These
are **test/benchmark dependencies only**; the boot-libraries-first rule governs
the *library*, not its test and bench suites. The cabal file gains a `test-suite`
and a `benchmark` stanza. Benchmarks are planned from the start because the risk
register flags goldfish-loop throughput as worth measuring early, and
`tasty-bench` reuses the same test tree.

**Integration over unit.** Tests assert the observable **game state after a
sequence of actions**, treating *how* the engine got there as an implementation
detail we stay free to refactor. We avoid pinning internal function behavior.

**Property-based tests carry most of the weight**, and the suspension design
makes them nearly free: a QuickCheck-driven interpreter answers `ChooseAction` /
`Shuffle` prompts arbitrarily, generating diverse legal games (and their
`DecisionLog`s) as input. M0 properties:

- **Replay determinism** (the exit criterion) — replaying a generated game's
  `DecisionLog` reproduces an identical `GameState` sequence.
- **Card conservation** — at every step each of the 120 deck cards has exactly
  one live object in exactly one zone (M0 creates/destroys nothing).
- **Bounded hand** — no player holds more than 7 cards after a cleanup step.
- **Monotonic ids** — `nextObjectId` only increases; no id is reused.
- **Turn structure** — phases/steps advance in CR 500–514 order; priority follows
  APNAP.
- **Termination & outcome** — every game reaches a `Result` in bounded turns, with
  exactly one `Won` or `Drawn`.
- **Fixed life** — no player's life changes from its starting value in M0.

**Example-based (`tasty-hunit`) tests** cover specific closed-half scenarios,
named after rule numbers (turn order 500–514, priority 117, SBAs 704, deck-out
104), so the suite doubles as a coverage map of the rules implemented. Per the
style guide's prefer-functions-over-operators rule, assertions use
`assertEqual` / `assertBool` rather than the `@?=` / `@?` operators.
