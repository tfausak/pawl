# M4.5 P10 — Player-counter substrate, poison and energy — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a counter map to `Player`, drive it from infect (poison) and energy, add the poison-at-ten SBA and a deals-combat-damage-to-a-player trigger, landing Glistener Elf and Longtusk Cub.

**Architecture:** A new `PlayerCounterKind` type (`Energy | Poison`), disjoint from object `CounterKind`, keys a `Map PlayerCounterKind Natural` on `Player`. Infect is a `Keyword` whose deal-time bit on `DamageEvent` diverts combat damage into poison counters (on players) or −1/−1 counters (on creatures). Energy is gained by a targetless `Effect` and spent by a `CostComponent`. Loss-at-ten is a state-based action beside life ≤ 0; the energy gain rides a new triggered-ability condition.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns`; `tasty` (`tasty-hunit` + `tasty-quickcheck`); cards as JSON data under `data/cards/`.

**Spec:** `docs/superpowers/specs/2026-07-23-p10-player-counters-design.md`. Issue `tfausak/pawl#6`.

## Global Constraints

- **Build must be warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag adds `-Werror`, so any warning fails the build. Build `all`: `cabal build all --enable-tests --enable-benchmarks`. `cabal clean` first when you need a definitive warning check (incremental builds hide warnings from unchanged modules).
- **Haskell 2010, no language extensions** unless unavoidable. No `LambdaCase`, no `OverloadedStrings`. `NamedFieldPuns` permitted where it improves record-heavy clarity.
- **One type per module** under `Pawl.Type.<Name>` (type + instances only); logic elsewhere. A module never imports its parents.
- **Qualified imports aliased to the last component** (`Data.Map.Strict` → `Map`); operators unqualified. Exception: `Data.Map.Strict (Map)` is imported **unqualified** for the bare `Map` type in a field declaration (as `Object.hs` does).
- **No partial functions** written or used. No `head`/`error`/`undefined`/non-exhaustive matches. `Natural` subtraction is partial on underflow — always guard it.
- **Constructors take a `Mk` prefix**, non-punning. **Derive at least `Eq` and `Show`**; a `Map`-key type also derives `Ord`.
- **`Text` not `String`; arbitrary-precision numbers** (`Integer`, `Natural`).
- **No boolean blindness** — but note `DamageEvent.dealtByInfect :: Bool` deliberately mirrors the existing `dealtByDeathtouch :: Bool` deal-time bit; that is the established pattern, not a new violation.
- **Two invariants outrank everything:** the rules core never cases on an effect's *identity* (only classifications — a keyword or counter kind is a citation, casing on it is fine); the engine never makes a player's choice. The only new casing homes are the sanctioned interpreters: `Pawl.Resolve` (effects), `Pawl.Cost` (costs), `Pawl.Event` (triggers), `Pawl.Codec` (the JSON boundary).
- **Every CR claim re-checked against `docs/rules.txt` and cited in-code.** Card text is from Scryfall (2026-07-23), never recalled.
- **TDD, non-negotiable:** write each failing test, run it, watch it fail, then implement. One small complete commit per task on `main`. Commit message trailer per CLAUDE.md.
- **`hooky` before done:** `git add -A`; `hooky fix`; `git add -A`; `hooky run` passes. Apply HLint suggestions or justify.

### Verified codebase facts (do not re-derive)

- `Player` (`source/library/Pawl/Type/Player.hs`) is `MkPlayer { life :: Integer, status :: Status }`, deriving `(Eq, Show)` — **no `Ord`**. Two construction sites use record syntax: `Setup.hs:52` and `DamageSpec.hs:204`. Under `-Werror`, a missing field there is a build failure.
- **`Player` has NO codec.** `playerToJson`/`jsonToPlayer` do not exist; `Object` is not serialized either. `Pawl.Codec` covers the `Card` closure plus `GameEvent`/`DamageEvent`. **Therefore the spec's §2.10/§5 "round-trip the `Player.counters` field" is not implementable and is dropped** — `PlayerCounterKind` still gets a codec because the `GainPlayerCounters` *effect* embeds it, and `DamageEvent.dealtByInfect` gets one because `DamageEvent` is serialized.
- `Object.counters :: Map CounterKind Natural` (bare `Map` via `import Data.Map.Strict (Map)`); the absent-key-means-zero convention is `Map.findWithDefault 0`.
- `CounterKind` = `PlusOnePlusOne | MinusOneMinusOne`, deriving `(Eq, Ord, Show)`.
- `Keyword` constructors are ordered by CR rule number; deriving `(Eq, Ord, Show)`. `Infect` is CR 702.90, so it sits **after `Fear` (702.36), before `Devoid` (702.114)**.
- `DamageEvent` = `MkDamageEvent { source :: ObjectId, target :: Recipient, amount :: Natural, dealtByDeathtouch :: Bool, kind :: DamageKind }`, deriving `(Eq, Ord, Show)`. `Recipient` = `ToCreature ObjectId | ToPlayer PlayerId | ToObject ObjectId`. `DamageKind` = `Combat | Noncombat`.
- **Every `MkDamageEvent` is positional** — a new field breaks all of them. Library: `Damage.hs:98,104,124,139` (Combat), `Resolve.hs:388` (Noncombat), `Codec.hs:997` (record-syntax decode). Tests: `DamageSpec.hs:115,125,170,171,223,225,234`; `CodecSpec.hs:456`; `ResolveSpec.hs:581,582,805`; `ReplacementSpec.hs:241,242`.
- `Damage.applyDamage :: [DamageEvent] -> Game ()`; its `markOne` is a pure `GameState -> DamageEvent -> GameState` fold applied via `State.modify'`, branching on `DamageEvent.target`. It reads deathtouch nowhere (the SBA does, later).
- `Effect card` is parameterized. `Draw Quantity` is the model targetless effect; `PutCounters CounterKind Quantity SlotName` the model counter effect. Counts are `Quantity`, **not `Natural`**. Adding an `Effect` constructor is **compiler-forced** in `Resolve.slotsOf`, `Resolve.readsX`, `Resolve.manaProduced`, `Resolve.searchesLibrary`, `Resolve.rewriteEffect`, `Resolve.applyEffect`; it falls through wildcards (no change needed) in `Resolve.textChangeSlots`, `Resolve.armedAbilities`, `Resolve.definedSlots`, `Resolve.bindsSeveralTokens`.
- `Resolve.applyEffect :: ObjectId -> PlayerId -> Map SlotName (Subtype,Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect Card -> Game ()`. Arg 2 (`controller`) is the resolving controller — the player a targetless effect reaches. `Draw` uses it directly. `Resolve` does **not** currently import `Pawl.Type.Player`.
- `Quantity.evaluate :: GameState -> ObjectId -> Maybe PlayerId -> Quantity -> Maybe Integer`. Feed results through `fromInteger`.
- `CostComponent` = `TapThis | SacrificeThis | PayLife Natural | Sacrifice Natural Filter`, deriving `(Eq, Ord, Show)`. `Pawl.Cost` is the sole caser: `canPayComponent :: PlayerId -> ObjectId -> CostComponent -> GameState -> Bool` and `payComponent :: PlayerId -> ObjectId -> CostComponent -> Game Payment.Payment`. `PayLife n` checks `Player.life player >= toInteger n` and pays via `Player.life p - toInteger n`.
- `Sba.losesNow :: GameState -> PlayerId -> Bool` guards `Player.status player == Status.Playing` then disjuncts `Player.life player <= 0` with `Set.member pid (GameState.drewFromEmpty gs)`. `Status` = `Playing | Departed Departure`; a loss shows as `Status.Departed Departure.Lost` (and `GameState.result` set) after `checkStateBasedActions`.
- `TriggerCondition` = `SelfEnters | StepBegins Phase TurnScope | StateIs StateCondition`, deriving `(Eq, Ord, Show)`. Adding a constructor is **compiler-forced** in exactly three places: `Event.matchesTrigger` (top-level `case cond`), the `live` helper inside `Event.stateTriggers`, and the two `Codec` functions. (`Event.stateHolds`'s `case cond` is `StateCondition`, unrelated.)
- `Event.matchesTrigger :: ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool`. `GameEvent` = `Moved ZoneChange ProjectedCharacteristics | DamageDealt DamageEvent | StepBegan Phase PlayerId | SpellCast PlayerId`. Combat damage already records `DamageDealt` events, so no new recording is needed.
- `Subtype` **lacks `Phyrexian` and `Elf`** (has `Warrior`, `Cat`). Glistener Elf ("Phyrexian Elf Warrior") needs both added to the type **and** to both `Codec` tables (`subtypeToJson`, `jsonToSubtype`).
- **Codec idioms:** nullary enum → `nullary (Text.pack "Name")` / `decodeNullary (Text.pack "T") [(Text.pack "Name", Ctor), …]`. Single-`Natural` payload → `Json.tagged (Text.pack "Name") (Just (natTo n))` / `("Name", Just v) -> fmap Ctor (natFrom v)`. Multi-field → `Json.tagged … (Just (Array [f1, f2]))` / match `Just (Array [a,b])`. `natTo :: Natural -> Value`, `natFrom :: Value -> Either Text Natural`, `withValue`, `jsonToBoolDefault :: Bool -> Value -> Either Text Bool`, `Json.jBool` all exist.
- **Cards.hs test loader** (`source/test-suite/Pawl/Cards.hs`): adding a card needs four edits — a `<name>Printing :: Printing.Printing` field in `MkCards`, a `<name>Printing_ <- loadPrinting "<slug>"` line in `loadCards`, a `<name>Printing = <name>Printing_` line in the `pure MkCards {…}`, and a `<name>Printing cards` entry in `allPrintings` (which gives every card automatic codec/conservation coverage). Gate cards need not join any `redDeck`/`greenDeck` bundle.
- **Test idiom:** build a `GameState` from a `Pawl.Support` (`S`) fixture, run via an `S` runner (`runPure`/`runPureWith`/`settleSba`/`fightWith`/`runCombat`) under a prompt answerer (`S.identityAnswer`, `S.aggressiveAnswer`), then `HU.assertEqual`/`HU.assertBool`. `S.addCreature printing pid gs`, `S.combatBoardOf mine theirs`, `S.addCounter kind n oid gs`, `S.lifeOf pid gs`, `S.settleSba gs` exist. `S.addCreature`/`addToken`/etc. set `Object.counters = Map.empty` — a new `Player` field does not touch them.
- **Activation idiom** (`ActivateSpec.hs`): `Activate.activatable :: PlayerId -> ObjectId -> ActivatedAbility Card -> GameState -> Bool` (incorporates `Cost.canPay`); `Activate.activateAbility :: PlayerId -> ObjectId -> ActivatedAbility Card -> Game ()` pays the cost at activation and puts the ability on the stack; `Stack.resolveTop` resolves it. Extract the sole ability with `Card.Type.activatedAbilities (Printing.card p)` (head).

---

## File Structure

**New files**
- `source/library/Pawl/Type/PlayerCounterKind.hs` — the `Energy | Poison` classification.
- `data/cards/glistener-elf.json` — the infect/poison gate card.
- `data/cards/longtusk-cub.json` — the energy gate card.

**Modified — library**
- `Pawl/Type/Player.hs` (+`counters`), `Pawl/Setup.hs` (init empty), `Pawl/Type/Keyword.hs` (+`Infect`), `Pawl/Type/DamageEvent.hs` (+`dealtByInfect`), `Pawl/Damage.hs` (set bit ×4, divert), `Pawl/Resolve.hs` (set bit ×1, +`GainPlayerCounters`, +`Player` import), `Pawl/Type/Effect.hs` (+`GainPlayerCounters`), `Pawl/Type/CostComponent.hs` (+`PayEnergy`), `Pawl/Cost.hs` (pay/payability), `Pawl/Sba.hs` (poison SBA), `Pawl/Type/TriggerCondition.hs` (+`SelfDealsCombatDamageToPlayer`), `Pawl/Event.hs` (matcher + `live`), `Pawl/Type/Subtype.hs` (+`Phyrexian`,`Elf`), `Pawl/Codec.hs` (all of the above).

**Modified — tests**
- `Pawl/Support.hs` (`addPlayerCounter`, `playerCounterOf`), `Pawl/Cards.hs` (two cards ×4 edits), `Pawl/CodecSpec.hs`, `Pawl/DamageSpec.hs`, `Pawl/ResolveSpec.hs`, `Pawl/ReplacementSpec.hs`, `Pawl/SetupSpec.hs`, `Pawl/CardSpec.hs`, `Pawl/EventSpec.hs`, `Pawl/CostSpec.hs`.

The task order follows spec §7. Each task is one commit; the plumbing tasks (1, 2) precede the behavior tasks so the tree compiles at every commit.

---

### Task 1: `PlayerCounterKind`, the `Player.counters` field, and setup init

**Files:**
- Create: `source/library/Pawl/Type/PlayerCounterKind.hs`
- Modify: `source/library/Pawl/Type/Player.hs`
- Modify: `source/library/Pawl/Setup.hs:52` (the `newPlayer` record)
- Modify: `source/test-suite/Pawl/DamageSpec.hs:204` (the hand-built `MkPlayer`)
- Modify: `source/library/Pawl/Codec.hs` (add `playerCounterKindToJson`/`jsonToPlayerCounterKind`)
- Test: `source/test-suite/Pawl/CodecSpec.hs` (round-trip both constructors), `source/test-suite/Pawl/SetupSpec.hs` (counters start empty)

**Interfaces:**
- Produces: `Pawl.Type.PlayerCounterKind.PlayerCounterKind` = `Energy | Poison` deriving `(Eq, Ord, Show)`; `Player.counters :: Map PlayerCounterKind Natural`; `Codec.playerCounterKindToJson :: PlayerCounterKind -> Value`, `Codec.jsonToPlayerCounterKind :: Value -> Either Text PlayerCounterKind`.

- [ ] **Step 1: Write the failing codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind` (alphabetically among the `Pawl.Type.*` imports). Find the leaf-enum `testGroup` that holds the `Keyword` round-trip case (`HU.testCase "Keyword"`) and add beside it:

```haskell
          HU.testCase "PlayerCounterKind" $ do
            roundTrip "energy" Codec.playerCounterKindToJson Codec.jsonToPlayerCounterKind PlayerCounterKind.Energy
            roundTrip "poison" Codec.playerCounterKindToJson Codec.jsonToPlayerCounterKind PlayerCounterKind.Poison,
```

- [ ] **Step 2: Run it to verify it fails (does not compile — the type and codec do not exist)**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: type constructor ... PlayerCounterKind` / `Codec.playerCounterKindToJson`.

- [ ] **Step 3: Create the type**

Create `source/library/Pawl/Type/PlayerCounterKind.hs`:

```haskell
module Pawl.Type.PlayerCounterKind where

-- CR 122: a counter is a marker on an object OR a player (CR 122.1). Player
-- counters are a DISJOINT domain from object CounterKind: CR 122 gives no kind
-- that goes on both -- +1/+1, keyword, shield, stun, finality, loyalty, defense
-- and lore counters are object-only (CR 122.1a-e,g-i); poison, energy,
-- experience and rad counters are player-only (CR 122.1f,i; CR 107.14). So this
-- is its own type, not an extension of CounterKind: "a +1/+1 counter on a
-- player" and "a poison counter on a creature" stay unrepresentable.
--
-- Like CounterKind and Keyword this is a CLASSIFICATION (a citation), not an
-- effect identity: the rules core reads counts by kind (the CR 704.5c poison
-- SBA; the CR 107.14 energy payment) and never cases on a card.
--
-- Ord is load-bearing: PlayerCounterKind is a Map key on Player.counters.
-- Constructors are ordered by rule number so the type stays diffable against the
-- rules, matching CounterKind's and Keyword's posture. Experience and rad
-- counters (CR 122.1i) are one constructor each once a card wants them.
data PlayerCounterKind
  = Energy -- CR 107.14
  | Poison -- CR 122.1f
  deriving (Eq, Ord, Show)
```

Run `cabal-gild` (via `hooky fix` later) so `exposed-modules` picks it up; do not hand-edit the cabal field.

- [ ] **Step 4: Add the `counters` field to `Player`**

Rewrite `source/library/Pawl/Type/Player.hs`:

```haskell
module Pawl.Type.Player where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.PlayerCounterKind (PlayerCounterKind)
import Pawl.Type.Status (Status)

data Player = MkPlayer
  { life :: Integer,
    status :: Status,
    -- CR 122.1: player counters, counted per kind. Unlike object counters (CR
    -- 122.2, which cease to exist on a zone change), these persist for the whole
    -- game -- a player never changes zones. Absent kind means zero
    -- (Map.findWithDefault 0), the convention Object.counters uses.
    counters :: Map PlayerCounterKind Natural
  }
  deriving (Eq, Show)
```

- [ ] **Step 5: Initialize the field at both construction sites**

In `source/library/Pawl/Setup.hs`, the `newPlayer` binding (~line 52) becomes:

```haskell
      newPlayer pid =
        ( pid,
          Player.MkPlayer
            { Player.life = startingLife,
              Player.status = Status.Playing,
              Player.counters = Map.empty
            }
        )
```

In `source/test-suite/Pawl/DamageSpec.hs:204`, add the field to the inline `MkPlayer`:

```haskell
        let gs = sbaBase {GameState.players = Map.insert S.alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing, Player.counters = Map.empty}) (GameState.players sbaBase)}
```

(`DamageSpec` already imports `Data.Map.Strict as Map`.)

- [ ] **Step 6: Add the codec**

In `source/library/Pawl/Codec.hs`, add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind` (alphabetical among `Pawl.Type.*`). Beside `counterKindToJson`/`jsonToCounterKind`, add:

```haskell
playerCounterKindToJson :: PlayerCounterKind.PlayerCounterKind -> Value
playerCounterKindToJson k = nullary . Text.pack $ case k of
  PlayerCounterKind.Energy -> "Energy"
  PlayerCounterKind.Poison -> "Poison"

jsonToPlayerCounterKind :: Value -> Either Text PlayerCounterKind.PlayerCounterKind
jsonToPlayerCounterKind =
  decodeNullary
    (Text.pack "PlayerCounterKind")
    [ (Text.pack "Energy", PlayerCounterKind.Energy),
      (Text.pack "Poison", PlayerCounterKind.Poison)
    ]
```

- [ ] **Step 7: Write the failing setup test**

In `source/test-suite/Pawl/SetupSpec.hs`, add (import `Pawl.Type.Player as Player` and `Data.Map.Strict as Map` if not already present) a case asserting a freshly set-up player has an empty counter map:

```haskell
      HU.testCase "CR 122.1 a new player starts with no counters" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "empty" (Just Map.empty) (fmap Player.counters (Map.lookup S.alice (GameState.players gs))),
```

- [ ] **Step 8: Run the suite; both new tests pass**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (`PlayerCounterKind`, `a new player starts with no counters`), whole suite green.

- [ ] **Step 9: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): PlayerCounterKind and the Player.counters substrate

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 2: `Keyword.Infect` and the `DamageEvent.dealtByInfect` deal-time bit

**Files:**
- Modify: `source/library/Pawl/Type/Keyword.hs`, `source/library/Pawl/Type/DamageEvent.hs`
- Modify: `source/library/Pawl/Damage.hs` (4 sites), `source/library/Pawl/Resolve.hs:388`
- Modify: `source/library/Pawl/Codec.hs` (`keywordToJson`/`jsonToKeyword`; `damageEventToJson`/`jsonToDamageEvent`)
- Modify (positional-site fixups): `source/test-suite/Pawl/CodecSpec.hs:456`, `source/test-suite/Pawl/DamageSpec.hs:115,125,170,171,223,225,234`, `source/test-suite/Pawl/ResolveSpec.hs:581,582,805`, `source/test-suite/Pawl/ReplacementSpec.hs:241,242`
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Produces: `Keyword.Infect`; `DamageEvent.dealtByInfect :: Bool` (positioned between `dealtByDeathtouch` and `kind`). New positional shape everywhere: `MkDamageEvent source target amount dealtByDeathtouch dealtByInfect kind`.

- [ ] **Step 1: Write the failing keyword round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, beside the existing `HU.testCase "Keyword"`, add:

```haskell
          HU.testCase "Keyword.Infect" $
            roundTrip "infect" Codec.keywordToJson Codec.jsonToKeyword Keyword.Infect,
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'Keyword.Infect'`.

- [ ] **Step 3: Add the `Infect` keyword**

In `source/library/Pawl/Type/Keyword.hs`, insert `Infect` between `Fear` and `Devoid`:

```haskell
  | Fear -- 702.36
  | Infect -- 702.90
  | Devoid -- 702.114
```

- [ ] **Step 4: Add the `dealtByInfect` field**

In `source/library/Pawl/Type/DamageEvent.hs`, add the field between `dealtByDeathtouch` and `kind`:

```haskell
    dealtByDeathtouch :: Bool,
    -- CR 702.90d: whether the source had infect WHEN THIS DAMAGE WAS DEALT
    -- (last known information), captured exactly as dealtByDeathtouch is.
    dealtByInfect :: Bool,
    -- CR 510 vs CR 608: combat damage or not.
    kind :: DamageKind
```

- [ ] **Step 5: Set the bit at the four combat sites in `Damage.hs`**

At each of `Damage.hs:98,104,124,139`, insert `(Projection.hasKeyword Keyword.Infect <src> gs)` immediately before the `DamageKind.Combat` argument, where `<src>` matches the deathtouch source already read on that line — `attacker` at 98/104/124, `blocker` at 139. Example for line 98:

```haskell
              pure [DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer defender) power (Projection.hasKeyword Keyword.Deathtouch attacker gs) (Projection.hasKeyword Keyword.Infect attacker gs) DamageKind.Combat]
```

Line 139 (source is `blocker`):

```haskell
            else [DamageEvent.MkDamageEvent blocker (Recipient.ToCreature attacker) (fromInteger p) (Projection.hasKeyword Keyword.Deathtouch blocker gs) (Projection.hasKeyword Keyword.Infect blocker gs) DamageKind.Combat]
```

(`Keyword` and `Projection` are already imported in `Damage.hs`.)

- [ ] **Step 6: Set the bit at the noncombat site in `Resolve.hs:388`**

```haskell
            Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs) (Projection.hasKeyword Keyword.Infect source gs) DamageKind.Noncombat]
```

- [ ] **Step 7: Extend the `Keyword` and `DamageEvent` codecs**

In `source/library/Pawl/Codec.hs`: add an `Infect` arm to `keywordToJson` (`Keyword.Infect -> "Infect"`) and a `(Text.pack "Infect", Keyword.Infect)` pair to `jsonToKeyword`'s `decodeNullary` table. In `damageEventToJson`, add after the `dealtByDeathtouch` entry:

```haskell
      (Text.pack "dealtByInfect", Json.jBool (DamageEvent.dealtByInfect ev)),
```

In `jsonToDamageEvent`, add a binding and field (default `False`, mirroring `dealtByDeathtouch`):

```haskell
  d <- Json.field (Text.pack "dealtByDeathtouch") ps >>= jsonToBoolDefault False
  i <- Json.field (Text.pack "dealtByInfect") ps >>= jsonToBoolDefault False
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.dealtByInfect = i,
        DamageEvent.kind = k
      }
```

- [ ] **Step 8: Fix every positional `MkDamageEvent` call site (library already done in Steps 5–6; now the tests)**

Insert the new `Bool` (`False`, unless the test's intent is an infect source) immediately before the `DamageKind.*` argument at each site:
- `CodecSpec.hs:456` — `... (Recipient.ToPlayer S.bob) 2 True DamageKind.Combat)` → `... 2 True False DamageKind.Combat)`.
- `DamageSpec.hs:115,125,170,171,223,225,234` — each `... 2 False DamageKind.<K>` → `... 2 False False DamageKind.<K>`.
- `ResolveSpec.hs:581,582` — `... 2 False DamageKind.<K>` → `... 2 False False DamageKind.<K>`.
- `ResolveSpec.hs:805` — `... 1 True DamageKind.Combat` → `... 1 True False DamageKind.Combat`.
- `ReplacementSpec.hs:241,242` — `... 2 False DamageKind.Combat` → `... 2 False False DamageKind.Combat`.

- [ ] **Step 9: Run the suite; it compiles and passes**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS — `Keyword.Infect` round-trips; the existing `DamageEvent`/`GameEvent` round-trip (CodecSpec:456) still passes with the new field.

- [ ] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): Infect keyword and the DamageEvent deal-time infect bit

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 3: The infect damage diversion in `applyDamage`, and the test-support readers

**Files:**
- Modify: `source/library/Pawl/Damage.hs` (`markOne`; add `CounterKind`/`PlayerCounterKind` imports)
- Modify: `source/test-suite/Pawl/Support.hs` (add `addPlayerCounter`, `playerCounterOf`)
- Test: `source/test-suite/Pawl/DamageSpec.hs`

**Interfaces:**
- Consumes: `DamageEvent.dealtByInfect` (Task 2); `Player.counters` (Task 1).
- Produces: `S.addPlayerCounter :: PlayerCounterKind.PlayerCounterKind -> Natural.Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState`; `S.playerCounterOf :: PlayerCounterKind.PlayerCounterKind -> PlayerId.PlayerId -> GameState.GameState -> Natural.Natural`.

- [ ] **Step 1: Add the two `Support` helpers (needed by the tests below and by Tasks 4/6)**

In `source/test-suite/Pawl/Support.hs`, add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind` (alphabetical among `Pawl.Type.*`). Beside `addCounter`, add:

```haskell
-- Put `n` counters of a player-counter kind directly onto a player, bypassing
-- the diversion/effect that would add them -- so an SBA or cost test can set up
-- poison or energy without resolving anything.
addPlayerCounter :: PlayerCounterKind.PlayerCounterKind -> Natural.Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addPlayerCounter kind n pid gs =
  let bump player = player {Player.counters = Map.insertWith (+) kind n (Player.counters player)}
   in gs {GameState.players = Map.adjust bump pid (GameState.players gs)}

-- How many counters of a kind a player has (absent kind = zero).
playerCounterOf :: PlayerCounterKind.PlayerCounterKind -> PlayerId.PlayerId -> GameState.GameState -> Natural.Natural
playerCounterOf kind pid gs =
  maybe 0 (Map.findWithDefault 0 kind . Player.counters) (Map.lookup pid (GameState.players gs))
```

(`Support` already imports `Player`, `Map`, `Natural`, `PlayerId`, `GameState`.)

- [ ] **Step 2: Write the failing diversion tests**

In `source/test-suite/Pawl/DamageSpec.hs`, add a `testGroup` (or extend the damage group). Import `Pawl.Type.PlayerCounterKind as PlayerCounterKind` and `Pawl.Type.CounterKind as CounterKind` if not present. These build infect events by hand and push them through the funnel:

```haskell
      HU.testCase "CR 120.3b infect damage to a player becomes poison, not life loss" $
        let (oid, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
         in do
              HU.assertEqual "bob has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
              HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "the source's controller gains no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 120.3d infect damage to a creature becomes -1/-1 counters, not marked damage" $
        let (src, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            (victim, gs1) = S.addPiker cards S.bob gs0
            ev = DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs1 (Damage.applyDamage [ev])
         in do
              HU.assertEqual "two -1/-1 counters" (Just 2) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject victim after))
              HU.assertEqual "no marked damage" (Just 0) (S.damageOf victim after),
```

- [ ] **Step 3: Run to verify they fail**

Run: `cabal test --test-options='-p "infect"' 2>&1 | tail -25`
Expected: FAIL — poison count is 0 and life is 17 (damage still drained); creature has marked damage 2 and no counters.

- [ ] **Step 4: Implement the diversion in `markOne`**

In `source/library/Pawl/Damage.hs`, add imports `import qualified Pawl.Type.CounterKind as CounterKind` and `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind`. Replace the `markOne` fold's two recipient arms so each checks `DamageEvent.dealtByInfect ev`:

```haskell
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          if DamageEvent.dealtByInfect ev
            then -- CR 120.3d / 702.90c: -1/-1 counters, no marked damage. Added
                 -- directly (not via Event.putCounters): this is a consequence of
                 -- a damage event that already ran the CR 616 replacement loop, so
                 -- a "would put -1/-1 from infect" CR 614 sub-replacement is out of
                 -- scope (#TBD-614-funnel).
              let addMinus obj = obj {Object.counters = Map.insertWith (+) CounterKind.MinusOneMinusOne (DamageEvent.amount ev) (Object.counters obj)}
               in g {GameState.objects = Map.adjust addMinus oid (GameState.objects g)}
            else
              let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
               in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          if DamageEvent.dealtByInfect ev
            then -- CR 120.3b / 702.90b: poison counters, no life loss.
              let poison player = player {Player.counters = Map.insertWith (+) PlayerCounterKind.Poison (DamageEvent.amount ev) (Player.counters player)}
               in g {GameState.players = Map.adjust poison pid (GameState.players g)}
            else
              let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
               in g {GameState.players = Map.adjust drain pid (GameState.players g)}
        Recipient.ToObject _ -> g
```

Note: the `#TBD-614-funnel` marker is a placeholder — Task 8 files the deferral issues and you replace it with the real `(#N)` before this task's commit (or in Task 8, whichever lands the citation). If Task 8 has already run, use the real number now.

- [ ] **Step 5: Run to verify they pass, and the whole suite is green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS. Existing non-infect damage tests (life drain, marked damage) unchanged.

- [ ] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): infect diverts damage to poison and -1/-1 counters

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 4: The poison-at-ten SBA and Glistener Elf

**Files:**
- Modify: `source/library/Pawl/Sba.hs` (`losesNow`)
- Modify: `source/library/Pawl/Type/Subtype.hs` (+`Phyrexian`, `Elf`), `source/library/Pawl/Codec.hs` (both Subtype tables)
- Create: `data/cards/glistener-elf.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (four edits for `glistenerElfPrinting`)
- Test: `source/test-suite/Pawl/DamageSpec.hs` (SBA + combat integration), `source/test-suite/Pawl/CardSpec.hs` (characteristics)

**Interfaces:**
- Consumes: `Player.counters`, `S.addPlayerCounter`, `S.playerCounterOf`, the infect diversion.
- Produces: `Subtype.Phyrexian`, `Subtype.Elf`; `Cards.glistenerElfPrinting :: Cards.Cards -> Printing.Printing`.

- [ ] **Step 1: Write the failing poison-SBA tests**

In `source/test-suite/Pawl/DamageSpec.hs` (its header already declares it covers `Pawl.Sba`), import `Pawl.Type.Status as Status`, `Pawl.Type.Departure as Departure`, `Pawl.Type.PlayerCounterKind as PlayerCounterKind` if not present, and add:

```haskell
      HU.testCase "CR 704.5c ten poison counters lose the game" $
        let gs = S.addPlayerCounter PlayerCounterKind.Poison 10 S.bob (Setup.emptyGame S.bothPlayers)
            after = S.settleSba gs
         in HU.assertEqual "bob lost" (Just (Status.Departed Departure.Lost)) (fmap Player.status (Map.lookup S.bob (GameState.players after))),
      HU.testCase "CR 704.5c nine poison counters do not" $
        let gs = S.addPlayerCounter PlayerCounterKind.Poison 9 S.bob (Setup.emptyGame S.bothPlayers)
            after = S.settleSba gs
         in HU.assertEqual "bob still playing" (Just Status.Playing) (fmap Player.status (Map.lookup S.bob (GameState.players after))),
```

- [ ] **Step 2: Run to verify they fail**

Run: `cabal test --test-options='-p "poison counters"' 2>&1 | tail -20`
Expected: FAIL — bob is still `Playing` at ten (no poison clause in `losesNow`).

- [ ] **Step 3: Add the poison clause to `losesNow`**

In `source/library/Pawl/Sba.hs`, add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind`. Extend the disjunction (`Map` is already imported):

```haskell
-- CR 704.5a (life <= 0), CR 704.5b (drawing from an empty library), and CR
-- 704.5c (ten or more poison counters). Two-Headed Giant's shared-poison variant
-- (CR 704.6b / 810) is out of scope (design.md §6).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && ( Player.life player <= 0
             || Set.member pid (GameState.drewFromEmpty gs)
             || Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters player) >= 10
         )
```

- [ ] **Step 4: Run to verify the SBA tests pass**

Run: `cabal build all --enable-tests && cabal test --test-options='-p "poison counters"' 2>&1 | tail -20`
Expected: PASS (both).

- [ ] **Step 5: Add the `Phyrexian` and `Elf` subtypes (Glistener Elf needs them)**

In `source/library/Pawl/Type/Subtype.hs`, add two constructors (place near the other creature types, e.g. after `Elephant`):

```haskell
  | Phyrexian -- CR 205.3m (a creature type; Glistener Elf's)
  | Elf -- CR 205.3m (a creature type; Glistener Elf's)
```

In `source/library/Pawl/Codec.hs`, add matching arms to `subtypeToJson` (`Subtype.Phyrexian -> "Phyrexian"`, `Subtype.Elf -> "Elf"`) and pairs to `jsonToSubtype`'s table (`(Text.pack "Phyrexian", Subtype.Phyrexian)`, `(Text.pack "Elf", Subtype.Elf)`).

- [ ] **Step 6: Create the Glistener Elf card JSON**

Create `data/cards/glistener-elf.json` (keys alphabetical, matching the repo's emitted order):

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "keywords": [{ "type": "Infect" }],
  "manaCost": [
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Green" } } }
  ],
  "name": "Glistener Elf",
  "power": { "type": "Literal", "value": 1 },
  "replacementEffects": [],
  "spell": {
    "modes": [{ "effects": [], "targetSpecs": [] }],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "toughness": { "type": "Literal", "value": 1 },
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [{ "type": "Phyrexian" }, { "type": "Elf" }, { "type": "Warrior" }],
    "supertypes": [],
    "types": [{ "type": "Creature" }]
  }
}
```

- [ ] **Step 7: Register the card in `Cards.hs` (four edits)**

In `source/test-suite/Pawl/Cards.hs`: add field `glistenerElfPrinting :: Printing.Printing,` to `MkCards`; add `glistenerElfPrinting_ <- loadPrinting "glistener-elf"` in `loadCards`; add `glistenerElfPrinting = glistenerElfPrinting_,` to the `pure MkCards {…}`; add `glistenerElfPrinting cards,` to `allPrintings`.

- [ ] **Step 8: Write the failing Glistener Elf tests (characteristics + combat integration)**

In `source/test-suite/Pawl/CardSpec.hs`, mirroring the Bird Maiden case:

```haskell
        , HU.testCase "Glistener Elf is a {G} 1/1 Phyrexian Elf Warrior with infect" $ do
            HU.assertEqual "name" (Text.pack "Glistener Elf") (Card.Type.name (card (Cards.glistenerElfPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power (card (Cards.glistenerElfPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card (Cards.glistenerElfPrinting cards)))
            HU.assertBool "has infect" (elem Keyword.Infect (Card.Type.keywords (card (Cards.glistenerElfPrinting cards))))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Warrior]) (TypeLine.subtypes (Card.Type.typeLine (card (Cards.glistenerElfPrinting cards))))
```

(Confirm the exact accessor for a card's keyword list in `CardSpec`'s existing imports — the m2a keyword tests already read it; reuse that accessor rather than guessing `Card.Type.keywords` if the module names it differently.)

In `source/test-suite/Pawl/DamageSpec.hs`, the combat integration:

```haskell
      HU.testCase "CR 702.90 Glistener Elf poisons an unblocked player, drains no life" $
        let (gs, _, _) = S.combatBoardOf [Cards.glistenerElfPrinting cards] []
            after = S.fightWith S.aggressiveAnswer gs
         in do
              HU.assertEqual "bob has one poison" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
              HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "alice (controller) has no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 702.90c Glistener Elf shrinks and kills a blocker with -1/-1 counters" $
        let (gs, _, blockers) = S.combatBoardOf [Cards.glistenerElfPrinting cards] [Cards.pikerPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            settled = S.settleSba fought
            blocker = case blockers of { b : _ -> b ; [] -> ObjectId.MkObjectId 999 }
         in do
              HU.assertEqual "one -1/-1 counter before SBA" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought))
              HU.assertEqual "no marked damage on the blocker" (Just 0) (S.damageOf blocker fought)
              HU.assertEqual "blocker buried by 704.5f" 1 (length (Game.zoneMembers Zone.Graveyard S.bob settled)),
```

- [ ] **Step 9: Run to verify they fail, then (with the card in place) pass**

Run: `cabal build all --enable-tests && cabal test --test-options='-p "Glistener"' 2>&1 | tail -25`
Expected: PASS once the JSON and `Cards.hs` wiring exist (the characteristics test and both combat tests).

- [ ] **Step 10: Run the whole suite**

Run: `cabal test 2>&1 | tail -15`
Expected: whole suite green (the new card also gets automatic codec coverage via `allPrintings`).

- [ ] **Step 11: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): poison-at-ten SBA and Glistener Elf

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 5: The `GainPlayerCounters` effect

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs` (six compiler-forced arms + the executor; add `Player` import)
- Modify: `source/library/Pawl/Codec.hs` (`effectToJson`/`jsonToEffect`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Player.counters`, `PlayerCounterKind`, `Quantity.evaluate`.
- Produces: `Effect.GainPlayerCounters :: PlayerCounterKind -> Quantity -> Effect card`. Encodes as `Json.tagged "GainPlayerCounters" (Just (Array [playerCounterKindToJson k, quantityToJson q]))`.

- [ ] **Step 1: Write the failing codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, in the effect round-trip group, add:

```haskell
          HU.testCase "GainPlayerCounters" $
            roundTrip "gpc" Codec.effectToJson Codec.jsonToEffect (Effect.GainPlayerCounters PlayerCounterKind.Energy (Quantity.Literal 2)),
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'Effect.GainPlayerCounters'`.

- [ ] **Step 3: Add the constructor**

In `source/library/Pawl/Type/Effect.hs`, add `import Pawl.Type.PlayerCounterKind (PlayerCounterKind)` and a constructor (beside the other counter/player effects):

```haskell
  | -- CR 122 / 107.14: the resolving controller ("you") gains N counters of a
    -- player-counter kind. Targetless, like Draw. Subsumes any self-scoped
    -- player counter (energy, experience, rad) without a new opcode. A TARGETED
    -- player-counter effect needs a recipient and is deferred (#TBD-targeted).
    GainPlayerCounters PlayerCounterKind Quantity
```

- [ ] **Step 4: Add the six compiler-forced arms and the executor in `Resolve.hs`**

Add `import qualified Pawl.Type.Player as Player`. Then:
- `slotsOf`: `Effect.GainPlayerCounters {} -> Set.empty`
- `readsX` (the `effectReadsX` case): `Effect.GainPlayerCounters _ quantity -> quantity == Quantity.Type.X`
- `manaProduced`: `Effect.GainPlayerCounters {} -> Nothing`
- `searchesLibrary`: `Effect.GainPlayerCounters {} -> False`
- `rewriteEffect`: `Effect.GainPlayerCounters {} -> effect`
- `applyEffect`:

```haskell
  Effect.GainPlayerCounters kind quantity -> do
    gs <- State.get
    case Quantity.evaluate gs source (Just controller) quantity of
      Just n
        | n > 0 ->
            State.modify'
              ( \g ->
                  g
                    { GameState.players =
                        Map.adjust
                          (\p -> p {Player.counters = Map.insertWith (+) kind (fromInteger n) (Player.counters p)})
                          controller
                          (GameState.players g)
                    }
              )
      _ -> pure ()
```

(Leave `textChangeSlots`, `armedAbilities`, `definedSlots`, `bindsSeveralTokens` alone — their wildcards absorb the new constructor correctly.)

- [ ] **Step 5: Add the effect codec**

In `source/library/Pawl/Codec.hs`, `effectToJson`:

```haskell
  Effect.GainPlayerCounters k q -> Json.tagged (Text.pack "GainPlayerCounters") (Just (Array [playerCounterKindToJson k, quantityToJson q]))
```

`jsonToEffect`:

```haskell
    "GainPlayerCounters" -> case mv of
      Just (Array [k, q]) -> Effect.GainPlayerCounters <$> jsonToPlayerCounterKind k <*> jsonToQuantity q
      _ -> Left (Text.pack "GainPlayerCounters expects [playerCounterKind, quantity]")
```

- [ ] **Step 6: Write the failing behavior test**

In `source/test-suite/Pawl/ResolveSpec.hs`, add a case driving `applyEffect` directly (mirror how the file already calls `applyEffect`/`Resolve` for `Draw`; supply empty `bound`/`legality`/`chosen` maps, source = the ability source, controller = alice):

```haskell
      HU.testCase "CR 107.14 GainPlayerCounters gives the resolving controller energy" $
        let (src, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            act = Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.GainPlayerCounters PlayerCounterKind.Energy (Quantity.Type.Literal 2))
            after = S.runPure S.identityAnswer gs0 act
         in HU.assertEqual "alice has two energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after),
```

(Check `Resolve.applyEffect`'s exact exported name/arity against the file; if `ResolveSpec` already has a thin wrapper it uses for effect tests, use that instead.)

- [ ] **Step 7: Run to verify pass, whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -20`
Expected: PASS (`GainPlayerCounters` round-trip and the energy-gain behavior).

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): GainPlayerCounters, a targetless player-counter effect

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 6: The `PayEnergy` cost component

**Files:**
- Modify: `source/library/Pawl/Type/CostComponent.hs`
- Modify: `source/library/Pawl/Cost.hs` (`canPayComponent`, `payComponent`)
- Modify: `source/library/Pawl/Codec.hs` (`costComponentToJson`/`jsonToCostComponent`)
- Test: `source/test-suite/Pawl/CostSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Player.counters`, `PlayerCounterKind.Energy`, `S.addPlayerCounter`, `S.playerCounterOf`.
- Produces: `CostComponent.PayEnergy :: Natural -> CostComponent`. Encodes as `Json.tagged "PayEnergy" (Just (natTo n))`.

- [ ] **Step 1: Write the failing codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, in the cost-component group:

```haskell
          HU.testCase "PayEnergy" $
            roundTrip "pe" Codec.costComponentToJson Codec.jsonToCostComponent (CostComponent.PayEnergy 2),
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'CostComponent.PayEnergy'`.

- [ ] **Step 3: Add the constructor**

In `source/library/Pawl/Type/CostComponent.hs`, add (a `Natural`, matching `PayLife`'s specificity; `Numeric.Natural (Natural)` is already imported):

```haskell
  | -- CR 107.14 / 118: pay N energy counters. Energy-specific, not a general
    -- PayPlayerCounters -- energy is the only player counter ever spent as a
    -- cost. A Natural, not a Quantity: a cost has no binding environment at CR
    -- 601.2f time, and no card pays a variable amount of energy (#TBD-variable).
    PayEnergy Natural
```

- [ ] **Step 4: Add the cost interpreter arms**

In `source/library/Pawl/Cost.hs`, add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind`. In `canPayComponent`:

```haskell
  CostComponent.PayEnergy n -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player) >= n
```

In `payComponent` (guard the `Natural` subtraction so it is never forced on underflow — payability guarantees `have >= n`, but keep it total):

```haskell
  CostComponent.PayEnergy n -> do
    let spend player =
          let have = Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player)
              left = if have >= n then have - n else 0
           in player {Player.counters = Map.insert PlayerCounterKind.Energy left (Player.counters player)}
    State.modify' (\gs -> gs {GameState.players = Map.adjust spend pid (GameState.players gs)})
    pure Payment.Paid
```

- [ ] **Step 5: Add the cost-component codec**

In `source/library/Pawl/Codec.hs`, `costComponentToJson`: `CostComponent.PayEnergy n -> Json.tagged (Text.pack "PayEnergy") (Just (natTo n))`. `jsonToCostComponent`: `("PayEnergy", Just v) -> fmap CostComponent.PayEnergy (natFrom v)`.

- [ ] **Step 6: Write the failing payability/payment tests**

In `source/test-suite/Pawl/CostSpec.hs` (import `PlayerCounterKind`, `CostComponent`, `Cost` as needed):

```haskell
      HU.testCase "CR 118.6 PayEnergy is unpayable below the count and payable at or above" $
        let (oid, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            two = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
            one = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice gs0
         in do
              HU.assertBool "two energy pays PayEnergy 2" (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) two)
              HU.assertBool "one energy cannot" (not (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) one)),
      HU.testCase "CR 107.14 paying PayEnergy removes that many energy counters" $
        let (oid, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            three = S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice gs0
            after = S.runPure S.identityAnswer three (Monad.void (Cost.payComponent S.alice oid (CostComponent.PayEnergy 2)))
         in HU.assertEqual "one energy left" 1 (S.playerCounterOf PlayerCounterKind.Energy S.alice after),
```

(Confirm `Cost.canPayComponent` / `Cost.payComponent` are exported — `Pawl.Cost` has no export list, so they are. Import `Control.Monad as Monad` for `void`, or use `runPureWith` and discard the `Payment`.)

- [ ] **Step 7: Run to verify pass, whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): PayEnergy, a spent-player-counter cost component

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 7: The combat-damage trigger and Longtusk Cub

**Files:**
- Modify: `source/library/Pawl/Type/TriggerCondition.hs`
- Modify: `source/library/Pawl/Event.hs` (`matchesTrigger` + the `live` helper in `stateTriggers`)
- Modify: `source/library/Pawl/Codec.hs` (`triggerConditionToJson`/`jsonToTriggerCondition`)
- Create: `data/cards/longtusk-cub.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (four edits for `longtuskCubPrinting`)
- Test: `source/test-suite/Pawl/EventSpec.hs` (matcher), `source/test-suite/Pawl/CostSpec.hs` (card + pay ability), plus an energy-gain integration case

**Interfaces:**
- Consumes: the whole substrate; `GainPlayerCounters` (Task 5), `PayEnergy` (Task 6), `Activate.activatable`/`activateAbility`, `Stack.resolveTop`.
- Produces: `TriggerCondition.SelfDealsCombatDamageToPlayer`; `Cards.longtuskCubPrinting`.

- [ ] **Step 1: Write the failing matcher tests**

In `source/test-suite/Pawl/EventSpec.hs`, add (imports `TriggerCondition`, `DamageEvent`, `GameEvent`, `Recipient`, `DamageKind`, `ObjectId` as needed):

```haskell
      HU.testCase "CR 603.2 SelfDealsCombatDamageToPlayer matches the bearer's combat damage to a player" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False DamageKind.Combat)
         in HU.assertBool "matches" (Event.matchesTrigger bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev),
      HU.testCase "it does not match combat damage to a creature" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToCreature (ObjectId.MkObjectId 2)) 2 False False DamageKind.Combat)
         in HU.assertBool "no match" (not (Event.matchesTrigger bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)),
      HU.testCase "it does not match noncombat damage to a player" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False DamageKind.Noncombat)
         in HU.assertBool "no match" (not (Event.matchesTrigger bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)),
```

- [ ] **Step 2: Run to verify they fail**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'TriggerCondition.SelfDealsCombatDamageToPlayer'`.

- [ ] **Step 3: Add the trigger condition**

In `source/library/Pawl/Type/TriggerCondition.hs`, add (nullary, so no new imports):

```haskell
  | -- CR 603.2 / 509-510: the bearer dealt combat damage to a player. Rides P4's
    -- event history -- combat damage already records a DamageDealt event.
    SelfDealsCombatDamageToPlayer
```

- [ ] **Step 4: Add the matcher arm and the `live` arm in `Event.hs`**

In `matchesTrigger`, add a top-level arm to `case cond of`:

```haskell
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
```

Add a small non-partial helper near `matchesTrigger` (or reuse one if `Event` already exposes a recipient discriminator — check first):

```haskell
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToObject _ -> False
```

In the `live` helper inside `stateTriggers`, add the compiler-forced arm (it is an event trigger, never a state trigger):

```haskell
                TriggerCondition.SelfDealsCombatDamageToPlayer -> False
```

Ensure `DamageKind` and `Recipient` are imported in `Event.hs` (both are referenced via `GameEvent`/`DamageEvent` already; add `import qualified Pawl.Type.DamageKind as DamageKind` and `import qualified Pawl.Type.Recipient as Recipient` if not present).

- [ ] **Step 5: Add the trigger-condition codec**

`triggerConditionToJson`: `TriggerCondition.SelfDealsCombatDamageToPlayer -> nullary (Text.pack "SelfDealsCombatDamageToPlayer")`. `jsonToTriggerCondition`: `("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer`.

- [ ] **Step 6: Run to verify the matcher tests pass**

Run: `cabal build all --enable-tests && cabal test --test-options='-p "SelfDealsCombatDamageToPlayer"' 2>&1 | tail -20`
Expected: PASS (all three matcher cases).

- [ ] **Step 7: Create the Longtusk Cub card JSON**

Create `data/cards/longtusk-cub.json`:

```json
{
  "activatedAbilities": [
    {
      "cost": { "components": [{ "type": "PayEnergy", "value": 2 }], "mana": [] },
      "modal": {
        "modes": [
          {
            "effects": [
              { "type": "PutCounters", "value": [{ "type": "PlusOnePlusOne" }, { "type": "Literal", "value": 1 }, "self"] }
            ],
            "targetSpecs": []
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    { "type": "Generic", "value": 1 },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Green" } } }
  ],
  "name": "Longtusk Cub",
  "power": { "type": "Literal", "value": 2 },
  "replacementEffects": [],
  "spell": {
    "modes": [{ "effects": [], "targetSpecs": [] }],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "toughness": { "type": "Literal", "value": 2 },
  "triggeredAbilities": [
    {
      "condition": { "type": "SelfDealsCombatDamageToPlayer" },
      "modal": {
        "modes": [
          {
            "effects": [
              { "type": "GainPlayerCounters", "value": [{ "type": "Energy" }, { "type": "Literal", "value": 2 }] }
            ],
            "targetSpecs": []
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "typeLine": {
    "subtypes": [{ "type": "Cat" }],
    "supertypes": [],
    "types": [{ "type": "Creature" }]
  }
}
```

- [ ] **Step 8: Register the card in `Cards.hs` (four edits)**

Add `longtuskCubPrinting :: Printing.Printing,` to `MkCards`; `longtuskCubPrinting_ <- loadPrinting "longtusk-cub"` in `loadCards`; `longtuskCubPrinting = longtuskCubPrinting_,` in `pure MkCards {…}`; `longtuskCubPrinting cards,` in `allPrintings`.

- [ ] **Step 9: Write the failing card + pay-ability + energy-gain tests**

In `source/test-suite/Pawl/CostSpec.hs`, add a local ability extractor (mirroring `ActivateSpec.theAbility`) and the cases. Import `Activate`, `Stack`, `CounterKind`, `PlayerCounterKind` as needed.

```haskell
      HU.testCase "Longtusk Cub is a {1}{G} 2/2 Cat with a pay-energy ability" $ do
        HU.assertEqual "name" (Text.pack "Longtusk Cub") (Card.Type.name (Printing.card (Cards.longtuskCubPrinting cards)))
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (Printing.card (Cards.longtuskCubPrinting cards)))
        HU.assertEqual "one activated ability" 1 (length (Card.Type.activatedAbilities (Printing.card (Cards.longtuskCubPrinting cards)))),
      HU.testCase "CR 118.6 the pay-energy ability is payable at two energy, not at one, and grows the Cub" $
        let (cubId, base) = S.addCreature (Cards.longtuskCubPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            ability = case Card.Type.activatedAbilities (Printing.card (Cards.longtuskCubPrinting cards)) of { ab : _ -> ab ; [] -> error "no ability" }
            withTwo = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice base
            withOne = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice base
            activated = S.runPure S.identityAnswer withTwo (Activate.activateAbility S.alice cubId ability)
            resolved = S.runPure S.identityAnswer activated Stack.resolveTop
         in do
              HU.assertBool "payable at two" (Activate.activatable S.alice cubId ability withTwo)
              HU.assertBool "unpayable at one" (not (Activate.activatable S.alice cubId ability withOne))
              HU.assertEqual "energy spent" 0 (S.playerCounterOf PlayerCounterKind.Energy S.alice activated)
              HU.assertEqual "Cub grew a +1/+1 counter" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject cubId resolved)),
```

Replace the `error "no ability"` fallback with a total form: bind the ability via a `case … of { ab : _ -> …; [] -> HU.assertFailure … }` wrapper, or lift the extraction into the `do` block so an empty list fails the assertion rather than calling `error` (no partial functions in committed tests).

For the energy-gain integration, add (drives a real attack through the engine so the trigger fires and resolves):

```haskell
      HU.testCase "CR 603.2 Longtusk Cub gains two energy when it connects" $
        let (gs, _, _) = S.combatBoardOf [Cards.longtuskCubPrinting cards] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "alice gained two energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after),
```

If `S.runCombat` stops before the placed trigger resolves (it halts once combat is left), fall back to driving more steps: replace it with a bounded `Engine.runStep` loop that continues into the postcombat main phase, or assert after `runCombat` then one extra `Engine.runStep`. Verify empirically in Step 10 and adjust the runner, **not** the assertion.

- [ ] **Step 10: Run to verify pass, whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (card characteristics, pay ability payable/unpayable + growth, energy-gain-on-connect). If the energy-gain case fails because the trigger has not resolved, adjust the runner per Step 9's note and re-run; do not weaken the `== 2` assertion.

- [ ] **Step 11: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p10): combat-damage trigger and Longtusk Cub

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 8: Deferral issues, in-code citations, and milestone bookkeeping

**Files:**
- Modify: `source/library/Pawl/Damage.hs` (replace the `#TBD-614-funnel` marker), `source/library/Pawl/Type/Effect.hs` (`#TBD-targeted`), `source/library/Pawl/Type/CostComponent.hs` (`#TBD-variable`)
- Modify: `docs/progress.md`, `CLAUDE.md`

**Interfaces:** none (bookkeeping).

- [ ] **Step 1: File the deferral issues on `tfausak/pawl`**

For each spec §8 deferral, `gh issue create` carrying status, rationale, and expiry trigger label (mostly `expires:card-driven`, plus `gap`/`rules-correctness` as fitting). File: counter→layer-6 ability-granting path; toxic (CR 702.164); poisonous (CR 702.70); proliferate (CR 701.27); targeted/variable player counters; variable energy cost; CR 614 player-counter replacement funnel; experience/rad counters; mana of any color. (Two-Headed Giant poison sharing is out of scope, not an issue.) Example:

```bash
gh issue create --repo tfausak/pawl \
  --title "CR 614 player-counter replacement funnel (energy/poison doubling)" \
  --label gap --label expires:card-driven \
  --body "Deferred by M4.5 P10. Energy gain and infect poison are added directly today (no CR 614 opportunity for a doubler). Fires when a card in the pool doubles or replaces player-counter gains. See docs/superpowers/specs/2026-07-23-p10-player-counters-design.md §8."
```

- [ ] **Step 2: Replace the in-code markers with the real issue numbers**

In `Pawl/Damage.hs` (infect −1/−1 comment), `Pawl/Type/Effect.hs` (`GainPlayerCounters` comment), `Pawl/Type/CostComponent.hs` (`PayEnergy` comment), replace `#TBD-…` with the actual `(#N)`. State only what is *not* implemented, plus `(#N)`; never write the expiry into the comment (CLAUDE.md).

- [ ] **Step 3: Verify no stray `#TBD` markers remain**

Run: `grep -rn "#TBD" source/ ; echo "exit: $?"`
Expected: no matches (grep exit 1).

- [ ] **Step 4: Add the P10 completion entry to `docs/progress.md`**

One distilled entry: gate cards (Glistener Elf, Longtusk Cub), the decision proved (a player-counter substrate disjoint from object counters; infect as a deal-time classification bit; energy as a bidirectional player counter), and the types/opcodes added (`PlayerCounterKind`; `Player.counters`; `Keyword.Infect`; `DamageEvent.dealtByInfect`; `Effect.GainPlayerCounters`; `CostComponent.PayEnergy`; `TriggerCondition.SelfDealsCombatDamageToPlayer`; the poison-at-ten SBA). Match the format of the existing entries.

- [ ] **Step 5: Replace (do not append) the status bullet in `CLAUDE.md`**

Update the "Current work and tracking" status bullet so it records P10 as closed (GAP-C player-counter substrate + poison/energy customers of GAP-S), and note P11 (Command zone) as the remaining M4.5 phase. Close issue #6: `gh issue close 6 --repo tfausak/pawl --comment "Landed by M4.5 P10."`.

- [ ] **Step 6: Final full verification**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -15`
Expected: warning-clean build, whole suite green.

- [ ] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
docs(m4.5-p10): completion note, deferral issues, CLAUDE.md status

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

## Exit criterion (spec §10)

Glistener Elf can be cast, attack, and (a) poison a player to a loss at ten and (b) shrink and kill a blocker with −1/−1 counters — all with no life loss and no marked damage. Longtusk Cub gains energy on connecting and spends it to grow itself, unpayable below cost. `Player` carries a counter map; `PlayerCounterKind` is distinct from `CounterKind`; the poison-at-ten SBA lives beside life ≤ 0. Build warning-clean, `hooky run` passes, every rules claim cited against `docs/rules.txt`.

## Progress check

`grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-p10-player-counters.md` must reach `0`.

## Deviations from the spec (verified against the codebase, applied in this plan)

1. **No `Player.counters` codec (spec §2.10/§5).** `Player` is not serialized (`playerToJson`/`jsonToPlayer` do not exist; `Object` is not serialized either — `Pawl.Codec` covers the `Card` closure plus `GameEvent`/`DamageEvent`). `PlayerCounterKind` still gets a codec because `GainPlayerCounters` embeds it; `DamageEvent.dealtByInfect` gets one because `DamageEvent` is serialized. There is no "round-trip the `Player.counters` field" task.
2. **`Subtype` needs `Phyrexian` and `Elf` (not mentioned in the spec).** Glistener Elf's "Phyrexian Elf Warrior" type line requires both, added to the type and both `Codec` tables in Task 4.
