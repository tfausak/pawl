# M4.5 P11 — Command zone, emblems, and the monarch — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the seventh game zone (Command) with the emblem as its first resident whose static ability functions from outside the battlefield, and the monarch as a `GameState` designation with two sourceless inherent triggered abilities — landing Palace Jailer (monarch) and a labeled synthetic emblem source.

**Architecture:** `Zone.Command` is a shared `Set ObjectId` on `GameState`, mirroring `exile`. An emblem is an `Object` distinguished by `Source.OfEmblem Card` (like a token's `OfToken Card`), minted into the command zone by `Effect.CreateEmblem`; the projection gains a symmetric gather pass over the command zone so an emblem's static ability radiates from there. The monarch is `monarch :: Maybe PlayerId`; its two inherent abilities (CR 725.2) have no bearer, so they live in a new `Pawl.Monarch` module, are gathered from the `monarch` field (not from any card), and are placed on the stack under a new `Source.OfInherentTrigger PlayerId (TriggeredAbility Card)`. Palace Jailer's "exile until an opponent becomes the monarch" is a new opcode `ExileUntilMonarch` that registers the exiled incarnation in a `GameState.exiledUntilMonarch` map, swept back to the battlefield by a new settle-loop pass when an opponent takes the crown.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns`; `tasty` (`tasty-hunit` + `tasty-quickcheck`); cards as JSON data under `data/cards/`.

**Spec:** `docs/superpowers/specs/2026-07-23-p11-command-zone-emblems-monarch-design.md`. Umbrella issue `tfausak/pawl#7`.

## Global Constraints

- **Build must be warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag adds `-Werror`, so any warning fails the build. Build `all`: `cabal build all --enable-tests --enable-benchmarks`. `cabal clean` first when you need a definitive warning check (incremental builds hide warnings from unchanged modules).
- **Haskell 2010, no language extensions** unless unavoidable. No `LambdaCase`, no `OverloadedStrings`. `NamedFieldPuns` permitted where it improves record-heavy clarity.
- **One type per module** under `Pawl.Type.<Name>` (type + instances only); logic elsewhere (`Pawl.Monarch` is a logic module, like `Pawl.Projection`/`Pawl.Event`). A module never imports its parents.
- **Qualified imports aliased to the last component** (`Data.Map.Strict` → `Map`); operators unqualified. `Data.Map.Strict (Map)` / `Data.Set (Set)` imported **unqualified** for a bare type in a field declaration (as `GameState.hs` does).
- **No partial functions** written or used. No `head`/`error`/`undefined`/non-exhaustive matches, in library **or committed tests**. Bind list heads with `case xs of { x : _ -> …; [] -> HU.assertFailure … }`, never `error`.
- **Constructors take a `Mk` prefix**, non-punning. **Derive at least `Eq` and `Show`**; a type used as a `Set` element or `Map` key (or embedded in `Source`, which derives `Ord`) also derives `Ord`.
- **`Text` not `String`; arbitrary-precision numbers** (`Integer`, `Natural`).
- **No boolean blindness** — `MonarchTarget` is a two-constructor sum, not a `Bool`.
- **Two invariants outrank everything:** the rules core never cases on an effect's *identity* (only classifications); the engine never makes a player's choice. The only new casing homes are the sanctioned interpreters: `Pawl.Resolve` (effects), `Pawl.Codec` (the JSON boundary), `Pawl.Event`/`Pawl.Monarch` (triggers), `Pawl.Projection` (modifications/layers). Palace Jailer's exile target is a real CR 115 choice; becoming the monarch, the end-step draw, and the steal are automatic/mandatory — no elision is introduced.
- **Every CR claim re-checked against `docs/rules.txt` and cited in-code.** Card text is from Scryfall (2026-07-23), never recalled. Palace Jailer is `{2}{W}{W}`, Creature — Human Soldier, **2/2** (the recalled "1/1 for {3}{W}" is wrong — re-read the printed text at implementation time).
- **TDD, non-negotiable:** write each failing test, run it, watch it fail, then implement. One small complete commit per task on `main`. Commit trailer per CLAUDE.md.
- **`hooky` before done:** `git add -A`; `hooky fix`; `git add -A`; `hooky run` passes. Apply HLint suggestions or justify.

### Verified codebase facts (do not re-derive)

**Serialization surface (mirrors P10's discovery).**
- **`Source`, `GameState`, and `Object` have NO codec.** `Pawl.Codec` covers the `Card` closure plus `GameEvent`/`DamageEvent` only. So `Source.OfEmblem`/`OfInherentTrigger`, and the `GameState` fields `command`/`monarch`/`exiledUntilMonarch`, need **no codec arms**. The spec's §2.10 over-claims here — dropped (Task 8 files a standalone "GameState/Object/Source serialization" deferral issue).
- **`Expiry` has no codec** (spec §2.10 correct) — and this phase adds no `Expiry` constructor anyway (see the exile-return deviation below).
- Codecs that **do** exist and gain arms: `Zone` (`Codec.hs:344`, nullary enum), `Effect` (`:1124`), `TriggerCondition` (`:770`), `GameEvent` (`:1105`). `MonarchTarget` needs a brand-new nullary codec because `Effect.BecomeMonarch` embeds it.
- Codec idioms: nullary → `nullary (Text.pack "Name")` + `decodeNullary (Text.pack "T") [(Text.pack "Name", Ctor), …]`. Single tagged value → `Json.tagged (Text.pack "Name") (Just v)` + decode via `withValue mv (fmap Ctor . jsonToInner)`. Multi-arg → `Json.tagged … (Just (Array [a,b]))` + match `Just (Array [a,b])`. Reusable leaves: `cardToJson`/`jsonToCard` (`:1519`/`:1586`), `playerIdToJson`/`jsonToPlayerId` (bare `natTo`/`natFrom`, `:831`), `quantityToJson`/`jsonToQuantity`, `slotNameToJson`/`jsonToSlotName`.
- The `roundTrip` codec-test helper (`CodecSpec.hs:89`): `roundTrip label enc dec x = HU.assertEqual label (Right x) (dec (enc x))`. Codec tests are explicit example-based (no QuickCheck `Arbitrary`).

**`Zone` (`Library | Hand | Graveyard | Battlefield | Stack | Exile`, deriving `(Eq, Ord, Show)`).** Adding `Command` is compiler-forced in exactly these `case zone`/`case Object.zone` sites: `Codec.zoneToJson` (`:344`, add tag), `Game.zoneMembers` (`Game.hs:44-50`), `Game.insertIntoZone` (`Game.hs:76-83`), `Event.sacrifice` (`Event.hs:257-263`). Plus **non-forced but required for parity**: the `Codec.jsonToZone` `decodeNullary` list (`:353`), and `Game.removeFromZones` (`Game.hs:65-74`, a record update — add a `command` delete clause or command-zone objects leak on move). The command zone is a shared `Set ObjectId` — `zoneMembers`/`insertIntoZone`/`removeFromZones` treat it exactly like `Battlefield`/`Exile` (owner-filtered on read). All other `Zone.X` occurrences are constructor uses or `==`, no forced arm.

**`Source` (`OfCard Printing | OfToken Card | OfAbility ObjectId (ActivatedAbility Card) | OfTrigger ObjectId (TriggeredAbility Card)`, deriving `(Eq, Ord, Show)`).** Adding `OfEmblem Card` and `OfInherentTrigger PlayerId (TriggeredAbility Card)` (both `Ord`-safe: `Card` and `TriggeredAbility` derive `Ord`) is compiler-forced in these `case Object.source` sites (each needs **both** new arms): `Stack.resolveTop` (`Stack.hs:39-69`), `Cost.costsFor` (`Cost.hs:72-89`), `Action.isLandObject` (`Action.hs:23-29`), `Game.cardOf` (`Game.hs:86-93`), `Game.isSpell` (`Game.hs:100-108`). Non-forced (wildcard): `Sba.isVanishingToken` (`Sba.hs:174-178`) — leave untouched (an emblem is not a token and never vanishes). New arms per site: emblem `cardOf → Just card`, inherent `cardOf → Nothing`; both `costsFor → []`, `isLandObject → False`, `isSpell → False`; `resolveTop` emblem → drop like `OfToken`, inherent → resolve like `OfTrigger` (Task 5).

**`Effect card`** is parameterized. Adding a constructor is compiler-forced in `Resolve.slotsOf`, `Resolve.readsX`/`effectReadsX`, `Resolve.manaProduced`, `Resolve.searchesLibrary`, `Resolve.rewriteEffect`, `Resolve.applyEffect`; it falls through wildcards (no change) in `Resolve.textChangeSlots`, `Resolve.armedAbilities`, `Resolve.definedSlots`, `Resolve.bindsSeveralTokens`. `Effect.Create` (`Resolve.hs:569`) is the token-mint template; `Effect.GainPlayerCounters` (`Resolve.hs:673`) is the targetless direct-`State.modify'` template; `Effect.MoveToZone`/`Effect.Destroy` (`Resolve.hs:498-522`) are the slot-target-then-`Event.changeZone` template. `applyEffect :: ObjectId -> PlayerId -> Map SlotName (Subtype,Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect Card -> Game ()`; arg 1 `source`, arg 2 `controller` (the resolving controller / "you"), arg 5 `chosen` (slot→Recipient, includes the reserved `Binding.triggerSource` = `"self"` slot). `recipientObject :: Recipient -> Maybe ObjectId` (`Resolve.hs:782`). `Resolve` already imports `Pawl.Binding`, `Pawl.Projection`, `Pawl.Event`.

**Effect resolution primitives.** `Event.createTokens`/`Event.placeObject` (`Event.hs:292`/`:102`) mint a fresh object (`Game.freshObjectId`+`Game.freshTimestamp`) and `Game.insertIntoZone dest`. `Event.changeZone :: ObjectId -> Zone -> Game ()` (`Event.hs:115`) is the zone-change funnel — it mints a **new** incarnation id internally (`newId`) and records it in `GameEvent.Moved`, but **returns `()`** (Task 7 refactors it to expose `newId`). `Event.recordEvent :: GameEvent -> GameState -> GameState` (`Event.hs:60`) is the sole event-append, used via `State.modify' (recordEvent …)`. `Game.cardOf`/`Game.lookupObject`/`Game.freshObjectId`/`Game.freshTimestamp` per `Game.hs`.

**Monarch scanning.** The trigger scanner is `Pawl.Event`; `Engine.placePendingTriggers` (`Engine.hs:207`) gathers via `Event.gatherTriggers (Event.unscannedEvents gs) gs`, bumps `scannedThrough`, then `placeOne` (`Engine.hs:234`) mints each stack object. `PendingTrigger.source :: ObjectId` **cannot** carry a sourceless trigger — inherent monarch triggers get a dedicated gather+place path (Task 5), NOT a `PendingTrigger`. `Engine.settleForPriority` (`Engine.hs:372`) is the settle loop: `swept <- Expiry.sweepConditional; acted <- Sba.performStateBasedActions; placed <- placePendingTriggers; Monad.when (swept||acted||placed) settleForPriority`. `Projection.isCreatureOf`/`Projection.controllerOf` per `Projection.hs`. `DamageEvent = MkDamageEvent { source :: ObjectId, target :: Recipient, amount :: Natural, dealtByDeathtouch :: Bool, dealtByInfect :: Bool, kind :: DamageKind }`. `Recipient = ToCreature ObjectId | ToPlayer PlayerId | ToObject ObjectId`.

**Construction primitives.** `Modal card = MkModal { modes :: Seq (Mode card), selection :: ModeSelection }`; `Mode card = MkMode { effects :: Seq (Effect card), targetSpecs :: Map SlotName TargetSpec }`; `ModeSelection` = `ChooseExactly Natural` (newtype); `TriggeredAbility card = MkTriggeredAbility { condition :: TriggerCondition, modal :: Modal card, intervening :: Maybe StateCondition }`. `Phase.Ending EndingStep.EndStep` (there is **no** `End` constructor — the spec's `Ending End` is wrong). `TurnScope = EachTurn | ControllersTurn`. `PlayerRelation = You | Opponent`. `StaticAbility = MkStaticAbility { affected :: Affected, modification :: Modification }`; `Affected.Matching Exclusion Filter`; `Exclusion.IncludesSource`; `Filter.ControlledBy PlayerRelation`; `Filter.And [Filter]`; `Filter.HasCardType CardType`; `Modification.ModifyPowerToughness Quantity Quantity` (layer 7c). `Binding.setYou`, `Binding.setTriggerSource`, `Binding.fromChoices targets subtypes mAmount mModes` (4th arg is a `Set ModeIndex`), `Binding.triggerSource` (`= "self"` slot).

**`Projection.affects`** (`Projection.hs:182`) evaluates a `Matching` affected set with `Filter.MkContext Nothing` and `viewOfCharacteristics oid partial Nothing gs` — both perspectives hard-coded `Nothing`, documented as valid *only while no affected-set filter references a player*. `Filter.matches`' `ControlledBy` returns `False` unless **both** the View's `controller` (the affected object's controller) and the Context `perspective` (the "you" = source's controller) are `Just`. The emblem anthem "creatures you control get +1/+1" is the first affected-set filter to reference a player, so Task 3 supplies both (`controllerOf oid gs` and `controllerOf source gs`). Safe: no existing affected-set filter reads `ControlledBy`, and those are the only two fields the change touches.

**Tests.** No new `Pawl.*Spec` module: Palace Jailer characteristics → `CardSpec.hs` (Master Thief pattern, `CardSpec.hs:488`); emblem + monarch gameplay → `ExpirySpec.hs` (`masterThiefTests` pattern, `ExpirySpec.hs:229`, which drives ETB-target through `Engine.settleForPriority` + `Engine.priorityLoop` after fabricating the entry event with `S.withEvent (GameEvent.Moved (ZoneChange.MkZoneChange oid Zone.Stack Zone.Battlefield) (Projection.project oid gs)) gs`). Support helpers: `S.addCreature`, `S.addToken :: Card -> PlayerId -> GameState -> (ObjectId, GameState)`, `S.addLibraryCard`, `S.runPure answerer gs action`, `S.settleSba`, `S.combatBoardOf`, `S.fightWith`, `S.withEvent`, `S.lifeOf`, `S.identityAnswer` (`ChooseTargets` → `Set.lookupMin`). New card into `Cards.hs` needs three edits: a `<name>Printing :: Printing.Printing` field in `MkCards`, `<name>Printing_ <- loadPrinting "<slug>"` + `<name>Printing = <name>Printing_` in `loadCards`, and `<name>Printing cards` in `allPrintings` (which auto-covers it in the honesty round-trips). Only **two** `MkGameState` literals exist (`Setup.hs`, `Support.hs`) — both need the three new fields.

---

## File Structure

**New files**
- `source/library/Pawl/Type/MonarchTarget.hs` — `TheController | ControllerOfSource`.
- `source/library/Pawl/Monarch.hs` — logic module: the two inherent abilities, their event-matcher, the inherent-trigger gatherer/placer, and the exile-return sweep.
- `data/cards/palace-jailer.json` — the monarch gate card.

**Modified — library**
- `Pawl/Type/Zone.hs` (+`Command`), `Pawl/Type/Source.hs` (+`OfEmblem`,`OfInherentTrigger`), `Pawl/Type/Effect.hs` (+`CreateEmblem`,`BecomeMonarch`,`ExileUntilMonarch`), `Pawl/Type/GameEvent.hs` (+`BecameMonarch`), `Pawl/Type/TriggerCondition.hs` (+`CreatureDealtCombatDamageToMonarch`), `Pawl/Type/GameState.hs` (+`command`,`monarch`,`exiledUntilMonarch`).
- `Pawl/Setup.hs` (seed the three fields), `Pawl/Game.hs` (Zone/Source arms), `Pawl/Event.hs` (Zone `sacrifice` arm; `changeZoneReturning`), `Pawl/Stack.hs` (Source arms), `Pawl/Cost.hs` (Source arms), `Pawl/Action.hs` (Source arm), `Pawl/Resolve.hs` (Effect classifier arms + `CreateEmblem`/`BecomeMonarch`/`ExileUntilMonarch` executors), `Pawl/Projection.hs` (command-zone gather + `affects` perspective), `Pawl/Engine.hs` (place inherent triggers; wire the return sweep), `Pawl/Codec.hs` (Zone/Effect/TriggerCondition/GameEvent/MonarchTarget arms).

**Modified — tests**
- `Pawl/Support.hs` (`withMonarch`, `command`/`monarch`/`exiledUntilMonarch` in its `MkGameState`), `Pawl/Cards.hs` (Palace Jailer ×3), `Pawl/CodecSpec.hs`, `Pawl/SetupSpec.hs`, `Pawl/ZoneSpec.hs` (or `CodecSpec` for the Zone round-trip), `Pawl/ProjectionSpec.hs` (emblem), `Pawl/CardSpec.hs` (Palace Jailer characteristics), `Pawl/ExpirySpec.hs` (monarch + emblem + Palace Jailer gameplay).

Task order follows spec §7: GAP-Z (Tasks 1–3) then the monarch (Tasks 4–7), each a single compiling commit.

---

### Task 1: `Zone.Command`, `GameState.command`, and the zone plumbing

**Files:**
- Modify: `source/library/Pawl/Type/Zone.hs`, `source/library/Pawl/Type/GameState.hs`
- Modify: `source/library/Pawl/Game.hs` (`zoneMembers`, `insertIntoZone`, `removeFromZones`), `source/library/Pawl/Event.hs` (`sacrifice`), `source/library/Pawl/Codec.hs` (`zoneToJson`, `jsonToZone`), `source/library/Pawl/Setup.hs`
- Modify: `source/test-suite/Pawl/Support.hs` (its `MkGameState` literal)
- Test: `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/SetupSpec.hs`

**Interfaces:**
- Produces: `Zone.Command`; `GameState.command :: Set ObjectId`; the four `Zone`-case arms.

- [ ] **Step 1: Write the failing Zone codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, beside the existing `Zone` round-trip cases (search `Codec.zoneToJson`), add:

```haskell
          HU.testCase "Zone.Command" $
            roundTrip "command" Codec.zoneToJson Codec.jsonToZone Zone.Command,
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'Zone.Command'`.

- [ ] **Step 3: Add the `Command` constructor**

In `source/library/Pawl/Type/Zone.hs`, add after `Exile`:

```haskell
  | Exile
  | -- CR 400.1 / 408.1: the command zone -- a game area for objects with an
    -- overarching effect that are not permanents and cannot be destroyed. Shared
    -- across players (not per-player), like Battlefield and Exile. Emblems (CR
    -- 114) are its first resident.
    Command
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the `command` field to `GameState`**

In `source/library/Pawl/Type/GameState.hs`, add after `exile`:

```haskell
    exile :: Set ObjectId,
    -- CR 400.1: the command zone -- a shared collection (not per-player), keyed
    -- into `objects` like `battlefield`/`exile`. Emblems live here; their static
    -- abilities are gathered live by the projection (Pawl.Projection.gather).
    command :: Set ObjectId,
```

- [ ] **Step 5: Add the four `Zone` case arms and the parity edits**

In `source/library/Pawl/Game.hs`:
- `zoneMembers`, after the `Exile` arm: `Zone.Command -> ownedShared (GameState.command gs)`.
- `insertIntoZone`, after the `Exile` arm: `Zone.Command -> gs {GameState.command = Set.insert oid (GameState.command gs)}`.
- `removeFromZones`, add to the record update: `GameState.command = Set.delete oid (GameState.command gs),`.

In `source/library/Pawl/Event.hs`, `sacrifice`'s `case Object.zone obj of`, add:

```haskell
        -- CR 408.1: a command-zone object is not a permanent, so it is never
        -- sacrificed.
        Zone.Command -> pure ()
```

In `source/library/Pawl/Codec.hs`: add `Zone.Command -> "Command"` to `zoneToJson`, and `(Text.pack "Command", Zone.Command)` to `jsonToZone`'s `decodeNullary` list.

- [ ] **Step 6: Seed `command` at both `MkGameState` sites**

In `source/library/Pawl/Setup.hs` (`emptyGame`), add `GameState.command = mempty,` beside `GameState.exile = mempty,`. In `source/test-suite/Pawl/Support.hs`, find its `MkGameState` literal and add `GameState.command = mempty,`.

- [ ] **Step 7: Write the failing setup test**

In `source/test-suite/Pawl/SetupSpec.hs`, add:

```haskell
      HU.testCase "CR 400.1 a new game's command zone is empty" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "empty command" mempty (GameState.command gs),
```

- [ ] **Step 8: Run the suite; both new tests pass**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (`Zone.Command`, `command zone is empty`), whole suite green.

- [ ] **Step 9: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): the Command zone and its empty GameState collection

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 2: `Source.OfEmblem`, `Effect.CreateEmblem`, and emblem minting

**Files:**
- Modify: `source/library/Pawl/Type/Source.hs`, `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Game.hs` (`cardOf`, `isSpell`), `source/library/Pawl/Stack.hs` (`resolveTop`), `source/library/Pawl/Cost.hs` (`costsFor`), `source/library/Pawl/Action.hs` (`isLandObject`)
- Modify: `source/library/Pawl/Resolve.hs` (Effect classifier arms + `CreateEmblem` executor), `source/library/Pawl/Codec.hs` (`effectToJson`, `jsonToEffect`)
- Test: `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `Zone.Command`, `Game.insertIntoZone` (Task 1).
- Produces: `Source.OfEmblem Card`; `Effect.CreateEmblem Card`; `Source.OfInherentTrigger` is **added in Task 5** — but its case-arms in the five `case Object.source` sites are added **now** so Task 5 need not re-touch them. (Add both constructors' arms; `OfInherentTrigger` constructor itself lands in Task 5, so guard: add `Source.OfEmblem` now and leave a `-- Task 5: OfInherentTrigger` note, OR add both constructors in this task and only wire the scanner in Task 5. **Chosen: add both `Source` constructors here** so every `case Object.source` becomes exhaustive in one commit; Task 5 only adds `Pawl.Monarch` + engine wiring.)

- [ ] **Step 1: Write the failing effect codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, in the `Effect` round-trip group, add (reusing a loaded card as the embedded emblem characteristics — no hand-built `Card`):

```haskell
          HU.testCase "CreateEmblem" $
            roundTrip "emblem" Codec.effectToJson Codec.jsonToEffect (Effect.CreateEmblem (Printing.card (Cards.pikerPrinting cards))),
```

(Ensure `Printing`/`Cards` are imported in `CodecSpec`; the honesty group already uses `Cards.allPrintings`.)

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'Effect.CreateEmblem'`.

- [ ] **Step 3: Add the two `Source` constructors**

In `source/library/Pawl/Type/Source.hs`, add after `OfTrigger`:

```haskell
  | -- CR 114: an emblem -- an object in the command zone whose only
    -- characteristics are its abilities (CR 114.3). Its characteristics ARE a
    -- Card (like OfToken), so Game.cardOf reads it with no special case; unlike a
    -- token it is never a permanent (CR 114.5) and never on the battlefield.
    -- Owned and controlled by the player who created it (CR 114.2 / 109.4c).
    OfEmblem Card
  | -- CR 725.2: a triggered ability with no object source, controlled by a
    -- specific player baked in at trigger time (like DelayedTrigger's controller).
    -- The monarch's two inherent abilities are the only customers.
    OfInherentTrigger PlayerId (TriggeredAbility Card)
  deriving (Eq, Ord, Show)
```

Add `import Pawl.Type.PlayerId (PlayerId)` to `Source.hs` (it already imports `Card`, `TriggeredAbility`).

- [ ] **Step 4: Add the five `case Object.source` arms**

Each site (`Stack.resolveTop`, `Cost.costsFor`, `Action.isLandObject`, `Game.cardOf`, `Game.isSpell`) gains both arms. For this task, resolution of `OfInherentTrigger` on the stack is not yet exercised (no inherent trigger is placed until Task 5), so give it the same shape as `OfTrigger` will use; the simplest exhaustive-and-correct arm now is to resolve like `OfTrigger` (Task 5 tests it). Concretely:

`source/library/Pawl/Game.hs` — `cardOf`:
```haskell
    Source.OfEmblem card -> Just card
    Source.OfInherentTrigger _ _ -> Nothing
```
`isSpell` inner case:
```haskell
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> False
```
`source/library/Pawl/Cost.hs` — `costsFor` (`OfToken/OfAbility/OfTrigger` already return `[]`):
```haskell
        Source.OfEmblem _ -> []
        Source.OfInherentTrigger _ _ -> []
```
`source/library/Pawl/Action.hs` — `isLandObject`:
```haskell
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ _ -> False
```
`source/library/Pawl/Stack.hs` — `resolveTop`:
```haskell
        -- CR 114.5: an emblem is never on the stack (created into the command
        -- zone, never cast). Drop it, like a token.
        Source.OfEmblem _ -> State.put gs {GameState.stack = rest}
        Source.OfInherentTrigger _ ability ->
          -- CR 725.2: an inherent monarch ability has no source object and no
          -- intervening "if" (intervening = Nothing); resolve its effects
          -- directly. Object.owner is the monarch (baked at placement) -- "you".
          let chosen = Binding.modesOf (Object.bindings obj)
              modal = TriggeredAbility.modal ability
           in Resolve.resolveEffects oid oid (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)
```
(`Stack.hs` already imports `Binding`, `Modal`, `TriggeredAbility`, `Resolve`. Leave `Sba.isVanishingToken`'s wildcard untouched.)

- [ ] **Step 5: Add the `CreateEmblem` opcode**

In `source/library/Pawl/Type/Effect.hs`, add (beside `Create`):

```haskell
  | -- CR 114.2: "[you] get an emblem with [abilities]." Puts an emblem owned and
    -- controlled by the resolving controller into the command zone. Targetless
    -- (the beneficiary is always the resolving controller); the abilities ride a
    -- Card so the emblem reuses the whole ability pipeline. First-order: a data
    -- Card, tied to Card by Card's own instantiation, exactly as Create's is.
    CreateEmblem card
```

- [ ] **Step 6: Add the Effect classifier arms and the executor in `Resolve.hs`**

- `slotsOf`: `Effect.CreateEmblem {} -> Set.empty`
- `readsX`/`effectReadsX`: `Effect.CreateEmblem {} -> False`
- `manaProduced`: `Effect.CreateEmblem {} -> Nothing`
- `searchesLibrary`: `Effect.CreateEmblem {} -> False`
- `rewriteEffect`: `Effect.CreateEmblem {} -> effect`
- `applyEffect` (mint the emblem via `Event.placeObject`, into the command zone; reuse the fresh-id/timestamp/insert primitive):

```haskell
  Effect.CreateEmblem card -> do
    -- CR 114.2 / 613.7a: the emblem enters the command zone under the resolving
    -- controller; its entry timestamp is what the projection reads when ordering
    -- its static ability's continuous effect. Inert per-incarnation fields (it is
    -- never tapped/damaged/countered): harmless, nothing reads them here.
    let mkObj ts =
          Object.MkObject
            { Object.owner = controller,
              Object.source = Source.OfEmblem card,
              Object.zone = Zone.Command,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Settled,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.timestamp = ts
            }
    _ <- Event.placeObject controller mkObj Zone.Command
    pure ()
```

Add whatever imports this needs to `Resolve.hs` (`Pawl.Type.Object`, `Pawl.Type.TapState`, `Pawl.Type.Sickness`, `Pawl.Type.Zone`, `Pawl.Type.Source`) — check which are already present; add only the missing ones.

- [ ] **Step 7: Add the effect codec arms**

`source/library/Pawl/Codec.hs`, `effectToJson`:
```haskell
  Effect.CreateEmblem c -> Json.tagged (Text.pack "CreateEmblem") (Just (cardToJson c))
```
`jsonToEffect`:
```haskell
    "CreateEmblem" -> withValue mv (fmap Effect.CreateEmblem . jsonToCard)
```

- [ ] **Step 8: Write the failing minting behavior test**

In `source/test-suite/Pawl/ResolveSpec.hs`, drive `applyEffect` directly (mirror the `Draw`/`GainPlayerCounters` direct-call tests; `source` is any object id — use a fresh piker):

```haskell
      HU.testCase "CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver" $
        let (src, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            act = Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (Printing.card (Cards.pikerPrinting cards)))
            after = S.runPure S.identityAnswer gs0 act
            emblems = filter (\oid -> fmap Object.zone (Game.lookupObject oid after) == Just Zone.Command) (Set.toList (GameState.command after))
         in do
              HU.assertEqual "one emblem in command" 1 (Set.size (GameState.command after))
              HU.assertEqual "owned by the resolver" [Just S.alice] (map (\oid -> fmap Object.owner (Game.lookupObject oid after)) emblems),
```

- [ ] **Step 9: Run to verify pass, whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (`CreateEmblem` round-trip; the minting test), whole suite green.

- [ ] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): OfEmblem source and the CreateEmblem opcode

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 3: The projection command-zone gather pass — closes GAP-Z

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (`gather`, `affects`)
- Modify: `source/test-suite/Pawl/Support.hs` (an emblem-anthem card fixture, labeled synthetic)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `GameState.command`, `Source.OfEmblem`, `Effect.CreateEmblem`.
- Produces: `Projection.gather` walks the command zone; `Projection.affects` supplies the source's-controller perspective; `S.anthemEmblemCard :: Card` fixture and `S.createEmblem` runner helper.

- [ ] **Step 1: Add the synthetic emblem-source fixture to `Support`**

In `source/test-suite/Pawl/Support.hs`, add (a **labeled synthetic crutch** — Task 8 files its expiry issue; built by overriding a loaded vanilla card's abilities so no full `Card` literal is needed):

```haskell
-- LABELED SYNTHETIC (expires when a real emblem source lands, see the P11 plan's
-- deferral issue): an emblem's characteristics are only its abilities (CR 114.3),
-- but pawl models no planeswalker/Ring to mint one, so tests use this fixture --
-- an Elspeth-style anthem, "creatures you control get +1/+1". Built by overriding
-- a vanilla card's static abilities; the residual printed fields are inert for a
-- command-zone object (never projected as a permanent). (#TBD-emblem-source)
anthemEmblemCard :: Cards.Cards -> Card.Type.Card
anthemEmblemCard cards =
  (Printing.card (Cards.pikerPrinting cards))
    { Card.Type.staticAbilities =
        [ StaticAbility.MkStaticAbility
            { StaticAbility.affected =
                Affected.Matching
                  Exclusion.IncludesSource
                  (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You]),
              StaticAbility.modification =
                Modification.ModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 1)
            }
        ]
    }
```

Add the imports `Printing`, `StaticAbility`, `Affected`, `Exclusion`, `Filter`, `CardType`, `PlayerRelation`, `Modification`, `Quantity` (aliased to last component). `Printing.card` is the printing→card accessor (confirmed: `CostSpec` uses `Printing.card (Cards.longtuskCubPrinting cards)`); `Card.Type.staticAbilities` is the field being overridden.

- [ ] **Step 2: Write the failing emblem-anthem projection tests**

In `source/test-suite/Pawl/ProjectionSpec.hs`, add a group. These mint the emblem via `Effect.CreateEmblem`, then read a creature's projected power:

```haskell
      HU.testCase "CR 114.4 an emblem's anthem buffs the controller's creatures from the command zone" $
        let (creature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
         in HU.assertEqual "piker is 2/1 -> 3/2" (Just 3) (Projection.powerOf creature withEmblem),
      HU.testCase "CR 114.4 the anthem is scoped to the controller's creatures" $
        let (mine, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (theirs, gs1) = S.addCreature (Cards.pikerPrinting cards) S.bob gs0
            withEmblem = S.runPure S.identityAnswer gs1 (Resolve.applyEffect mine S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
         in do
              HU.assertEqual "alice's creature buffed" (Just 3) (Projection.powerOf mine withEmblem)
              HU.assertEqual "bob's creature untouched" (Just 2) (Projection.powerOf theirs withEmblem),
      HU.testCase "CR 114.5 the emblem survives a battlefield wipe and buffs a fresh token" $
        let (creature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
            wiped = withEmblem {GameState.battlefield = mempty, GameState.objects = Map.filterWithKey (\oid _ -> Set.member oid (GameState.command withEmblem)) (GameState.objects withEmblem)}
            (token, afterToken) = S.addCreature (Cards.pikerPrinting cards) S.alice wiped
         in HU.assertEqual "emblem still buffs the new creature" (Just 3) (Projection.powerOf token afterToken),
```

(Confirm the piker's printed power is 2; if it differs, adjust the `Just 3`/`Just 2` expectations to `printed+1`/`printed`. Use whatever vanilla card `Support` already exposes if `pikerPrinting` is not a 2/x.)

- [ ] **Step 3: Run to verify they fail**

Run: `cabal test --test-options='-p "emblem"' 2>&1 | tail -25`
Expected: FAIL — power is `Just 2` (anthem not gathered from the command zone) and/or `ControlledBy` matches nothing (perspective `Nothing`).

- [ ] **Step 4: Give `affects` the source's-controller perspective**

In `source/library/Pawl/Projection.hs`, `affects`, replace the `Matching` arm's `Filter.matches` call so both the affected object's controller and the source's-controller perspective are supplied (CR 109.5):

```haskell
  Affected.Matching exclusion f ->
    let notExcluded = case exclusion of
          Exclusion.ExcludesSource -> oid /= source
          Exclusion.IncludesSource -> True
        -- CR 109.5: "you" on a continuous effect is the effect's SOURCE's
        -- controller; ControlledBy compares the affected object's controller to
        -- it. Both were Nothing while no affected-set filter referenced a player
        -- (the emblem anthem is the first that does).
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
          && notExcluded
          && Filter.matches (Filter.MkContext perspective) (viewOfCharacteristics oid partial (controllerOf oid gs) gs) f
```

Update the `affects` doc comment: the "no affected-set filter references a player, so … Nothing" clause is now retired (an emblem's `ControlledBy You` anthem is the consumer); cite CR 109.5.

- [ ] **Step 5: Add the command-zone gather pass**

In `source/library/Pawl/Projection.hs`, `gather`, add a `fromEmblem` pass over `GameState.command` and fold it into the result. Structurally identical to `fromPermanent` minus the `SetLandSubtype` liveness / text-change machinery (an emblem has no basic-land-type text and cannot be stripped):

```haskell
      fromEmblem emblemId = case Game.lookupObject emblemId gs of
        Nothing -> []
        Just emblemObj -> case Game.cardOf emblemId gs of
          Nothing -> []
          Just card ->
            -- CR 114.4 / 113.6: an emblem's abilities function in the command
            -- zone. Its static ability's continuous effect shares the emblem's
            -- entry timestamp (CR 613.7a). No liveness/text-change pass: nothing
            -- in scope strips an emblem's abilities or rewrites land types.
            map
              ( \sa ->
                  MkGathered
                    { gSource = emblemId,
                      gAffected = StaticAbility.affected sa,
                      gLayer = layer (StaticAbility.modification sa),
                      gTimestamp = Object.timestamp emblemObj,
                      gModification = StaticAbility.modification sa
                    }
              )
              (Card.Type.staticAbilities card)
      emblems = concatMap fromEmblem (Set.toList (GameState.command gs))
```

and change the final line from `stored ++ static_ ++ counterGathered gs` to `stored ++ static_ ++ emblems ++ counterGathered gs`.

- [ ] **Step 6: Run to verify pass; whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (all three emblem cases). **GAP-Z closed** — a static ability functions from the command zone, controller-scoped, surviving a battlefield wipe.

- [ ] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): the projection gathers command-zone emblem statics (GAP-Z)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 4: `GameState.monarch`, `MonarchTarget`, `Effect.BecomeMonarch`, `GameEvent.BecameMonarch`

**Files:**
- Create: `source/library/Pawl/Type/MonarchTarget.hs`
- Modify: `source/library/Pawl/Type/GameState.hs` (+`monarch`), `source/library/Pawl/Type/Effect.hs` (+`BecomeMonarch`), `source/library/Pawl/Type/GameEvent.hs` (+`BecameMonarch`)
- Modify: `source/library/Pawl/Resolve.hs` (classifier arms + `BecomeMonarch` executor), `source/library/Pawl/Event.hs` (`movedOf`/`damageOf`/`castOf` arms), `source/library/Pawl/Setup.hs`, `source/library/Pawl/Codec.hs`
- Modify: `source/test-suite/Pawl/Support.hs` (`withMonarch`, `monarch` field)
- Test: `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/SetupSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `GameState.monarch :: Maybe PlayerId`; `MonarchTarget.MonarchTarget` = `TheController | ControllerOfSource`; `Effect.BecomeMonarch MonarchTarget`; `GameEvent.BecameMonarch PlayerId`; `S.withMonarch :: PlayerId -> GameState -> GameState`.

- [ ] **Step 1: Write the failing codec round-trips**

In `source/test-suite/Pawl/CodecSpec.hs`, add a `MonarchTarget` round-trip (needs a new `Codec.monarchTargetToJson`/`jsonToMonarchTarget`), a `GameEvent.BecameMonarch` round-trip, and a `BecomeMonarch` effect round-trip:

```haskell
          HU.testCase "MonarchTarget" $ do
            roundTrip "tc" Codec.monarchTargetToJson Codec.jsonToMonarchTarget MonarchTarget.TheController
            roundTrip "cos" Codec.monarchTargetToJson Codec.jsonToMonarchTarget MonarchTarget.ControllerOfSource,
          HU.testCase "GameEvent.BecameMonarch" $
            roundTrip "bm" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.BecameMonarch S.alice),
          HU.testCase "BecomeMonarch" $
            roundTrip "e" Codec.effectToJson Codec.jsonToEffect (Effect.BecomeMonarch MonarchTarget.TheController),
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `MonarchTarget` / `Effect.BecomeMonarch` / `GameEvent.BecameMonarch` not in scope.

- [ ] **Step 3: Create `MonarchTarget`**

Create `source/library/Pawl/Type/MonarchTarget.hs`:

```haskell
module Pawl.Type.MonarchTarget where

-- Which player an Effect.BecomeMonarch names. pawl has no general "which player"
-- spec for effects yet (#120 tracks the targeted case); these two cases are the
-- entire need this phase, so a minimal two-constructor sum is the honest shape.
data MonarchTarget
  = -- "you become the monarch" (Palace Jailer's ETB): the resolving controller.
    TheController
  | -- CR 725.2: "its controller becomes the monarch" (the steal): the controller
    -- of the object bound as the ability's source (the damaging creature).
    ControllerOfSource
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add `GameState.monarch` and seed it**

In `source/library/Pawl/Type/GameState.hs`, add after `activeControl` (import `PlayerId` is already present):

```haskell
    activeControl :: Maybe Decider,
    -- CR 725.1/725.3: the monarch, a single game-wide player designation (at most
    -- one at a time). Nothing until a player becomes the monarch. On GameState,
    -- not Player, because it is one designation, not a per-player counter.
    monarch :: Maybe PlayerId
```

In `source/library/Pawl/Setup.hs` add `GameState.monarch = Nothing,`; in `source/test-suite/Pawl/Support.hs`'s `MkGameState` add `GameState.monarch = Nothing,`.

- [ ] **Step 5: Add `Effect.BecomeMonarch` and `GameEvent.BecameMonarch`**

In `source/library/Pawl/Type/Effect.hs` add `import Pawl.Type.MonarchTarget (MonarchTarget)` and:

```haskell
  | -- CR 725: a player becomes the monarch. Targetless; the beneficiary is named
    -- by the MonarchTarget (the resolving controller, or the controller of the
    -- ability's bound source). Setting the monarch emits GameEvent.BecameMonarch.
    BecomeMonarch MonarchTarget
```

In `source/library/Pawl/Type/GameEvent.hs`:

```haskell
  | -- CR 725.1: a player became the monarch. What Palace Jailer's exile duration
    -- keys off, and the substrate for any future "whenever a player becomes the
    -- monarch" trigger.
    BecameMonarch PlayerId
```

- [ ] **Step 6: Add the `GameEvent` arms (Codec + Event readers)**

In `source/library/Pawl/Codec.hs`, `gameEventToJson`: `GameEvent.BecameMonarch pid -> Json.tagged (Text.pack "BecameMonarch") (Just (playerIdToJson pid))`; `jsonToGameEvent`: `("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> jsonToPlayerId v`. Add the `MonarchTarget` codec beside it:

```haskell
monarchTargetToJson :: MonarchTarget.MonarchTarget -> Value
monarchTargetToJson t = nullary . Text.pack $ case t of
  MonarchTarget.TheController -> "TheController"
  MonarchTarget.ControllerOfSource -> "ControllerOfSource"

jsonToMonarchTarget :: Value -> Either Text MonarchTarget.MonarchTarget
jsonToMonarchTarget =
  decodeNullary
    (Text.pack "MonarchTarget")
    [ (Text.pack "TheController", MonarchTarget.TheController),
      (Text.pack "ControllerOfSource", MonarchTarget.ControllerOfSource)
    ]
```
and `effectToJson`: `Effect.BecomeMonarch t -> Json.tagged (Text.pack "BecomeMonarch") (Just (monarchTargetToJson t))`; `jsonToEffect`: `"BecomeMonarch" -> withValue mv (fmap Effect.BecomeMonarch . jsonToMonarchTarget)`.

In `source/library/Pawl/Event.hs`, add a `False`/`Nothing`-shaped arm for `GameEvent.BecameMonarch` in each exhaustive `case event` reader that lacks a wildcard (`movedOf`, `damageOf`, `castOf` at `Event.hs:64-85`, and any `matchesTrigger` inner cases — the `-Werror` build names them precisely).

- [ ] **Step 7: Add the `BecomeMonarch` classifier arms and executor in `Resolve.hs`**

- `slotsOf`: `Effect.BecomeMonarch {} -> Set.empty`
- `readsX`: `Effect.BecomeMonarch {} -> False`
- `manaProduced`: `Effect.BecomeMonarch {} -> Nothing`
- `searchesLibrary`: `Effect.BecomeMonarch {} -> False`
- `rewriteEffect`: `Effect.BecomeMonarch {} -> effect`
- `applyEffect`:

```haskell
  Effect.BecomeMonarch target -> do
    gs <- State.get
    let newMonarch = case target of
          -- "you become the monarch."
          MonarchTarget.TheController -> Just controller
          -- CR 725.2: the controller of the ability's bound source (the damaging
          -- creature), read from the reserved trigger-source slot.
          MonarchTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= recipientObject
              >>= (\o -> Projection.controllerOf o gs)
    case newMonarch of
      Nothing -> pure ()
      Just p -> do
        -- CR 725.3: the previous monarch ceases simply because `monarch` is
        -- overwritten (at most one at a time).
        State.modify' (\g -> g {GameState.monarch = Just p})
        State.modify' (Event.recordEvent (GameEvent.BecameMonarch p))
```

Add `import qualified Pawl.Type.MonarchTarget as MonarchTarget` and `import qualified Pawl.Type.GameEvent as GameEvent` to `Resolve.hs` if missing.

- [ ] **Step 8: Add the `withMonarch` support helper and a setup + behavior test**

In `source/test-suite/Pawl/Support.hs`:

```haskell
-- Set the monarch directly, for tests that need the designation without
-- resolving the effect that grants it.
withMonarch :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
withMonarch pid gs = gs {GameState.monarch = Just pid}
```

In `source/test-suite/Pawl/SetupSpec.hs`:
```haskell
      HU.testCase "CR 725.1 a new game has no monarch" $
        HU.assertEqual "no monarch" Nothing (GameState.monarch (Setup.emptyGame S.bothPlayers)),
```
In `source/test-suite/Pawl/ResolveSpec.hs`:
```haskell
      HU.testCase "CR 725 BecomeMonarch TheController makes the resolver the monarch" $
        let (src, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs0 (Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.BecomeMonarch MonarchTarget.TheController))
         in do
              HU.assertEqual "alice is monarch" (Just S.alice) (GameState.monarch after)
              HU.assertBool "a BecameMonarch event was recorded" (elem (GameEvent.BecameMonarch S.alice) (Foldable.toList (GameState.events after))),
```

- [ ] **Step 9: Run to verify pass; whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (three codec round-trips, no-monarch setup, TheController behavior).

- [ ] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): the monarch designation, BecomeMonarch, and its event

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 5: `Pawl.Monarch`, the inherent triggers, and the end-step draw

**Files:**
- Create: `source/library/Pawl/Monarch.hs`
- Modify: `source/library/Pawl/Engine.hs` (`placePendingTriggers` gathers/places inherent triggers)
- Test: `source/test-suite/Pawl/ExpirySpec.hs` (end-step draw)

**Interfaces:**
- Consumes: `GameState.monarch`, `Source.OfInherentTrigger`, `Effect.Draw`, `TriggerCondition.StepBegins`, `Binding.setYou`/`fromChoices`.
- Produces: `Monarch.monarchAbilities`, `Monarch.endStepDraw`, `Monarch.inherentMatch`, `Monarch.inherentMonarchPending`, `Monarch.placeInherent`.

- [ ] **Step 1: Write the failing end-step-draw tests**

In `source/test-suite/Pawl/ExpirySpec.hs` (reusing its `settle`/`resolveAll` helpers — `settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority`, `resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop`), add. The falsifier: no permanents on the battlefield at all, so the draw cannot hang on a bearer.

```haskell
      HU.testCase "CR 725.2 the monarch draws at the beginning of their own end step" $
        let (_, gs0) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            began = S.withEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice) gs0
            after = resolveAll (settle began)
         in HU.assertEqual "alice drew (one card now in hand)" 1 (length (Game.zoneMembers Zone.Hand S.alice after)),
      HU.testCase "CR 725.2 the end-step draw fires only on the monarch's own end step" $
        let (_, gs0) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob (S.withMonarch S.bob (Setup.emptyGame S.bothPlayers))
            -- alice is the active player; her end step is not bob's (the monarch).
            began = S.withEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice) gs0
            after = resolveAll (settle began)
         in HU.assertEqual "bob did not draw on alice's end step" 0 (length (Game.zoneMembers Zone.Hand S.bob after)),
```

(`Setup.emptyGame` makes `alice` the active player. Confirm `S.addLibraryCard`'s return shape; if it returns only the `GameState`, drop the `(_, …)` binding.)

- [ ] **Step 2: Run to verify they fail**

Run: `cabal test --test-options='-p "end step"' 2>&1 | tail -20`
Expected: FAIL — alice's hand is empty (no inherent draw is placed/resolved).

- [ ] **Step 3: Create `Pawl.Monarch` with the draw ability, matcher, gatherer, and placer**

Create `source/library/Pawl/Monarch.hs` (Task 6 adds the steal ability/arm to `monarchAbilities` and `inherentMatch`; write those two entries as single-element / partial now and extend them there):

```haskell
module Pawl.Monarch where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.Game (Game)
import Pawl.Type.GameEvent (GameEvent)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Phase as Phase
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.TriggerCondition (TriggerCondition)
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.Zone as Zone

-- A single-mode, single-effect triggered ability (the shape all monarch inherent
-- abilities take): one Mode with no targets, forced (ChooseExactly 1).
oneEffect :: TriggerCondition -> Effect.Effect Card -> TriggeredAbility Card
oneEffect cond eff =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = cond,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton eff) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }

-- CR 725.2: "At the beginning of the monarch's end step, that player draws a
-- card." Controller-scoped to the monarch, so ControllersTurn + the monarch as
-- "you" is exactly the monarch's own end step.
endStepDraw :: TriggeredAbility Card
endStepDraw =
  oneEffect
    (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.ControllersTurn)
    (Effect.Draw (Quantity.Literal 1))

-- The monarch's inherent abilities, present only while there is a monarch.
monarchAbilities :: [TriggeredAbility Card]
monarchAbilities = [endStepDraw]

-- CR 725.2: match one inherent ability against one event for the given monarch
-- (who is the ability's controller), yielding the placed ability's binding
-- environment (empty for the draw; Task 6 adds the steal's damaging-creature
-- binding). Sourceless -- no bearer, so this is a dedicated matcher, not
-- Event.matchesTrigger.
inherentMatch :: PlayerId -> TriggerCondition -> GameState -> GameEvent -> Maybe (Map SlotName.SlotName Binding)
inherentMatch monarch cond gs event = case (cond, event) of
  (TriggerCondition.StepBegins wanted scope, GameEvent.StepBegan began active)
    | began == wanted && scopeOk scope active -> Just Map.empty
  _ -> Nothing
  where
    scopeOk s a = case s of
      TurnScope.EachTurn -> True
      TurnScope.ControllersTurn -> a == monarch

-- CR 725.1/725.2: the inherent triggers that fire on this batch of events, each
-- as (controller, ability, bindings). Empty when there is no monarch (the
-- abilities do not exist).
inherentMonarchPending :: [GameEvent] -> GameState -> [(PlayerId, TriggeredAbility Card, Map SlotName.SlotName Binding)]
inherentMonarchPending events gs = case GameState.monarch gs of
  Nothing -> []
  Just m ->
    let forAbility ab =
          Maybe.mapMaybe
            (\ev -> fmap (\b -> (m, ab, b)) (inherentMatch m (TriggeredAbility.condition ab) gs ev))
            events
     in concatMap forAbility monarchAbilities

-- Mint a sourceless inherent trigger onto the stack (the placeOne analog for an
-- ability with no source object). Single mode, no targets -- so no mode/target
-- prompt; the single mode is selected outright.
placeInherent :: PlayerId -> TriggeredAbility Card -> Map SlotName.SlotName Binding -> Game ()
placeInherent controller ability provided = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      allModes = Set.fromList (map ModeIndex.MkModeIndex [0 .. fromIntegral (Seq.length (Modal.modes (TriggeredAbility.modal ability))) - 1])
      bindings = Binding.setYou controller (Map.union provided (Binding.fromChoices Map.empty Map.empty Nothing allModes))
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfInherentTrigger controller ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = bindings,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
```

Notes for the implementer to confirm against the real modules (adjust if wrong, do not guess past them):
- `ModeIndex.MkModeIndex :: Natural -> ModeIndex` and `Binding.fromChoices`' 4th argument is `Set ModeIndex` (placeOne passes a `Set ModeIndex` there). If that arg is a `Maybe (Set ModeIndex)`, pass `(Just allModes)`.
- The `Binding` type comes from `Pawl.Type.Binding` (imported unqualified as the bare field type, the `Data.Map (Map)` exception) and the helpers from `Pawl.Binding`. `Modal.modes` is a `Seq (Mode card)`.

- [ ] **Step 4: Gather and place inherent triggers in `placePendingTriggers`**

In `source/library/Pawl/Engine.hs`, add `import qualified Pawl.Monarch as Monarch`, and rewrite `placePendingTriggers` so it reads the unscanned events once and also places inherent triggers:

```haskell
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let evs = Event.unscannedEvents gs
      (pending, surviving) = Event.gatherTriggers evs gs
      inherent = Monarch.inherentMonarchPending evs gs
  State.put
    gs
      { GameState.scannedThrough = fromIntegral (Seq.length (GameState.events gs)),
        GameState.delayedTriggers = surviving
      }
  ordered <- orderPending pending
  Monad.mapM_ placeOne ordered
  Monad.mapM_ (\(p, ab, b) -> Monarch.placeInherent p ab b) inherent
  pure (not (null pending) || not (null inherent))
```

- [ ] **Step 5: Run to verify the end-step tests pass; whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (both end-step cases). The monarch draws on their own end step; a non-monarch does not draw on the active player's end step.

- [ ] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): sourceless inherent triggers and the monarch end-step draw

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 6: The combat-damage steal — `CreatureDealtCombatDamageToMonarch`

**Files:**
- Modify: `source/library/Pawl/Type/TriggerCondition.hs` (+`CreatureDealtCombatDamageToMonarch`)
- Modify: `source/library/Pawl/Event.hs` (`matchesTrigger` arm + the `stateTriggers` `live` arm), `source/library/Pawl/Codec.hs` (both TriggerCondition functions), `source/library/Pawl/Monarch.hs` (`crownSteal`, the `inherentMatch` steal arm, add to `monarchAbilities`)
- Test: `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/ExpirySpec.hs`

**Interfaces:**
- Consumes: `DamageEvent`, `Projection.isCreatureOf`, `Binding.setTriggerSource`, `Effect.BecomeMonarch ControllerOfSource`.
- Produces: `TriggerCondition.CreatureDealtCombatDamageToMonarch`; `Monarch.crownSteal`.

- [ ] **Step 1: Write the failing codec round-trip and steal tests**

In `source/test-suite/Pawl/CodecSpec.hs`:
```haskell
          HU.testCase "CreatureDealtCombatDamageToMonarch" $
            roundTrip "cd" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.CreatureDealtCombatDamageToMonarch,
```
In `source/test-suite/Pawl/ExpirySpec.hs` (falsifier: B's creature deals combat damage to monarch A ⇒ **B** — the creature's controller, not the ability's controller A — becomes the monarch):
```haskell
      HU.testCase "CR 725.2 combat damage to the monarch hands the crown to the damager's controller" $
        let (bobCreature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False DamageKind.Combat
            began = S.withEvent (GameEvent.DamageDealt dmg) gs0
            after = resolveAll (settle began)
         in HU.assertEqual "bob took the crown" (Just S.bob) (GameState.monarch after),
      HU.testCase "CR 725.2 noncombat damage to the monarch does not hand over the crown" $
        let (bobCreature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False DamageKind.Noncombat
            began = S.withEvent (GameEvent.DamageDealt dmg) gs0
            after = resolveAll (settle began)
         in HU.assertEqual "alice keeps the crown" (Just S.alice) (GameState.monarch after),
```

- [ ] **Step 2: Run to verify they fail**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `CreatureDealtCombatDamageToMonarch` not in scope.

- [ ] **Step 3: Add the trigger condition and its Event/Codec arms**

In `source/library/Pawl/Type/TriggerCondition.hs`, add after `SelfDealsCombatDamageToPlayer`:
```haskell
  | -- CR 725.2: a creature dealt combat damage to the monarch. NOT bearer-scoped
    -- (any creature); matched only via Pawl.Monarch.inherentMatch, never through a
    -- card's bearer. Rides P4's DamageDealt history.
    CreatureDealtCombatDamageToMonarch
```
In `source/library/Pawl/Event.hs`: add the compiler-forced arm to `matchesTrigger`'s top-level `case cond` returning `False` for all events (a card-bearer never carries this condition; it fires only via the inherent path) — a one-line `TriggerCondition.CreatureDealtCombatDamageToMonarch -> False`, with a comment citing that the real match is `Pawl.Monarch.inherentMatch`; add the same to the `live` helper inside `stateTriggers` if it cases exhaustively on `TriggerCondition` (per the P10 note that adding a `TriggerCondition` constructor forces exactly `matchesTrigger`, `stateTriggers`' `live`, and the two `Codec` functions).
In `source/library/Pawl/Codec.hs`: `triggerConditionToJson` → `TriggerCondition.CreatureDealtCombatDamageToMonarch -> nullary (Text.pack "CreatureDealtCombatDamageToMonarch")`; `jsonToTriggerCondition` → `("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch`.

- [ ] **Step 4: Add the steal ability and its match arm in `Pawl.Monarch`**

In `source/library/Pawl/Monarch.hs`, add imports `Pawl.Type.DamageEvent`, `Pawl.Type.DamageKind`, `Pawl.Type.Recipient`, `Pawl.Type.MonarchTarget`, and:
```haskell
-- CR 725.2: "Whenever a creature deals combat damage to the monarch, its
-- controller becomes the monarch." Controlled by the current monarch; makes a
-- DIFFERENT player (the damager's controller) the monarch.
crownSteal :: TriggeredAbility Card
crownSteal =
  oneEffect
    TriggerCondition.CreatureDealtCombatDamageToMonarch
    (Effect.BecomeMonarch MonarchTarget.ControllerOfSource)
```
Change `monarchAbilities = [endStepDraw]` to `monarchAbilities = [endStepDraw, crownSteal]`. Extend `inherentMatch`'s `case` with the steal arm (binds the damaging creature under the reserved trigger-source slot so `ControllerOfSource` reads it):
```haskell
  (TriggerCondition.CreatureDealtCombatDamageToMonarch, GameEvent.DamageDealt ev)
    | DamageEvent.kind ev == DamageKind.Combat
        && DamageEvent.target ev == Recipient.ToPlayer monarch
        && Projection.isCreatureOf (DamageEvent.source ev) gs ->
        Just (Binding.setTriggerSource (DamageEvent.source ev) Map.empty)
```

- [ ] **Step 5: Run to verify pass; whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS (round-trip; the crown transfers on combat damage to the damager's controller; noncombat damage does not).

- [ ] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): the monarch combat-damage steal trigger

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 7: Palace Jailer — exile-until-monarch and the gate card — closes GAP-S

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs` (+`ExileUntilMonarch`), `source/library/Pawl/Type/GameState.hs` (+`exiledUntilMonarch`)
- Modify: `source/library/Pawl/Event.hs` (`changeZoneReturning`), `source/library/Pawl/Monarch.hs` (`returnExiledForMonarch`), `source/library/Pawl/Engine.hs` (wire the sweep), `source/library/Pawl/Resolve.hs` (classifier arms + `ExileUntilMonarch` executor), `source/library/Pawl/Codec.hs` (Effect arms), `source/library/Pawl/Setup.hs`, `source/test-suite/Pawl/Support.hs` (`exiledUntilMonarch` field)
- Create: `data/cards/palace-jailer.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (Palace Jailer ×3)
- Test: `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/ExpirySpec.hs`

**Interfaces:**
- Consumes: `Effect.BecomeMonarch TheController` (ETB #1), `GameState.monarch`, `Event.changeZone`.
- Produces: `Effect.ExileUntilMonarch SlotName`; `GameState.exiledUntilMonarch :: Map ObjectId PlayerId`; `Event.changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)`; `Monarch.returnExiledForMonarch :: Game Bool`; `Cards.palaceJailerPrinting`.

- [ ] **Step 1: Expose the new incarnation id from the zone-change funnel**

In `source/library/Pawl/Event.hs`, refactor `changeZone` so its body becomes `changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)` returning `Just newId` on a completed move and `Nothing` when the move is cancelled (the existing `resolved == Nothing` branch), and define `changeZone oid dest = Monad.void (changeZoneReturning oid dest)`. This keeps all ~30 existing `changeZone` callers unchanged (no `-Wunused-do-bind` fallout). Concretely: rename the current function, change its two `pure ()`/end points to `pure Nothing` / `pure (Just newId)`, and add the two-line `changeZone` wrapper.

- [ ] **Step 2: Write the failing `ExileUntilMonarch` codec round-trip**

In `source/test-suite/Pawl/CodecSpec.hs`:
```haskell
          HU.testCase "ExileUntilMonarch" $
            roundTrip "eum" Codec.effectToJson Codec.jsonToEffect (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target"))),
```

- [ ] **Step 3: Add the opcode, the GameState carrier, and seeds**

In `source/library/Pawl/Type/Effect.hs`:
```haskell
  | -- CR 725 (Palace Jailer): exile the slot's target UNTIL an opponent of the
    -- effect's controller becomes the monarch. The exile is the usual targeted
    -- move; the DURATION is the novelty -- the exiled incarnation is registered in
    -- GameState.exiledUntilMonarch and returned by Pawl.Monarch's settle-loop
    -- sweep. NOT MoveToZone: that has no duration and schedules no return.
    ExileUntilMonarch SlotName
```
In `source/library/Pawl/Type/GameState.hs`, add after `monarch`:
```haskell
    monarch :: Maybe PlayerId,
    -- CR 725 (Palace Jailer): objects exiled "until an opponent becomes the
    -- monarch", keyed by the exiled incarnation id to the effect's controller
    -- (whose opponent taking the crown ends the exile). Not an Expiry: the Expiry
    -- sweeps are delete-and-recompute and cannot perform the return zone change.
    exiledUntilMonarch :: Map ObjectId PlayerId
```
Seed `GameState.exiledUntilMonarch = Map.empty` in `Setup.hs` and in `Support.hs`'s `MkGameState`. (`GameState.hs` already imports `Map` and `ObjectId`.)

- [ ] **Step 4: Add the Effect classifier arms and the executor**

- `slotsOf`: `Effect.ExileUntilMonarch slot -> Set.singleton slot` (mirror the `Destroy`/`MoveToZone` targeting arm)
- `readsX`: `Effect.ExileUntilMonarch {} -> False`
- `manaProduced`: `Effect.ExileUntilMonarch {} -> Nothing`
- `searchesLibrary`: `Effect.ExileUntilMonarch {} -> False`
- `rewriteEffect`: `Effect.ExileUntilMonarch {} -> effect`
- `applyEffect` (mirror `MoveToZone`, but capture the exiled id and register it):

```haskell
  Effect.ExileUntilMonarch slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        Just target -> do
          -- CR 400.7: exile the target through the funnel; register the resulting
          -- incarnation for return when an opponent of `controller` (CR 102.2)
          -- becomes the monarch.
          mNew <- Event.changeZoneReturning target Zone.Exile
          case mNew of
            Nothing -> pure ()
            Just newId ->
              State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.insert newId controller (GameState.exiledUntilMonarch g)})
      _ -> pure ()
```

- [ ] **Step 5: Add the effect codec arms**

`effectToJson`: `Effect.ExileUntilMonarch s -> Json.tagged (Text.pack "ExileUntilMonarch") (Just (slotNameToJson s))`. `jsonToEffect`: `"ExileUntilMonarch" -> withValue mv (fmap Effect.ExileUntilMonarch . jsonToSlotName)`.

- [ ] **Step 6: Add the return sweep and wire it into the settle loop**

In `source/library/Pawl/Monarch.hs` add (imports `Pawl.Event`, `Pawl.Type.Zone`, `Control.Monad`):
```haskell
-- CR 725 (Palace Jailer): return every "exiled until an opponent becomes the
-- monarch" object once an opponent of its controller is the monarch. Two-player
-- (CR 102.2): an opponent is any player other than the controller, so an entry
-- is due iff its controller is not the current monarch. Runs in the settle loop.
returnExiledForMonarch :: Game Bool
returnExiledForMonarch = do
  gs <- State.get
  case GameState.monarch gs of
    Nothing -> pure False
    Just m ->
      let due = Map.keys (Map.filter (/= m) (GameState.exiledUntilMonarch gs))
       in if null due
            then pure False
            else do
              Monad.forM_ due $ \oid -> do
                _ <- Event.changeZoneReturning oid Zone.Battlefield
                State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g)})
              pure True
```
In `source/library/Pawl/Engine.hs`, `settleForPriority`, add the sweep and fold it into the re-loop condition:
```haskell
settleForPriority = do
  swept <- Expiry.sweepConditional
  returned <- Monarch.returnExiledForMonarch
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  Monad.when (swept || returned || acted || placed) settleForPriority
```

- [ ] **Step 7: Create the Palace Jailer card JSON**

Create `data/cards/palace-jailer.json` (re-read the printed text from Scryfall; keys alphabetical to match the emitted order). Two `SelfEnters` triggered abilities: (1) `BecomeMonarch TheController`; (2) `ExileUntilMonarch` on slot `target`, filtered to a creature an opponent controls:

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    { "type": "Generic", "value": 2 },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "White" } } },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "White" } } }
  ],
  "name": "Palace Jailer",
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
      "condition": { "type": "SelfEnters" },
      "modal": {
        "modes": [
          { "effects": [{ "type": "BecomeMonarch", "value": { "type": "TheController" } }], "targetSpecs": [] }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    },
    {
      "condition": { "type": "SelfEnters" },
      "modal": {
        "modes": [
          {
            "effects": [{ "type": "ExileUntilMonarch", "value": "target" }],
            "targetSpecs": [
              {
                "slot": "target",
                "spec": {
                  "pool": { "type": "Creatures" },
                  "filter": { "type": "ControlledBy", "value": { "type": "Opponent" } },
                  "exclusion": { "type": "IncludesSource" }
                }
              }
            ]
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "typeLine": {
    "subtypes": [{ "type": "Human" }, { "type": "Soldier" }],
    "supertypes": [],
    "types": [{ "type": "Creature" }]
  }
}
```

(Verify `Subtype` has `Human` and `Soldier`; if either is missing, add it to `Pawl.Type.Subtype` and both `Codec` subtype tables, as P10 did for `Phyrexian`/`Elf`. Verify the `targetSpecs` array-vs-object shape against a loaded card such as Master Thief — copy its exact structure.)

- [ ] **Step 8: Register Palace Jailer in `Cards.hs` (three edits)**

Add `palaceJailerPrinting :: Printing.Printing,` to `MkCards`; `palaceJailerPrinting_ <- loadPrinting "palace-jailer"` + `palaceJailerPrinting = palaceJailerPrinting_,` in `loadCards`; `palaceJailerPrinting cards,` in `allPrintings`.

- [ ] **Step 9: Write the failing Palace Jailer characteristics test**

In `source/test-suite/Pawl/CardSpec.hs` (Master Thief pattern):
```haskell
        , HU.testCase "Palace Jailer is a {2}{W}{W} 2/2 Human Soldier with two ETB triggers" $ do
            HU.assertEqual "name" (Text.pack "Palace Jailer") (Card.Type.name (card (Cards.palaceJailerPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.palaceJailerPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card (Cards.palaceJailerPrinting cards)))
            HU.assertEqual "two triggered abilities" 2 (length (Card.Type.triggeredAbilities (card (Cards.palaceJailerPrinting cards))))
```

- [ ] **Step 10: Write the failing Palace Jailer gameplay tests**

In `source/test-suite/Pawl/ExpirySpec.hs`, Master-Thief-style (fabricate the entry event, `settle` then `resolveAll`). The falsifiers: exile persists across a turn boundary while the caster stays monarch, and returns exactly when an opponent takes the crown.

```haskell
      HU.testCase "CR 725 Palace Jailer: ETB makes the caster monarch and exiles an opponent's creature until an opponent takes the crown" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (victim, gs1) = S.addCreature (Cards.pikerPrinting cards) S.bob gs0
            (jailer, gs2) = S.addCreature (Cards.palaceJailerPrinting cards) S.alice gs1
            entered = ZoneChange.MkZoneChange jailer Zone.Stack Zone.Battlefield
            gs3 = S.withEvent (GameEvent.Moved entered (Projection.project jailer gs2)) gs2
            afterEtb = resolveAll (settle gs3)
            -- caster stays monarch across a turn boundary: exile holds.
            heldExiled = S.settleSba afterEtb
            -- an opponent (bob) takes the crown: the creature returns.
            afterSteal = resolveAll (settle (S.withMonarch S.bob heldExiled))
         in do
              HU.assertEqual "alice is monarch on ETB" (Just S.alice) (GameState.monarch afterEtb)
              HU.assertEqual "victim is exiled" 0 (length (filter (== victim) (Set.toList (GameState.battlefield afterEtb))))
              HU.assertBool "victim registered for return" (not (Map.null (GameState.exiledUntilMonarch afterEtb)))
              HU.assertBool "still exiled while alice stays monarch" (not (Map.null (GameState.exiledUntilMonarch heldExiled)))
              HU.assertEqual "a creature is back on the battlefield once bob is monarch" 1 (length (Game.zoneMembers Zone.Battlefield S.bob afterSteal))
              HU.assertEqual "return cleared the exile register" True (Map.null (GameState.exiledUntilMonarch afterSteal)),
```

(If the two ETB triggers require an ordering prompt that `S.identityAnswer` does not answer, use the answerer `ExpirySpec` already uses for its multi-trigger cases, or verify empirically in Step 11 and adjust the *answerer*, never the assertions. `victim` is exiled, so it is a fresh incarnation on return — assert on `zoneMembers Battlefield S.bob`, not the old `victim` id.)

- [ ] **Step 11: Run to verify pass; whole suite green**

Run: `cabal build all --enable-tests && cabal test 2>&1 | tail -25`
Expected: PASS — characteristics, and the full ETB → exile → persist → return cycle. **GAP-S (monarch) closed.** If the return case fails because the sweep did not run, confirm `returnExiledForMonarch` is wired into `settleForPriority` and that `settle` reaches it; do not weaken the assertion.

- [ ] **Step 12: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p11): exile-until-monarch and Palace Jailer (GAP-S monarch)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

### Task 8: Deferral issues, in-code citations, and milestone bookkeeping

**Files:**
- Modify: any in-code `#TBD-*` markers left in Tasks 2–7 (replace with real `(#N)`)
- Modify: `docs/progress.md`, `CLAUDE.md`

**Interfaces:** none (bookkeeping).

- [ ] **Step 1: File the deferral issues on `tfausak/pawl`**

`gh issue create` for each, carrying status, rationale, and expiry label:
- **Synthetic emblem source** (`S.anthemEmblemCard`) — `expires:card-driven`; retired when planeswalkers or the Ring land and a real card can mint an emblem. Cite `Support.hs`'s fixture site.
- **GameState / Object / Source serialization** — `gap`; no codec exists for these today (P10 hit the same wall with `Player.counters`; the user asked for this to be logged). Fires when the engine needs to persist/replay full game state. Cite the spec §2.10 over-claim.
- **Command-zone casting** (Commander tax, CR 903.8) — `gap`, `expires:card-driven`; no gate card.
- **CR 725.4 monarch reassignment on a player leaving the game** — `rules-correctness`, `expires:card-driven`; fires only when a player leaves (multiplayer). Note `returnExiledForMonarch`/the steal use the project-wide two-player `Opponent` assumption (CR 102.2). Related to #87.
- **CR 725.5** (a static effect keyed to "the monarch" with no monarch present) — `rules-correctness`, `expires:card-driven`; no gate card exercises it.
- **Day/night, Ring/Ring-bearer, initiative, venture, speed, experience/rad counters** (GAP-S backlog) — one issue or a short cluster, `gap`; ride P10's substrate + P4's triggers; census §3.4.
- **Other command-zone residents** (dungeon/plane/scheme/vanguard/conspiracy CR 309–315; the format variants CR 408.3) — `gap`; each its own subsystem/format.

- [ ] **Step 2: Replace the `#TBD-*` markers with the real issue numbers**

Replace `(#TBD-emblem-source)` in `source/test-suite/Pawl/Support.hs` (the `anthemEmblemCard` fixture) with the synthetic-emblem-source issue's `(#N)`, and any other `#TBD-*` markers you left while implementing. State only what is *not* implemented plus `(#N)`; never write the expiry into the comment (CLAUDE.md).

Run: `grep -rn "#TBD" source/ data/ ; echo "exit: $?"`
Expected: no matches (grep exit 1).

- [ ] **Step 3: Add the P11 completion entry to `docs/progress.md`**

One distilled entry: gate cards (Palace Jailer; the labeled synthetic emblem source), the decisions proved (a seventh zone whose statics function off the battlefield; the monarch as a game-wide designation with two sourceless inherent triggers; exile-until-designation as a dedicated carrier because the Expiry sweeps are delete-only), and the types/opcodes added (`Zone.Command`; `GameState.command`/`monarch`/`exiledUntilMonarch`; `Source.OfEmblem`/`OfInherentTrigger`; `Effect.CreateEmblem`/`BecomeMonarch`/`ExileUntilMonarch`; `MonarchTarget`; `GameEvent.BecameMonarch`; `TriggerCondition.CreatureDealtCombatDamageToMonarch`; `Pawl.Monarch`; the projection command-zone gather + affected-set perspective). Match the existing entries' format.

- [ ] **Step 4: Replace (do not append) the status bullet in `CLAUDE.md`**

Update the "Current work and tracking" status bullet so it records P11 as closed (GAP-Z command zone + emblems; the monarch customer of GAP-S) and states **M4.5 is complete** — every closed-half axis the census flagged now has a type-system axis and a real (or sanctioned labeled-synthetic) gate card with a passing gameplay test. Remove "P11 is the one remaining M4.5 phase." Close the umbrella issue: `gh issue close 7 --repo tfausak/pawl --comment "Landed by M4.5 P11; M4.5 complete."`.

- [ ] **Step 5: Final full verification**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -15`
Expected: warning-clean build, whole suite green.

- [ ] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "$(cat <<'EOF'
docs(m4.5-p11): completion note, deferral issues, CLAUDE.md status

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MNCaryS2oscsB7mTSdiho4
EOF
)"
```

---

## Exit criterion (spec §10)

(a) `Zone.Command` exists with an emblem resident whose **static ability functions from the command zone** (the projection gather pass), proven by the synthetic-source test surviving a battlefield wipe and being controller-scoped. (b) The **monarch** exists as a `GameState` designation with its two **sourceless** inherent triggers and Palace Jailer as the real gate — the crown transfers on combat damage to the damager's controller, the monarch draws on their own end step, and the exile returns exactly when an opponent takes the crown. Build warning-clean, `hooky run` passes, every rules claim cited against `docs/rules.txt`. **M4.5 is complete.**

## Progress check

`grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-p11-command-zone-emblems-monarch.md` must reach `0`.

## Deviations from the spec (verified against the codebase, applied in this plan)

1. **No codecs for `Source`, `GameState`, `Object` (spec §2.10).** None of these are serialized today (`Pawl.Codec` covers the `Card` closure plus `GameEvent`/`DamageEvent`). So `Source.OfEmblem`/`OfInherentTrigger` and the three new `GameState` fields need no codec arms. `MonarchTarget` still gets one (embedded in `Effect.BecomeMonarch`). A standalone "GameState/Object/Source serialization" deferral issue is filed (Task 8), per the user's request to log this recurring gap.
2. **Palace Jailer's exile-return is a dedicated carrier + settle-loop sweep, not an `Expiry` (spec §1/§2.9/§6).** The `Expiry` sweeps are **delete-and-recompute only** — they drop a stored effect so the next projection reverts it, and none performs a zone change. A physically-exiled permanent cannot be recomputed back onto the battlefield. So the plan adds `GameState.exiledUntilMonarch` + `Effect.ExileUntilMonarch` + `Monarch.returnExiledForMonarch` (wired into `settleForPriority`) and adds **no** `Expiry` constructor and **no** `StateCondition`. Observable behavior matches the spec ("the exile lasts until an opponent becomes the monarch"); the mechanism differs because the spec's stated one is not buildable.
3. **`changeZone` is refactored to expose the new incarnation id.** It returns `()` today; capturing the exiled incarnation for the return register needs `changeZoneReturning :: … -> Game (Maybe ObjectId)`, with `changeZone` as a `Monad.void` wrapper so existing callers are untouched.
4. **`Projection.affects` gains a source's-controller perspective.** The emblem anthem "creatures **you** control get +1/+1" is the first affected-set filter to reference a player; `affects` hard-coded the perspective to `Nothing` (documented as valid only until such a filter existed). Supplying `controllerOf source`/`controllerOf oid` is safe (no existing affected-set filter reads `ControlledBy`). This partially retires the shortcut #34/#35 tracks (the affected-set half only; the player-scoped-`Count` half is untouched).
5. **`Ending EndStep`, not `Ending End` (spec §2.7).** The `Phase`/`EndingStep` constructors are `Phase.Ending EndingStep.EndStep`.
6. **No new `Pawl.*Spec` module.** Palace Jailer characteristics land in `CardSpec.hs`; all emblem/monarch gameplay lands in `ExpirySpec.hs` and `ProjectionSpec.hs`, matching how P10 avoided a new suite module.
