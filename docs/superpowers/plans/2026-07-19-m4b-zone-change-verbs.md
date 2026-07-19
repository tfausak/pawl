# M4b Zone-Change Verbs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the targeted zone-change opcode family (Destroy, MoveToZone, Draw, Mill, Discard) plus the Indestructible keyword, proving M3f's `Event.changeZone` funnel generalizes into card-driven verbs and that *destroy ≠ move-to-graveyard*.

**Architecture:** Five new first-order `Effect` constructors, each executed only by `Pawl.Resolve.applyEffect` through the existing `Event.changeZone` funnel. `Destroy` and the two lethal-damage state-based actions read a new `Keyword.Indestructible` through the projection. Each verb ships with a real, Scryfall-verified card (a JSON file under `data/cards/`) and a gameplay-level test that casts or resolves it through the stack.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns` already in use), `tasty` (`tasty-hunit` + `tasty-quickcheck`), the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

## Global Constraints

Copied verbatim from the spec (`docs/superpowers/specs/2026-07-19-m4b-zone-change-verbs-design.md`) and `CLAUDE.md`. Every task's requirements implicitly include these:

- **`cabal build all --enable-tests --enable-benchmarks` must be warning-clean** (`-Weverything` minus the allow-list; the `+pedantic` flag makes any warning a build failure). When in doubt run `cabal clean` first — incremental builds hide warnings from unchanged modules.
- **`Pawl.Resolve` is the sole home of `case effect of`**; `Pawl.Event` the sole home of `case` on `ReplacementEffect`/`TriggerCondition`; `Pawl.Codec` may case on an effect's identity (open-half serialization). No other closed-half module names a card or cases on an effect's identity.
- **`Keyword.Indestructible` is CR 702.12, a citation** — casing on it (read through `Projection.hasKeyword`/`PC.keywords`, never `Card.keywords`) is legitimate, the M2a rule.
- **Every card is a `data/cards/<slug>.json` file** rendered by the codec; the committed file must equal `Json.render (Codec.printingToJson p) <> "\n"` (the `CardsSpec` P3 byte-stability check). Never hand-edit `Cards.allPrintings` field ordering carelessly — the honesty round-trip (`CodecSpec` P1/P2) iterates it.
- **No language extensions** beyond those already present; **no partial functions** (`head`/`error`/`undefined`/non-exhaustive matches); **`Text` not `String`**; **arbitrary-precision `Natural`/`Integer`**; **`Mk`-prefixed non-punning constructors**; **qualified imports aliased to the last component**; **no explicit export lists**; **derive `Eq`/`Show`** (and `Ord` where the type is a map key).
- **Every rules claim is checked against `docs/rules.txt`** and cited by number in a code comment.
- **TDD is mandatory:** write each failing test and run it to watch it fail (a compile failure on a not-yet-added constructor counts as red) before implementing. Tick each `- [ ]` as you finish it.
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-clean, `cabal test`, then `git add -A && hooky fix && git add -A && hooky run` before committing.

---

## File Structure

Modules touched across the plan (no new library modules, so `pawl.cabal` needs no edit; no new test-spec module, so the test-suite `other-modules` needs no edit):

**Library (`source/library/`):**
- `Pawl/Type/Effect.hs` — +5 constructors (`Destroy`, `MoveToZone`, `Draw`, `Mill`, `Discard`).
- `Pawl/Type/Keyword.hs` — +`Indestructible` (702.12).
- `Pawl/Type/TargetSpec.hs` — +`CreatureOrEnchantmentTarget`.
- `Pawl/Type/Subtype.hs` — +`Myr`.
- `Pawl/Resolve.hs` — new arms in `slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, and `applyEffect`.
- `Pawl/Target.hs` — `legalRecipients` arm for `CreatureOrEnchantmentTarget`.
- `Pawl/Sba.hs` — `creatureDies` indestructible guard (704.5g/704.5h, not 704.5f).
- `Pawl/Event.hs` — new shared `drawCard` primitive.
- `Pawl/Engine.hs` — `drawFor` delegates to `Event.drawCard`.
- `Pawl/Setup.hs` — `drawCard` delegates to `Event.drawCard`.
- `Pawl/Codec.hs` — arms for the 5 `Effect` constructors, `Indestructible`, `CreatureOrEnchantmentTarget`, `Myr`.

**Card data (`data/cards/`):** `darksteel-myr.json`, `murder.json`, `unsummon.json`, `angelic-edict.json`, `divination.json`, `tome-scour.json`, `mind-rot.json`.

**Test suite (`source/test-suite/Pawl/`):**
- `Cards.hs` — 7 accessors, 7 `loadPrinting` lines, 7 `allPrintings` entries, deck edits (Task 9).
- `Support.hs` — `blueBlack` matchup and the `matchups` list (Task 9).
- `ResolveSpec.hs` — new `indestructibleTests` and `zoneChangeTests` groups.
- `CardSpec.hs` — new `m4bCardTests` group and the `allPrintings` count assertion (31 → 38, bumped per card task).

---

## Task 1: Indestructible keyword + the state-based-action guard + Darksteel Myr

Delivers the keyword and its SBA readers (CR 704.5g/704.5h guarded, 704.5f not), gated by Darksteel Myr in combat/deathtouch/zero-toughness scenarios. The Destroy-opcode reader lands in Task 2.

**Files:**
- Modify: `source/library/Pawl/Type/Keyword.hs`
- Modify: `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Mana.hs:40-56` (`subtypeMana` — new `Myr -> Nothing` arm)
- Modify: `source/library/Pawl/Sba.hs:66-83` (`creatureDies`)
- Modify: `source/library/Pawl/Codec.hs` (`keywordToJson`/`jsonToKeyword`, `subtypeToJson`/`jsonToSubtype`)
- Create: `data/cards/darksteel-myr.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Modify: `source/test-suite/Pawl/CardSpec.hs` (count 31 → 32, new card test)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (new `indestructibleTests` group)

**Interfaces:**
- Produces: `Keyword.Indestructible`; `Subtype.Myr`; `Cards.darksteelMyrPrinting :: Cards.Cards -> Printing.Printing`; `Sba.creatureDies` now guards 704.5g/704.5h with indestructibility.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ResolveSpec.hs` a new group and wire it into `tests`. `withEffect` (already defined in this module) applies a test-local continuous effect for the 704.5f case; `DamageEvent`/`Recipient`/`Modification`/`Quantity`/`Keyword` are already imported.

```haskell
indestructibleTests :: Cards.Cards -> Tasty.TestTree
indestructibleTests cards =
  Tasty.testGroup
    "Indestructible"
    [ HU.testCase "CR 704.5g an indestructible creature survives lethal marked damage" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- Myr is 0/1; 3 marked damage is lethal (704.5g) but indestructible saves it.
            after = Sba.checkStateBasedActions (S.markDamage myrId 3 gs)
         in do
              HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Myr not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 704.5h an indestructible creature survives deathtouch" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- Zero marked damage (so 704.5g is silent) plus a deathtouch event isolates
            -- the 704.5h path; indestructible must guard it too (CR 700.4).
            wounded = gs {GameState.damageEvents = [DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True]}
            after = Sba.checkStateBasedActions wounded
         in HU.assertEqual "Myr survives deathtouch" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- A test-local -0/-1 drops Myr (0/1) to 0/0; 704.5f is a put-into-graveyard,
            -- not a destroy, so indestructible does not apply (Myr's own reminder text).
            zeroed = withEffect myrId (Timestamp.MkTimestamp 5) (Modification.ModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal (-1))) gs
            after = Sba.checkStateBasedActions zeroed
         in do
              HU.assertEqual "Myr left the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Myr in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
    ]
```

Wire it in: change the final `tests` to include it.

```haskell
tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Resolve" [targetTests cards, resolveTests cards, fizzleTests cards, indestructibleTests cards]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -30`
Expected: compile failure — `Cards.darksteelMyrPrinting` and `Keyword`/`Subtype` references not in scope (red).

- [x] **Step 3: Add the keyword and subtype constructors**

In `source/library/Pawl/Type/Keyword.hs`, insert `Indestructible -- 702.12` in rule-number order (between `Haste -- 702.10` and `Reach -- 702.17`):

```haskell
  | Haste -- 702.10
  | Indestructible -- 702.12
  | Reach -- 702.17
```

In `source/library/Pawl/Type/Subtype.hs`, append `Myr` after `Elephant` (a creature type; the first Myr).

```haskell
  | Elephant
  | Myr
```

`Mana.subtypeMana` (`source/library/Pawl/Mana.hs:40-56`) is an exhaustive `case` on `Subtype` with no wildcard, so add the arm (a Myr is not a land subtype, so it produces no mana):

```haskell
  Subtype.Myr -> Nothing
```

(No other module cases exhaustively on `Subtype` — verified: only `Mana.subtypeMana` and `Codec`.)

- [x] **Step 4: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add `Indestructible` to `keywordToJson` (case arm `Keyword.Indestructible -> "Indestructible"`) and to the `jsonToKeyword` table (`(Text.pack "Indestructible", Keyword.Indestructible)`), keeping rule-number order. Add `Myr` to `subtypeToJson` (`Subtype.Myr -> "Myr"`) and to the `jsonToSubtype` table (`(Text.pack "Myr", Subtype.Myr)`).

- [x] **Step 5: Add the indestructible guard to the SBA**

In `source/library/Pawl/Sba.hs`, `creatureDies` (lines ~66-83): read indestructibility off the already-projected characteristics and guard 704.5g and 704.5h, leaving 704.5f ungated.

```haskell
creatureDies :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
creatureDies gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
      -- CR 700.4 / 702.12b: indestructible stops "destroy" and lethal-damage/
      -- deathtouch state-based actions (704.5g/704.5h) but NOT 704.5f (toughness
      -- 0 or less is a put-into-graveyard, not a destruction). Read off the
      -- already-projected keywords, so Humility (layer 6) strips it.
      indestructible = Set.member Keyword.Indestructible (PC.keywords pc)
   in isCreature && case PC.toughness pc of
        Nothing -> False
        Just toughness ->
          -- CR 704.5f: toughness 0 or less. Ungated by indestructible.
          (toughness <= 0)
            -- CR 704.5g: lethal marked damage. Guarded.
            || ( not indestructible
                   && ( case Game.lookupObject oid gs of
                          Nothing -> False
                          Just obj -> toInteger (Object.damage obj) >= toughness
                      )
               )
            -- CR 704.5h: wounded by a deathtouch source. Guarded.
            || (not indestructible && woundedByDeathtouch gs oid)
```

Add `import qualified Pawl.Type.Keyword as Keyword` to `Pawl.Sba` (alphabetical within its import group).

- [x] **Step 6: Create the Darksteel Myr card file**

Create `data/cards/darksteel-myr.json` with exactly this content (single line, then one trailing newline — the `CardsSpec` P3 byte-stability check enforces `render(card) <> "\n"`; `types` is `[Creature, Artifact]` because the render sorts by the `CardType` `Ord`, where `Creature` precedes `Artifact`):

```json
{"name":"Darksteel Myr","manaCost":[{"type":"Generic","value":3}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"},{"type":"Artifact"}],"subtypes":[{"type":"Myr"}]},"power":{"type":"Literal","value":0},"toughness":{"type":"Literal","value":1},"keywords":[{"type":"Indestructible"}],"staticAbilities":[],"effects":[],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[]}
```

(If `CardsSpec` P3 later reports a byte mismatch, regenerate the exact bytes: `cabal repl pawl:test:test`, then `Data.Text.IO.putStr (Pawl.Json.render (Pawl.Codec.cardToJson <the card>) <> Data.Text.pack "\n")` and copy the output. The content above is computed to match.)

- [x] **Step 7: Register the card**

In `source/test-suite/Pawl/Cards.hs`: add the field `darksteelMyrPrinting :: Printing.Printing` to `MkCards`; add `darksteelMyrPrinting_ <- loadPrinting "darksteel-myr"` in `loadCards`; add `darksteelMyrPrinting = darksteelMyrPrinting_` to the returned record; add `darksteelMyrPrinting cards,` to `allPrintings`.

- [x] **Step 8: Add the card-data test and bump the registry count**

In `source/test-suite/Pawl/CardSpec.hs`, bump the registry-count assertion from 31 to 32:

```haskell
      HU.testCase "the registry holds every printing (32 at M4b Task 1)" $
        HU.assertEqual "count" 32 (length (Cards.allPrintings cards)),
```

Add an `m4bCardTests` group and wire it into `tests`:

```haskell
m4bCardTests :: Cards.Cards -> Tasty.TestTree
m4bCardTests cards =
  Tasty.testGroup
    "M4bCards"
    [ HU.testCase "Darksteel Myr is a {3} 0/1 Artifact Creature with indestructible" $
        let c = Printing.card (Cards.darksteelMyrPrinting cards)
         in do
              HU.assertEqual "name" (Text.pack "Darksteel Myr") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) (Card.Type.manaCost c)
              HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 0))) (Card.Type.power c)
              HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
              HU.assertEqual "keyword" (Set.singleton Keyword.Indestructible) (Card.Type.keywords c)
    ]
```

```haskell
tests cards =
  Tasty.testGroup
    "Card"
    [cardTests cards, lintTests cards, m2aCardTests cards, m2bCardTests cards, m2cCardTests cards, basicLandTests cards, m3cCardTests cards, m3eCardTests cards, m4bCardTests cards]
```

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (indestructible group green, card group green, count = 32, honesty round-trip green over 32 printings).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: Indestructible keyword + SBA guard (704.5g/h, not 704.5f), Darksteel Myr

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 2: Destroy opcode + Murder (the gate)

Delivers `Effect.Destroy` and its indestructible-aware executor, gated by Murder vs. a normal creature (→ graveyard) and vs. Darksteel Myr (unchanged — *destroy ≠ move-to-graveyard*).

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, `applyEffect`)
- Modify: `source/library/Pawl/Codec.hs` (`effectToJson`/`jsonToEffect`)
- Create: `data/cards/murder.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Modify: `source/test-suite/Pawl/CardSpec.hs` (count 32 → 33, Murder card test)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (new `zoneChangeTests` group)

**Interfaces:**
- Consumes: `Cards.darksteelMyrPrinting`, `Projection.hasKeyword`, `Event.changeZone` (all existing).
- Produces: `Effect.Destroy SlotName`; `Cards.murderPrinting :: Cards.Cards -> Printing.Printing`. `Resolve.slotsOf (Effect.Destroy s) == Set.singleton s`.

- [x] **Step 1: Write the failing tests**

Add a `zoneChangeTests` group to `source/test-suite/Pawl/ResolveSpec.hs`. It reuses `S.landsInPlay`, `S.addCreature`, `S.handOne`, `S.identityAnswer`, `Cast.castSpell`, `Stack.resolveTop`. `identityAnswer`'s `ChooseTargets` picks `Set.lookupMin`, which for a single-creature board is that creature.

```haskell
-- alice controls `n` Swamps and holds `printing` in a main phase with priority;
-- bob controls one `foe`. Returns (foe's id, post-cast-and-resolve state).
castBlackRemovalAt :: Cards.Cards -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
castBlackRemovalAt cards printing foe =
  let base = S.landsInPlay (Cards.swampPrinting cards) 3
      (foeId, withFoe) = S.addCreature foe S.bob base
      (gs, spellId) = S.handOne printing withFoe
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (foeId, resolved)

zoneChangeTests :: Cards.Cards -> Tasty.TestTree
zoneChangeTests cards =
  Tasty.testGroup
    "ZoneChange"
    [ HU.testCase "CR 701.7 Murder destroys a normal creature into its owner's graveyard" $
        let (_, after) = castBlackRemovalAt cards (Cards.murderPrinting cards) (Cards.pikerPrinting cards)
         in do
              HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 700.4 Murder does nothing to an indestructible creature (destroy /= move)" $
        let (_, after) = castBlackRemovalAt cards (Cards.murderPrinting cards) (Cards.darksteelMyrPrinting cards)
         in do
              -- The falsifier: modelling Destroy as MoveToZone slot Graveyard would
              -- bury the Myr. It stays; the spell still resolved and was buried.
              HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
              HU.assertEqual "bob's graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "Murder in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
    ]
```

Wire `zoneChangeTests cards` into `tests` alongside the others:

```haskell
tests cards = Tasty.testGroup "Resolve" [targetTests cards, resolveTests cards, fizzleTests cards, indestructibleTests cards, zoneChangeTests cards]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.murderPrinting` and `Effect.Destroy` not in scope (red).

- [x] **Step 3: Add the `Destroy` constructor**

In `source/library/Pawl/Type/Effect.hs`, add after `ControlPlayerNextTurn` (order does not matter for `data`, but keep the block tidy):

```haskell
  | -- CR 701.7 / 700.4: destroy the slot's target permanent -- move it to its
    -- owner's graveyard via the changeZone funnel UNLESS it is indestructible.
    -- NOT MoveToZone slot Graveyard: the indestructible check is why this is its
    -- own opcode (Murder vs Darksteel Myr). A future interceptable "destroy event"
    -- (regeneration, CR 615) is M4d.
    Destroy SlotName
```

- [x] **Step 4: Add the `Destroy` classification arms**

In `source/library/Pawl/Resolve.hs`, add an arm to each classification function. `slotsOf`: `Effect.Destroy slot -> Set.singleton slot`. `readsX`'s `effectReadsX`: `Effect.Destroy _ -> False`. `manaProduced`: `Effect.Destroy _ -> Nothing`. `searchesLibrary`: `Effect.Destroy _ -> False`. `rewriteEffect`: `Effect.Destroy _ -> effect` (no rewritable land-type word).

- [x] **Step 5: Add the `Destroy` executor arm**

In `source/library/Pawl/Resolve.hs`, add to `applyEffect` (the module already imports `Keyword`, `Projection`, `Event`, `Recipient`):

```haskell
  Effect.Destroy slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target ->
            -- CR 700.4: an indestructible permanent can't be destroyed -- the
            -- effect does nothing (no move). Read through the projection, so a
            -- Humility'd permanent (keywords stripped) can be destroyed.
            if Projection.hasKeyword Keyword.Indestructible target gs
              then gs
              else Event.changeZone target Zone.Graveyard gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
```

- [x] **Step 6: Add the `Destroy` codec arm**

In `source/library/Pawl/Codec.hs`, `effectToJson`: `Effect.Destroy s -> Json.tagged (Text.pack "Destroy") (Just (slotNameToJson s))`. In `jsonToEffect`: `"Destroy" -> withValue mv (fmap Effect.Destroy . jsonToSlotName)`.

- [x] **Step 7: Create and register Murder**

Create `data/cards/murder.json` (single line + trailing newline):

```json
{"name":"Murder","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Destroy","value":"target"}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}
```

In `Cards.hs`: add `murderPrinting :: Printing.Printing` to `MkCards`, `murderPrinting_ <- loadPrinting "murder"`, the record line, and the `allPrintings` entry.

- [x] **Step 8: Add the card-data test and bump the count**

Bump the `CardSpec.hs` count assertion to 33 (`"...(33 at M4b Task 2)"`, `33`). Add to `m4bCardTests`:

```haskell
      HU.testCase "Murder is a {1}{B}{B} Instant that destroys a target creature" $
        let c = Printing.card (Cards.murderPrinting cards)
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black, black])) (Card.Type.manaCost c)
              HU.assertBool "an instant" (Card.isInstant c)
              HU.assertEqual "effect destroys the target slot" [Effect.Destroy (SlotName.MkSlotName (Text.pack "target"))] (Card.Type.effects c)
              HU.assertEqual "one CreatureTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget) (Card.Type.targetSpecs c),
```

(`Effect`, `SlotName`, `TargetSpec`, `Color`, `ManaType` are already imported in `CardSpec.hs`.)

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (Destroy gate green; count 33; D4 lint green — Murder reads `{target}` and declares `{target}`; honesty round-trip green).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: Destroy opcode + Murder (the gate: destroy != move-to-graveyard)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 3: MoveToZone opcode + Unsummon (bounce)

Delivers `Effect.MoveToZone SlotName Zone` and its executor, gated by Unsummon returning a creature to its owner's hand — the generalization proof (a non-graveyard destination through a targeted opcode).

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs` (classification arms + `applyEffect`)
- Modify: `source/library/Pawl/Codec.hs`
- Create: `data/cards/unsummon.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs` (count → 34)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (`zoneChangeTests`)

**Interfaces:**
- Produces: `Effect.MoveToZone SlotName Zone`; `Cards.unsummonPrinting`. `Resolve.slotsOf (Effect.MoveToZone s _) == Set.singleton s`.

- [x] **Step 1: Write the failing test**

Add to `zoneChangeTests` in `ResolveSpec.hs`:

```haskell
      HU.testCase "CR 400.7 Unsummon returns a creature to its owner's hand" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, withPiker) = S.addPiker cards S.bob base
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "a card in bob's hand (its owner)" 1 (S.handSize S.bob after)
              HU.assertEqual "Unsummon in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.unsummonPrinting` / `Effect.MoveToZone` not in scope (red).

- [x] **Step 3: Add the `MoveToZone` constructor**

In `source/library/Pawl/Type/Effect.hs`:

```haskell
  | -- CR 400.7: move the slot's target object to a zone through the changeZone
    -- funnel. Bounce = MoveToZone slot Hand (owner-relative -- changeZone carries
    -- Object.owner); targeted exile = MoveToZone slot Exile. The destination is
    -- data; one opcode for every targeted single-object move. Distinct from
    -- Destroy (unconditional move, no indestructible check).
    MoveToZone SlotName Zone
```

Add `import Pawl.Type.Zone (Zone)` to `Pawl.Type.Effect` (alphabetical).

- [x] **Step 4: Add the classification arms**

In `Resolve.hs`: `slotsOf`: `Effect.MoveToZone slot _ -> Set.singleton slot`. `readsX`: `Effect.MoveToZone {} -> False`. `manaProduced`: `Effect.MoveToZone {} -> Nothing`. `searchesLibrary`: `Effect.MoveToZone {} -> False`. `rewriteEffect`: `Effect.MoveToZone {} -> effect`.

- [x] **Step 5: Add the executor arm**

In `applyEffect`:

```haskell
  Effect.MoveToZone slot zone ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative.
          Just target -> Event.changeZone target zone gs
        _ -> gs
```

- [x] **Step 6: Add the codec arm**

`effectToJson`: `Effect.MoveToZone s z -> Json.tagged (Text.pack "MoveToZone") (Just (Array [slotNameToJson s, zoneToJson z]))`. `jsonToEffect`:

```haskell
    "MoveToZone" -> case mv of
      Just (Array [s, z]) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z
      _ -> Left (Text.pack "MoveToZone expects [slot, zone]")
```

- [x] **Step 7: Create and register Unsummon**

Create `data/cards/unsummon.json`:

```json
{"name":"Unsummon","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"MoveToZone","value":["target",{"type":"Hand"}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}
```

Register in `Cards.hs` (`unsummonPrinting`, `loadPrinting "unsummon"`, record line, `allPrintings`).

- [x] **Step 8: Card-data test and count → 34**

Bump count to 34. Add to `m4bCardTests`:

```haskell
      HU.testCase "Unsummon is a {U} Instant that bounces a target creature to hand" $
        let c = Printing.card (Cards.unsummonPrinting cards)
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [blue])) (Card.Type.manaCost c)
              HU.assertEqual "effect returns to hand" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand] (Card.Type.effects c),
```

Add `import qualified Pawl.Type.Zone as Zone` to `CardSpec.hs` (alphabetical).

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (bounce green; count 34; round-trip green).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: MoveToZone opcode + Unsummon (bounce -- funnel generalizes past RiP)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 4: CreatureOrEnchantmentTarget + Angelic Edict (targeted exile)

Delivers the broadened target spec and Angelic Edict exiling a creature and (exercising the non-creature admission) an enchantment.

**Files:**
- Modify: `source/library/Pawl/Type/TargetSpec.hs`
- Modify: `source/library/Pawl/Target.hs` (`legalRecipients`)
- Modify: `source/library/Pawl/Codec.hs` (`targetSpecToJson`/`jsonToTargetSpec`)
- Create: `data/cards/angelic-edict.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs` (count → 35)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (`zoneChangeTests`)

**Interfaces:**
- Consumes: `Projection.cardTypesOf`, `CardType.Enchantment` (existing).
- Produces: `TargetSpec.CreatureOrEnchantmentTarget`; `Cards.angelicEdictPrinting`. `Target.legalRecipients CreatureOrEnchantmentTarget` returns `Recipient.ToObject` of battlefield creatures and enchantments.

- [x] **Step 1: Write the failing tests**

Add to `zoneChangeTests`. Angelic Edict is `{4}{W}` (five mana ≥1 white) — use five Plains; it is a Sorcery, so `handOne`'s main-phase/priority/empty-stack state satisfies sorcery speed.

```haskell
      HU.testCase "CR 701.10 Angelic Edict exiles a target creature" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 5
            (_, withPiker) = S.addPiker cards S.bob base
            (gs, spellId) = S.handOne (Cards.angelicEdictPrinting cards) withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 115 Angelic Edict may exile an enchantment (non-creature permanent)" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 5
            -- bob controls only Rest in Peace (an enchantment, not a creature), so
            -- it is the single legal CreatureOrEnchantmentTarget.
            (ripId, withRip) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.angelicEdictPrinting cards) withRip
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the enchantment left the battlefield" Nothing (Game.lookupObject ripId after)
              HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.angelicEdictPrinting` / `TargetSpec.CreatureOrEnchantmentTarget` not in scope (red).

- [x] **Step 3: Add the target spec**

In `source/library/Pawl/Type/TargetSpec.hs`:

```haskell
  | -- CR 115: "target creature or enchantment" (Angelic Edict). The first spec
    -- admitting a non-creature permanent -- named as ToObject, like LandTarget.
    CreatureOrEnchantmentTarget
```

- [x] **Step 4: Add the `legalRecipients` arm**

In `source/library/Pawl/Target.hs`, add a case arm (the module imports `Projection`, `CardType`, `GameState`, `Recipient`, `Set`):

```haskell
        TargetSpec.CreatureOrEnchantmentTarget ->
          let ok oid =
                let ts = Projection.cardTypesOf oid gs
                 in Set.member CardType.Creature ts || Set.member CardType.Enchantment ts
              matches = filter ok (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject matches)
```

- [x] **Step 5: Add the codec arms**

In `Codec.hs`, add `TargetSpec.CreatureOrEnchantmentTarget -> "CreatureOrEnchantmentTarget"` to `targetSpecToJson` and `(Text.pack "CreatureOrEnchantmentTarget", TargetSpec.CreatureOrEnchantmentTarget)` to the `jsonToTargetSpec` table.

- [x] **Step 6: Create and register Angelic Edict**

Create `data/cards/angelic-edict.json` (slug `angelic-edict`):

```json
{"name":"Angelic Edict","manaCost":[{"type":"Generic","value":4},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Sorcery"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"MoveToZone","value":["target",{"type":"Exile"}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureOrEnchantmentTarget"}}]}
```

Register in `Cards.hs` (`angelicEdictPrinting`, `loadPrinting "angelic-edict"`, record line, `allPrintings`).

- [x] **Step 7: Card-data test and count → 35**

Bump count to 35. Add to `m4bCardTests`:

```haskell
      HU.testCase "Angelic Edict is a {4}{W} Sorcery exiling a creature or enchantment" $
        let c = Printing.card (Cards.angelicEdictPrinting cards)
         in do
              HU.assertBool "not an instant" (not (Card.isInstant c))
              HU.assertEqual "effect exiles" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile] (Card.Type.effects c)
              HU.assertEqual "creature-or-enchantment slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureOrEnchantmentTarget) (Card.Type.targetSpecs c),
```

- [x] **Step 8: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (both exile tests green; count 35; round-trip green).

- [x] **Step 9: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: CreatureOrEnchantmentTarget + Angelic Edict (targeted exile)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 5: Consolidate the single-card draw primitive

Behavior-preserving refactor: extract one `Event.drawCard` and route `Engine.drawFor` and `Setup.drawCard` through it. Prerequisite for the `Draw` opcode (Task 6). No new card, no rules change.

**Files:**
- Modify: `source/library/Pawl/Event.hs` (new `drawCard`)
- Modify: `source/library/Pawl/Engine.hs:88-94` (`drawFor`)
- Modify: `source/library/Pawl/Setup.hs:118-123` (`drawCard`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (unit test for `Event.drawCard`)

**Interfaces:**
- Produces: `Event.drawCard :: PlayerId -> GameState -> GameState` — moves the top library card to hand (CR 121.2) or, on an empty library, records `drewFromEmpty` (CR 121.3 → 704.5b). `Engine.drawFor` and `Setup.drawCard` become thin wrappers.

- [x] **Step 1: Write the failing test**

Add to `ResolveSpec.hs` a small group (or fold into `zoneChangeTests`):

```haskell
drawCardTests :: Cards.Cards -> Tasty.TestTree
drawCardTests cards =
  Tasty.testGroup
    "DrawCard"
    [ HU.testCase "CR 121.2 drawCard moves the top library card to hand" $
        let base = Setup.emptyGame S.bothPlayers
            (_, withCard) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            after = Event.drawCard S.alice withCard
         in do
              HU.assertEqual "one card in hand" 1 (S.handSize S.alice after)
              HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.3 drawing from an empty library records the failed draw" $
        let after = Event.drawCard S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after))
    ]
```

Wire `drawCardTests cards` into `tests`.

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Event.drawCard` not in scope (red).

- [x] **Step 3: Add `Event.drawCard`**

In `source/library/Pawl/Event.hs`, add (the module imports `Game`, `GameState`, `Zone`, `PlayerId`; add `import qualified Data.Set as Set` to its import group):

```haskell
-- CR 121.2/121.3: the single-card draw. Move pid's top library card to their
-- hand; an empty library records the failed draw (CR 704.5b makes it a loss at
-- the next state-based-action check). The primitive shared by the draw step
-- (Engine.drawFor), opening hands (Setup.drawCard), and the Draw effect (Resolve).
drawCard :: PlayerId -> GameState -> GameState
drawCard pid gs = case Game.zoneMembers Zone.Library pid gs of
  [] -> gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
  top : _ -> changeZone top Zone.Hand gs
```

- [x] **Step 4: Route the two existing draws through it**

In `source/library/Pawl/Engine.hs`, replace `drawFor`:

```haskell
drawFor :: PlayerId -> Game ()
drawFor pid = State.modify' (Event.drawCard pid)
```

(Remove any now-unused imports Engine used only for the old body — check `cabal build` warnings; `Set`/`Zone` may still be used elsewhere in Engine, so remove only what becomes unused.)

In `source/library/Pawl/Setup.hs`, replace `drawCard`:

```haskell
drawCard :: PlayerId -> Game ()
drawCard pid = State.modify' (Event.drawCard pid)
```

(Setup's opening-hand draw never empties a freshly built 60-card library, so recording `drewFromEmpty` there is unreachable — behavior is preserved.)

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS — the new unit tests green and the whole existing suite green (draw step, deck-out loss, opening hands unchanged). Warning-clean.

- [x] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: consolidate the single-card draw into Event.drawCard (no behavior change)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 6: Draw opcode + Divination

Delivers `Effect.Draw Quantity`, gated by Divination drawing two, plus the draw-from-empty loss path through a spell.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Create: `data/cards/divination.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs` (count → 36)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `Event.drawCard` (Task 5), `Quantity.evaluate` (existing).
- Produces: `Effect.Draw Quantity`; `Cards.divinationPrinting`. `Resolve.slotsOf (Effect.Draw _) == Set.empty`.

- [x] **Step 1: Write the failing tests**

Add to `zoneChangeTests` in `ResolveSpec.hs`. Divination is `{2}{U}` (three mana ≥1 blue) — three Islands.

```haskell
      HU.testCase "CR 120 Divination draws its controller two cards" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 3
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            (_, g2) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice g1
            (gs, spellId) = S.handOne (Cards.divinationPrinting cards) g2
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "two cards drawn to hand" 2 (S.handSize S.alice after)
              HU.assertEqual "library emptied" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.3 a Draw that outruns the library records the loss" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 3
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            (gs, spellId) = S.handOne (Cards.divinationPrinting cards) g1
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after)),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.divinationPrinting` / `Effect.Draw` not in scope (red).

- [x] **Step 3: Add the `Draw` constructor**

In `source/library/Pawl/Type/Effect.hs` (the module imports `Quantity`):

```haskell
  | -- CR 120: the controller draws this many cards. Targetless (a spell's
    -- controller draws, CR 120.2). Empty-library draw is a loss (CR 121.3),
    -- unlike Mill -- the semantic asymmetry that keeps Draw and Mill separate.
    Draw Quantity
```

- [x] **Step 4: Add the classification arms**

`slotsOf`: `Effect.Draw _ -> Set.empty`. `readsX`: `Effect.Draw quantity -> quantity == Quantity.Type.X`. `manaProduced`: `Effect.Draw _ -> Nothing`. `searchesLibrary`: `Effect.Draw _ -> False`. `rewriteEffect`: `Effect.Draw _ -> effect`.

- [x] **Step 5: Add the executor arm**

In `applyEffect`:

```haskell
  Effect.Draw quantity -> do
    gs <- State.get
    case Quantity.evaluate gs source quantity of
      Just n | n > 0 ->
        -- CR 120: draw n, folding the shared primitive so each draw re-reads the
        -- library top and the CR 121.3 empty-library loss is preserved.
        State.modify' (\g -> List.foldl' (\g1 _ -> Event.drawCard controller g1) g [1 .. n])
      _ -> pure ()
```

(`List` and `Quantity` are already imported in `Resolve.hs`; the fold index `[1 .. n]` is `Integer`.)

- [x] **Step 6: Add the codec arm**

`effectToJson`: `Effect.Draw q -> Json.tagged (Text.pack "Draw") (Just (quantityToJson q))`. `jsonToEffect`: `"Draw" -> withValue mv (fmap Effect.Draw . jsonToQuantity)`.

- [x] **Step 7: Create and register Divination**

Create `data/cards/divination.json`:

```json
{"name":"Divination","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Sorcery"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Draw","value":{"type":"Literal","value":2}}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[]}
```

Register in `Cards.hs`.

- [x] **Step 8: Card-data test and count → 36**

Bump count to 36. Add to `m4bCardTests`:

```haskell
      HU.testCase "Divination is a {2}{U} Sorcery that draws two cards with no target" $
        let c = Printing.card (Cards.divinationPrinting cards)
         in do
              HU.assertEqual "effect draws two" [Effect.Draw (Quantity.Type.Literal 2)] (Card.Type.effects c)
              HU.assertBool "no target slots" (Map.null (Card.Type.targetSpecs c)),
```

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (draw + loss green; count 36; the D4 X-lint stays green — Divination reads no `X` and declares no `{X}`; round-trip green).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: Draw opcode + Divination (draw two; empty-library loss through a spell)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 7: Mill opcode + Tome Scour

Delivers `Effect.Mill SlotName Quantity`, gated by Tome Scour milling five from a target player's library (and fewer from a short library, no loss).

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`, `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Codec.hs`
- Create: `data/cards/tome-scour.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs` (count → 37)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Effect.Mill SlotName Quantity`; `Cards.tomeScourPrinting`. `Resolve.slotsOf (Effect.Mill s _) == Set.singleton s`.

- [x] **Step 1: Write the failing tests**

First add `import qualified Data.List as List` to `ResolveSpec.hs` (this file does **not** currently import it, and the helpers below use `List.foldl'`).

Tome Scour is `{U}` and targets a player. `identityAnswer` picks `Set.lookupMin` of the `PlayerTarget` set, which is `Recipient.ToPlayer S.alice` (alice's id 0 < bob's 1). To target bob, add a local answerer.

```haskell
-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p
```

Add to `zoneChangeTests`:

```haskell
      HU.testCase "CR 701.13 Tome Scour mills five from a target player's library" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            withLib = List.foldl' (\g _ -> snd (S.addLibraryCard (Cards.pikerPrinting cards) S.bob g)) base [1 .. (6 :: Int)]
            (gs, spellId) = S.handOne (Cards.tomeScourPrinting cards) withLib
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "five milled to graveyard" 5 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "one card left in library" 1 (length (Game.zoneMembers Zone.Library S.bob after)),
      HU.testCase "CR 701.13b milling a short library mills fewer with no loss" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob base
            (_, g2) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob g1
            (gs, spellId) = S.handOne (Cards.tomeScourPrinting cards) g2
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop))
         in do
              HU.assertEqual "two milled" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertBool "bob did not lose (milling is not drawing)" (not (Set.member S.bob (GameState.drewFromEmpty after))),
```

(`List` is already imported in `ResolveSpec.hs`; `Prompt` too.)

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.tomeScourPrinting` / `Effect.Mill` not in scope (red).

- [x] **Step 3: Add the `Mill` constructor**

```haskell
  | -- CR 701.13: the slot's target player mills this many (top N of their library
    -- to their graveyard). Milling a short/empty library mills fewer, no penalty
    -- (CR 701.13b) -- unlike Draw, which loses on empty.
    Mill SlotName Quantity
```

- [x] **Step 4: Add the classification arms**

`slotsOf`: `Effect.Mill slot _ -> Set.singleton slot`. `readsX`: `Effect.Mill _ quantity -> quantity == Quantity.Type.X`. `manaProduced`: `Effect.Mill {} -> Nothing`. `searchesLibrary`: `Effect.Mill {} -> False`. `rewriteEffect`: `Effect.Mill {} -> effect`.

- [x] **Step 5: Add the executor arm**

```haskell
  Effect.Mill slot quantity ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just (Recipient.ToPlayer target), True) ->
          case Quantity.evaluate gs source quantity of
            Just n | n > 0 ->
              -- CR 701.13/701.13b: top min(n, library) of the target's library to
              -- their graveyard, funnelled so each move mints a new incarnation.
              let topN = take (fromInteger n) (Game.zoneMembers Zone.Library target gs)
               in List.foldl' (\g c -> Event.changeZone c Zone.Graveyard g) gs topN
            _ -> gs
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
```

- [x] **Step 6: Add the codec arm**

`effectToJson`: `Effect.Mill s q -> Json.tagged (Text.pack "Mill") (Just (Array [slotNameToJson s, quantityToJson q]))`. `jsonToEffect`:

```haskell
    "Mill" -> case mv of
      Just (Array [s, q]) -> Effect.Mill <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Mill expects [slot, quantity]")
```

- [x] **Step 7: Create and register Tome Scour**

Create `data/cards/tome-scour.json`:

```json
{"name":"Tome Scour","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Sorcery"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Mill","value":["target",{"type":"Literal","value":5}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"PlayerTarget"}}]}
```

Register in `Cards.hs`.

- [x] **Step 8: Card-data test and count → 37**

Bump count to 37. Add to `m4bCardTests`:

```haskell
      HU.testCase "Tome Scour is a {U} Sorcery milling five from a target player" $
        let c = Printing.card (Cards.tomeScourPrinting cards)
         in do
              HU.assertEqual "effect mills five" [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 5)] (Card.Type.effects c)
              HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.PlayerTarget) (Card.Type.targetSpecs c),
```

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (mill + short-library green; count 37; round-trip green).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: Mill opcode + Tome Scour (target player mills; short library, no loss)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 8: Discard opcode + Mind Rot

Delivers `Effect.Discard SlotName Quantity`, reusing the existing `Prompt.ChooseDiscard`. Gated by Mind Rot: a real choice when the hand exceeds the count, and a forced full-hand discard that is *not* prompted.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`, `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Codec.hs`
- Create: `data/cards/mind-rot.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs` (count → 38)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `Prompt.ChooseDiscard`, `Decide.deciderFor` (existing).
- Produces: `Effect.Discard SlotName Quantity`; `Cards.mindRotPrinting`. `Resolve.slotsOf (Effect.Discard s _) == Set.singleton s`.

- [x] **Step 1: Write the failing tests**

Mind Rot is `{2}{B}` and targets a player. To exercise the real choice, put extra cards in the target's hand; to exercise the forced elision, leave the hand equal to the count and answer `ChooseDiscard` with `[]` (so a *prompted* discard would discard nothing, distinguishing prompt from elision). A local helper puts `k` cards in a player's hand.

First add `import qualified Pawl.Type.PlayerId as PlayerId` to `ResolveSpec.hs` (it is not currently imported; `handCards`'s signature needs it). `Data.List as List` was added in Task 7. `Object`, `Source`, `TapState`, `Sickness`, `Timestamp`, `Seq` are already imported.

```haskell
-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs = List.foldl' (\g _ -> addOne g) gs [1 .. k]
  where
    addOne g =
      let (oid, g1) = Game.freshObjectId g
          obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 Sickness.Settled Map.empty (Timestamp.MkTimestamp 0)
       in g1
            { GameState.objects = Map.insert oid obj (GameState.objects g1),
              GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
            }
```

Add to `zoneChangeTests` (`atBobAnswer` was defined in Task 7 in this same file):

```haskell
      HU.testCase "CR 701.8 Mind Rot discards two chosen cards from a hand of three" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            withHand = handCards (Cards.pikerPrinting cards) S.bob 3 base
            (gs, spellId) = S.handOne (Cards.mindRotPrinting cards) withHand
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "one card left in bob's hand" 1 (S.handSize S.bob after)
              HU.assertEqual "two cards in bob's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 701.8b a forced full-hand discard is not prompted" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            withHand = handCards (Cards.pikerPrinting cards) S.bob 2 base
            (gs, spellId) = S.handOne (Cards.mindRotPrinting cards) withHand
            -- Answer ChooseDiscard with [] so a prompt would discard nothing;
            -- aim the spell at bob.
            noDiscard q = case q of
              Prompt.ChooseDiscard {} -> []
              Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
              _ -> S.identityAnswer q
            cast = snd (Engine.runGamePure noDiscard gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
         in do
              -- Elision (hand == count): the whole hand is discarded without asking.
              HU.assertEqual "bob's hand emptied" 0 (S.handSize S.bob after)
              HU.assertEqual "both cards discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.mindRotPrinting` / `Effect.Discard` not in scope (red).

- [x] **Step 3: Add the `Discard` constructor**

```haskell
  | -- CR 701.8: the slot's target player discards this many. The DISCARDING player
    -- chooses which (CR 701.8a) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. A hand smaller than the count discards all of it (CR
    -- 701.8b), forced -- so it is not prompted.
    Discard SlotName Quantity
```

- [x] **Step 4: Add the classification arms**

`slotsOf`: `Effect.Discard slot _ -> Set.singleton slot`. `readsX`: `Effect.Discard _ quantity -> quantity == Quantity.Type.X`. `manaProduced`: `Effect.Discard {} -> Nothing`. `searchesLibrary`: `Effect.Discard {} -> False`. `rewriteEffect`: `Effect.Discard {} -> effect`.

- [x] **Step 5: Add the executor arm**

`applyEffect` (the module imports `Decide`, `Program`, `Prompt`, `Trans`, `Recipient`, `List`; mirror `Engine.discardToHandSize`'s safety filter):

```haskell
  Effect.Discard slot quantity -> do
    gs <- State.get
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate gs source quantity of
          Just n | n > 0 -> do
            let held = Game.zoneMembers Zone.Hand target gs
                bury cs g = List.foldl' (\g1 c -> Event.changeZone c Zone.Graveyard g1) g cs
            if fromInteger n >= length held
              -- CR 701.8b: the whole hand is forced -- no choice, so no prompt.
              then State.modify' (bury held)
              else do
                -- CR 701.8a: the discarding player chooses which cards.
                let decider = Decide.deciderFor target gs
                choices <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider target held (fromInteger n)))
                let toDiscard = take (fromInteger n) (filter (\c -> elem c held) choices)
                State.modify' (bury toDiscard)
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
```

- [x] **Step 6: Add the codec arm**

`effectToJson`: `Effect.Discard s q -> Json.tagged (Text.pack "Discard") (Just (Array [slotNameToJson s, quantityToJson q]))`. `jsonToEffect`:

```haskell
    "Discard" -> case mv of
      Just (Array [s, q]) -> Effect.Discard <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Discard expects [slot, quantity]")
```

- [x] **Step 7: Create and register Mind Rot**

Create `data/cards/mind-rot.json`:

```json
{"name":"Mind Rot","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Sorcery"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Discard","value":["target",{"type":"Literal","value":2}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"PlayerTarget"}}]}
```

Register in `Cards.hs`.

- [x] **Step 8: Card-data test and count → 38**

Bump count to 38. Add to `m4bCardTests`:

```haskell
      HU.testCase "Mind Rot is a {2}{B} Sorcery making a target player discard two" $
        let c = Printing.card (Cards.mindRotPrinting cards)
         in do
              HU.assertEqual "effect discards two" [Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 2)] (Card.Type.effects c)
              HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.PlayerTarget) (Card.Type.targetSpecs c)
```

- [x] **Step 9: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: PASS (both discard tests green; count 38; round-trip green over 38 printings; D4 lint green).

- [x] **Step 10: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: Discard opcode + Mind Rot (chooser via ChooseDiscard; forced full-hand elided)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 9: Random-game coverage (the fast follow)

Give the highest-value verbs random-play coverage before M4c: add Murder + Mind Rot to the black deck (Destroy + Discard) and a new blue deck + blue/black matchup (bounce + draw + mill). Every deck stays exactly 60 cards, so the property suite's 120-object conservation holds.

**Files:**
- Modify: `source/test-suite/Pawl/Cards.hs` (`blackDeck`, new `blueDeck`)
- Modify: `source/test-suite/Pawl/Support.hs` (`blueBlack`, `matchups`)
- Test: `source/test-suite/Pawl/PropertySpec.hs` (already iterates `S.matchups` — no edit, just re-run)

**Interfaces:**
- Consumes: the seven M4b card accessors, `Cards.islandPrinting`, `Cards.swampPrinting`, `Cards.typhoidRatsPrinting` (existing).
- Produces: `Cards.blueDeck :: Cards -> Deck.Deck`; `Support.blueBlack`; `Support.matchups` now returns three matchups.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/Support.hs` the new matchup and extend `matchups` (this is the "test" — the property suite consumes `matchups`, so a not-yet-defined `blueBlack`/`blueDeck` is the red):

```haskell
blueBlack :: Cards.Cards -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
blueBlack cards = (alice, Cards.blueDeck cards) NonEmpty.:| [(bob, Cards.blackDeck cards)]

matchups :: Cards.Cards -> [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups cards = [redRed cards, greenBlack cards, blueBlack cards]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 | tail -20`
Expected: compile failure — `Cards.blueDeck` not in scope (red).

- [x] **Step 3: Add the blue deck and extend the black deck**

In `source/test-suite/Pawl/Cards.hs`, add `blueDeck` and rewrite `blackDeck` (each totals 60):

```haskell
-- Blue, no creatures: Divination accelerates its own deck-out, Unsummon bounces
-- the opponent's creatures, Tome Scour mills them. Gives bounce/draw/mill random
-- coverage (M4b fast follow).
blueDeck :: Cards -> Deck.Deck
blueDeck cards =
  Deck.MkDeck $
    Map.fromList
      [ (islandPrinting cards, 40),
        (unsummonPrinting cards, 8),
        (divinationPrinting cards, 8),
        (tomeScourPrinting cards, 4)
      ]

blackDeck :: Cards -> Deck.Deck
blackDeck cards =
  Deck.MkDeck $
    Map.fromList
      [ (swampPrinting cards, 36),
        (typhoidRatsPrinting cards, 16),
        -- Murder and Mind Rot give Destroy and Discard random-play coverage.
        (murderPrinting cards, 4),
        (mindRotPrinting cards, 4)
      ]
```

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -40`
Expected: PASS — the property suite runs over three matchups; each still ends with 120 objects, terminates, mints ≥120 ids, and empties the mana pool. If a random game surfaces a real bug (non-termination, lost object), that is a genuine finding — stop and diagnose per systematic-debugging, do **not** weaken the property.

- [x] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b fast follow: random-game coverage (Destroy/Discard in black, bounce/draw/mill in a new blue matchup)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Task 10: Milestone completion log + status update

Record M4b in the completion log and flip the forward-status notes, matching the M4a wrap-up commit.

**Files:**
- Modify: `docs/progress.md` (new M4b entry after the M4a entry)
- Modify: `CLAUDE.md` (the "Current work and tracking" bullet)

- [x] **Step 1: Append the M4b completion-log entry**

Add to `docs/progress.md`, after the M4a entry, one distilled bullet in the house style (gate card, decision proved, opcodes/types added, elisions with named expiries):

```markdown
- **M4b is complete** (the targeted zone-change opcode family. **Gate: Murder**
  (`{1}{B}{B}` Instant, "Destroy target creature") vs. **Darksteel Myr** (`{3}`
  0/1 Artifact Creature, Indestructible): the falsifier is that modelling destroy
  as a move-to-graveyard buries the Myr, so `Effect.Destroy` is its own opcode
  that consults CR 700.4 indestructibility before funnelling to the graveyard.
  Five opcodes, all executed only by `Resolve.applyEffect` through M3f's
  `Event.changeZone` funnel — proving it generalizes past Rest in Peace's single
  redirect into card-driven verbs: `Destroy SlotName`; `MoveToZone SlotName Zone`
  (Unsummon bounces to Hand, Angelic Edict exiles); `Draw Quantity` (Divination,
  controller-targetless, empty-library loss preserved via the consolidated
  `Event.drawCard`); `Mill SlotName Quantity` (Tome Scour, target player, short
  library mills fewer with no loss, CR 701.13b); `Discard SlotName Quantity`
  (Mind Rot, reusing `Prompt.ChooseDiscard`, the discarding player choosing per
  CR 701.8a). **`Keyword.Indestructible`** (CR 702.12) is read through the
  projection at two independent sites — the `Destroy` opcode and `Sba.creatureDies`
  (guarding CR 704.5g lethal damage and 704.5h deathtouch but **not** 704.5f
  toughness ≤ 0, which is a put-into-graveyard, not a destruction) — so Humility
  strips it for free. New types: `TargetSpec.CreatureOrEnchantmentTarget` (the
  first spec admitting a non-creature permanent, exercised by exiling an
  enchantment), `Subtype.Myr`. The single-card draw was consolidated into one
  `Event.drawCard` shared by the draw step, opening hands, and the Draw opcode.
  Seven cards (Darksteel Myr, Murder, Unsummon, Angelic Edict, Divination, Tome
  Scour, Mind Rot), Scryfall-verified, deterministic fixtures, with a fast-follow
  random-game matchup (blue/black) carrying the high-value verbs. **Named
  elisions/expiries**: Destroy is a plain check-then-move, not yet an interceptable
  destroy event — regeneration/prevention (CR 615) makes it replaceable at **M4d**;
  the 704.5f toughness-drop test uses a synthetic `-0/-1` continuous effect until
  the first real **−1/−1** ability; a forced full-hand discard is elided (not
  prompted) per the engine-makes-no-choices rule; derived references ("its
  controller"/"its power", Path/Swords) and a lifegain opcode are deferred with the
  first card that needs them; `MoveToZone slot Graveyard` (an unconditional
  put-into-graveyard, distinct from Destroy) awaits its first card. Spec and plan
  kept as reference:
  `docs/superpowers/specs/2026-07-19-m4b-zone-change-verbs-design.md` and
  `docs/superpowers/plans/2026-07-19-m4b-zone-change-verbs.md`.
```

- [x] **Step 2: Update the CLAUDE.md status bullet**

In `CLAUDE.md`, the "Current work and tracking" section: change "**M0–M3g, M3.5, and M4a are complete**" to include M4b, update the M4a parenthetical to note M4b landed the zone-change verbs, and change "**M4b is next.**" to "**M4c (tokens) is next.**" Keep the sentence about the milestone completion log and the forward path unchanged.

- [x] **Step 3: Verify the docs and commit**

Run: `grep -c "M4b is complete" docs/progress.md` (expect 1) and re-read the two edits.

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4b: milestone completion log entry + status update

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01JrPQomB3CLM3ek2J4yVWFd"
```

---

## Final verification (after all tasks)

- [ ] `cabal clean && cabal build all --enable-tests --enable-benchmarks` — warning-clean from scratch (incremental builds hide warnings).
- [ ] `cabal test` — the whole suite green, including the honesty round-trip over all 38 printings, the D4 slot lint, the D4 X-lint, and the three-matchup property suite.
- [ ] `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-19-m4b-zone-change-verbs.md` — reaches 0.
- [ ] `git add -A && hooky fix && git add -A && hooky run` — clean.
