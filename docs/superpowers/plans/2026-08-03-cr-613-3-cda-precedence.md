# CR 613.3 CDA-Before-Timestamp Precedence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move devoid from the projection seed to an in-place characteristic-defining ability at the start of layer 5 (CR 613.3), and add CR 105.3's "in addition" colour vocabulary that makes the ordering observable.

**Architecture:** `Projection.projectWith` already seeds layer 7a with the object's own characteristic-defining P/T; layer 5 gains the identical hook, reading `Devoid` off the *partial* projection's keywords. Two new `Modification` arms carry CR 105.3's parenthetical, one with a literal colour set and one that reads a colour chosen for the effect's source as it entered. A new `Affected` arm drops the battlefield gate so Painter's Servant reaches spells on the stack.

**Tech Stack:** GHC 9.14.1 via the Nix flake, `cabal`, `tasty`, `cabal-gild`, `hooky`.

**Spec:** `docs/superpowers/specs/2026-08-03-cr-613-3-cda-precedence-design.md`

**Issue:** #35

## Global Constraints

- **One PR, small commits.** Every commit builds warning-free and leaves the suite green. Open the PR as a draft only after the last task; mark it ready once self-review findings are pushed.
- **One build at a time.** `cabal build all`, never just the library — the suites break separately. Never poke inside `dist-newstyle`. Never `cabal clean`.
- **Run every tool through direnv:** `direnv exec . cabal build all`, `direnv exec . cabal test`, `direnv exec . hooky fix`.
- **`hooky` acts on staged files.** `git add`, then `hooky fix`, then `git add` again, then `hooky run`.
- **`cabal-gild pawl.cabal` must be run directly** whenever a module is added or deleted; `hooky fix` only covers staged files.
- **Never trust recalled Magic rules.** Every CR citation this plan writes into a comment was checked against `docs/rules.txt` on 2026-08-03. Any *other* citation you touch must be re-checked before you leave it there.
- **The rules core must not case on an effect's identity.** `Pawl.Engine.Projection` is the one module allowed to case on `Modification` (see `Types/Modification.hs`'s header). Do not add a `case` on `Modification` anywhere else.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **`git add` explicit paths, never `git add -A`.** Other sessions may share this checkout.
- Build must be warning-free under `-Weverything` (`pawl.cabal`'s `common warnings`).

## Adding a constructor: how to find every site

`-Weverything` makes every incomplete `case` a build error, so the compiler
enumerates the sites for you. Add the constructor, run
`direnv exec . cabal build all`, and fix what it names. The known total cases are:

- **`Modification`** — `Projection.layer`, `Projection.applyModification`,
  `Projection.freezeQuantities`, `Projection.removesAbilities`,
  `Projection.modificationWrites`, `Codec.Modification.toJson`/`fromJson`.
- **`Affected`** — `Projection.affects`, `Projection.staticallyMovable`,
  `projectWith`'s local `movableReads`, `Codec.Affected.toJson`/`fromJson`.
- **`EntryRewrite`** — `Replacement.hs`'s apply loop plus its `bucket` and its
  copy-ish predicate (lines ~597 and ~651), `Codec.EntryRewrite.toJson`/`fromJson`.
- **`Prompt`** — `Replay.hs` (three cases: record, match, default),
  `source/benchmark/Main.hs` (three), and the deciders in `Pawl/Support.hs`
  (four), `Pawl/GameSpec.hs` (two), `Pawl/CastSpec.hs` (three),
  `Pawl/ReplacementSpec.hs` (one). Add the new arm to each.
- **`Response`** — `Codec.Response.toJson`/`fromJson`, `Replay.hs`.

---

## Task 1: Slaughter Drone replaces the synthetic devoid drone

Pure card swap. No engine change, no behaviour change — the suite must stay green
on the new card. Doing it first means every later task's tests are written against
a real card.

Slaughter Drone (`{1}{B}`, Creature — Eldrazi Drone, 2/2, "Devoid. `{C}`: This
creature gains deathtouch until end of turn.") is a cost-and-P/T match for the
synthetic, so the two specs move across unchanged in shape.

**Files:**
- Create: `data/cards/slaughter-drone.json`
- Delete: `data/cards/synthetic-devoid-drone.json`
- Modify: `source/test-suite/Pawl/ColorSpec.hs` (five `"Synthetic Devoid Drone"` lookups)
- Modify: `source/test-suite/Pawl/CombatSpec.hs` (its `"Synthetic Devoid Drone"` lookups)

**Interfaces:**
- Produces: the registry name `"Slaughter Drone"`, resolvable via
  `S.printingOf s registry "Slaughter Drone"`.

- [ ] **Step 1: Write the card**

Create `data/cards/slaughter-drone.json`. The shape mirrors
`data/cards/synthetic-devoid-drone.json` with an activated ability added. The
activated-ability shape is `data/cards/aladdin.json`'s, minus the target spec.

```json
{
  "activatedAbilities": [
    {
      "cost": {
        "components": [],
        "mana": [{ "type": "OfType", "value": { "type": "Colorless" } }]
      },
      "modal": {
        "modes": [
          {
            "effects": [
              {
                "type": "ModifyTarget",
                "value": [
                  { "type": "UntilEndOfTurn" },
                  { "type": "GainKeyword", "value": { "type": "Deathtouch" } },
                  "self"
                ]
              }
            ],
            "targetSpecs": []
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "castingPermissions": [],
  "keywords": [{ "type": "Devoid" }],
  "manaCost": [
    { "type": "Generic", "value": 1 },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Black" } } }
  ],
  "name": "Slaughter Drone",
  "power": { "type": "Literal", "value": 2 },
  "replacementEffects": [],
  "spell": {
    "modes": [{ "effects": [], "targetSpecs": [] }],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "toughness": { "type": "Literal", "value": 2 },
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [{ "type": "Eldrazi" }, { "type": "Drone" }],
    "supertypes": [],
    "types": [{ "type": "Creature" }]
  }
}
```

**Two things to verify rather than assume**, because this plan could not check
them without building:

1. **`"self"` as a `ModifyTarget` slot.** Slaughter Drone's ability modifies the
   creature it is on, with no target. Grep for how an existing self-modifying
   activated ability names its slot — `grep -l 'ModifyTarget' data/cards/*.json`
   and look at one whose `targetSpecs` is empty. If no such card exists, give the
   ability a `targetSpecs` entry with `Pool.Creatures` and a `Filter.IsSource`
   conjunct instead, which is the vocabulary's existing way to say "this
   creature", and note the substitution in the commit message.
2. **`Subtype.Eldrazi` and `Subtype.Drone` exist.** If either is missing, adding a
   Subtype has a known edit-site set — see `CLAUDE.md` and
   `Pawl.Engine.Subtype`; the easy-to-miss site is `Mana.subtypeMana`. Add both.

- [ ] **Step 2: Verify the card round-trips**

`Pawl.CardsSpec` round-trips every file in `data/cards/`, so a malformed field
fails there rather than at the point of use.

Run: `direnv exec . cabal test 2>&1 | tail -30`
Expected: `Pawl.CardsSpec` passes. Everything else still passes — nothing
references the new card yet.

- [ ] **Step 3: Move the specs onto the real card**

In `source/test-suite/Pawl/ColorSpec.hs`, replace every
`S.printingOf s registry "Synthetic Devoid Drone"` with
`S.printingOf s registry "Slaughter Drone"`, and rename the binding
`devoidDrone` to `slaughterDrone` at each site. Do the same in
`source/test-suite/Pawl/CombatSpec.hs`.

Then re-read the comments above each of those tests. Several say "this card's
cost is `{1}{B}`" or call the card synthetic; the cost claim is still true, the
synthetic claim is not. `ColorSpec.hs:63`'s comment in particular reads "THE
FALSIFIER for 'an object's colours are the coloured symbols in its mana cost'" —
still true. Fix only what the swap made wrong.

- [ ] **Step 4: Delete the synthetic**

```bash
git rm data/cards/synthetic-devoid-drone.json
```

- [ ] **Step 5: Verify green**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`
Expected: build warning-free, suite green, count unchanged.

- [ ] **Step 6: Commit**

```bash
git add data/cards/slaughter-drone.json source/test-suite/Pawl/ColorSpec.hs source/test-suite/Pawl/CombatSpec.hs
direnv exec . hooky fix
git add data/cards/slaughter-drone.json source/test-suite/Pawl/ColorSpec.hs source/test-suite/Pawl/CombatSpec.hs
direnv exec . hooky run
git commit -m "Retire the synthetic devoid drone for Slaughter Drone"
```

---

## Task 2: `Modification.AddColor`, with Indigo Faerie and Red Elemental Blast

CR 105.3: "If an effect gives an object a new color, the new color replaces all
previous colors the object had (unless the effect said the object became that
color 'in addition' to its other colors)." `SetColor` is the first half of that
sentence; this is the parenthetical.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Modification.hs`
- Modify: `source/libraries/codec/Pawl/Codec/ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs` (`layer`, `applyModification`, `freezeQuantities`, `removesAbilities`, `modificationWrites`)
- Create: `data/cards/indigo-faerie.json`
- Create: `data/cards/red-elemental-blast.json`
- Modify: `source/test-suite/Pawl/ColorSpec.hs`

**Interfaces:**
- Consumes: `"Slaughter Drone"` from Task 1.
- Produces: `Modification.AddColor :: Set.Set Color.Color -> Modification`, wire
  tag `"AddColor"` with a colour array as its value; registry names
  `"Indigo Faerie"` and `"Red Elemental Blast"`.

- [ ] **Step 1: Write the failing test**

Append to `source/test-suite/Pawl/ColorSpec.hs`, beside the existing
`"CR 105.3 a new colour REPLACES all previous colours"` test:

```haskell
  Spec.it s "CR 105.3 an 'in addition' colour effect ADDS rather than replaces" $ do
    -- The falsifier for implementing every colour change as a replacement: the
    -- Rats are black, and after an AddColor they are black AND blue.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addCreature typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.AddColor (Set.singleton Color.Blue)) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.fromList [Color.Black, Color.Blue]
```

- [ ] **Step 2: Run it and watch it fail**

Run: `direnv exec . cabal build all 2>&1 | tail -20`
Expected: FAIL — `Data constructor not in scope: Modification.AddColor`.

- [ ] **Step 3: Add the constructor**

In `source/libraries/types/Pawl/Types/Modification.hs`, directly after the
`SetColor` arm:

```haskell
  | -- | layer 5, CR 613.1e / 105.3: this object becomes these colours IN
    -- ADDITION to the ones it already has. CR 105.3's parenthetical -- "unless
    -- the effect said the object became that color 'in addition' to its other
    -- colors" -- so this unions where SetColor replaces. Indigo Faerie's
    -- "target permanent becomes blue in addition to its other colors".
    AddColor (Set.Set Color.Color)
```

**Also fix the prose the new arm falsifies.** `SetCreatureSubtype`'s comment
currently reads "Unlike SetColor's missing AddColor -- a RULES answer, since CR
105.3 makes every colour change a replacement". That was wrong when it was
written: CR 105.3's parenthetical is exactly the exception. Replace that sentence
with:

```haskell
    -- No AddCreatureSubtype beside it, though AddColor now sits beside SetColor
    -- below: adding a creature type is a real thing an effect can do ("in
    -- addition to its other types"), and it is absent only because no card in
    -- the pool does it, so the constructor would have no producer.
```

Then delete the "and no card in the pool does, so there is deliberately no
AddColor constructor" clause from `SetColor`'s own comment, which the new arm
also falsifies.

- [ ] **Step 4: Let the build enumerate the remaining sites**

Run: `direnv exec . cabal build all 2>&1 | grep -A3 "Pattern match"`

Fix each named site:

`Projection.layer` — beside `SetColor`:
```haskell
  Modification.AddColor _ -> Layer.Color
```

`Projection.applyModification` — beside the `SetColor` arm at ~line 250:
```haskell
        -- CR 105.3's parenthetical: an "in addition" colour is added to the
        -- object's existing colours rather than replacing them.
        Modification.AddColor cs ->
          pc {PC.colors = Set.union cs (PC.colors pc)}
```

`Projection.freezeQuantities` — beside `SetColor _ -> Just m`:
```haskell
        Modification.AddColor _ -> Just m
```

`Projection.removesAbilities` — a colour change is not an ability removal:
```haskell
  Modification.AddColor _ -> False
```
(Match the surrounding arms' shape; if that function returns `False` via a
catch-all rather than per-arm, leave it alone — the header says it is total, so
it will not be a catch-all.)

`Projection.modificationWrites` — beside `SetColor _ -> Set.singleton Colors`:
```haskell
  Modification.AddColor _ -> Set.singleton Colors
```

`Codec.Modification.toJson`, beside `SetColor`:
```haskell
  Modification.AddColor cs -> Common.tagged "AddColor" . Just $ Common.encodeSet Color.toJson cs
```

`Codec.Modification.fromJson`, beside `"SetColor"`:
```haskell
    "AddColor" -> Common.withValue mv (fmap Modification.AddColor . Common.decodeSet Color.fromJson)
```

- [ ] **Step 5: Pin the wire format**

In `source/libraries/codec/Pawl/Codec/ModificationSpec.hs`, beside the `SetColor`
case, following that file's existing shape exactly:

```haskell
  Spec.it s "AddColor" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddColor (Set.singleton Color.Blue))
      "{\"type\":\"AddColor\",\"value\":[{\"type\":\"Blue\"}]}"
```

Read the neighbouring `SetColor` case first and copy its helper names and
argument order — this snippet is written from the file's general shape, not from
that exact line.

- [ ] **Step 6: Run and verify green**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`
Expected: build warning-free, the new `AddColor` test passes, count up by two.

- [ ] **Step 7: Write Indigo Faerie**

Create `data/cards/indigo-faerie.json` — `{1}{U}`, Creature — Faerie Wizard, 1/1,
"Flying. `{U}`: Target permanent becomes blue in addition to its other colors
until end of turn."

The activated ability is `data/cards/aladdin.json`'s shape (mana cost, one mode,
one target spec) with `Pool.Permanents` and no filter — "target permanent". The
effect is `crimson-wisps.json`'s `ModifyTarget` with `AddColor` swapped in.
Omitted fields (`castingPermissions`, `replacementEffects`, `staticAbilities`,
`triggeredAbilities`, the empty `spell`) take `slaughter-drone.json`'s values.

```json
  "activatedAbilities": [
    {
      "cost": {
        "components": [],
        "mana": [{ "type": "OfType", "value": { "type": "Colored", "value": { "type": "Blue" } } }]
      },
      "modal": {
        "modes": [
          {
            "effects": [
              {
                "type": "ModifyTarget",
                "value": [
                  { "type": "UntilEndOfTurn" },
                  { "type": "AddColor", "value": [{ "type": "Blue" }] },
                  "target"
                ]
              }
            ],
            "targetSpecs": [
              { "slot": "target", "spec": { "pool": { "type": "Permanents" } } }
            ]
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "keywords": [{ "type": "Flying" }],
  "manaCost": [
    { "type": "Generic", "value": 1 },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Blue" } } }
  ],
  "name": "Indigo Faerie",
  "power": { "type": "Literal", "value": 1 },
  "toughness": { "type": "Literal", "value": 1 },
  "typeLine": {
    "subtypes": [{ "type": "Faerie" }, { "type": "Wizard" }],
    "supertypes": [],
    "types": [{ "type": "Creature" }]
  }
```

- [ ] **Step 8: Write Red Elemental Blast**

Create `data/cards/red-elemental-blast.json` — `{R}`, Instant, "Choose one — •
Counter target blue spell. • Destroy target blue permanent."

Mode 1's effect is `cancel.json`'s `Counter`; mode 2's is `doom-blade.json`'s
`Destroy`, with the filter flipped from `Not (HasColor Black)` over
`Pool.Creatures` to `HasColor Blue` over `Pool.Permanents`. Two modes with
`ChooseExactly 1` is `chaos-charm.json`'s shape.

```json
  "activatedAbilities": [],
  "keywords": [],
  "manaCost": [
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Red" } } }
  ],
  "name": "Red Elemental Blast",
  "spell": {
    "modes": [
      {
        "effects": [{ "type": "Counter", "value": "spell" }],
        "targetSpecs": [
          {
            "slot": "spell",
            "spec": {
              "filter": { "type": "HasColor", "value": { "type": "Blue" } },
              "pool": { "type": "Spells" }
            }
          }
        ]
      },
      {
        "effects": [{ "type": "Destroy", "value": ["permanent", { "type": "Regenerable" }] }],
        "targetSpecs": [
          {
            "slot": "permanent",
            "spec": {
              "filter": { "type": "HasColor", "value": { "type": "Blue" } },
              "pool": { "type": "Permanents" }
            }
          }
        ]
      }
    ],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [{ "type": "Instant" }]
  }
```

`Instant` has no power/toughness box; follow another instant in the pool
(`doom-blade.json`) for whether `"power"`/`"toughness"` are omitted or null.

- [ ] **Step 9: Write the Indigo Faerie test**

Append to `source/test-suite/Pawl/ColorSpec.hs`. This is spec §4's T3.

```haskell
  Spec.it s "Indigo Faerie's 'in addition' blue makes a devoid drone a legal blue target" $ do
    -- Slaughter Drone is devoid, so it is colourless until something adds a
    -- colour. Red Elemental Blast's destroy mode reads blue, and the drone is
    -- outside its set until Indigo Faerie's ability resolves.
    indigoFaerie <- S.printingOf s registry "Indigo Faerie"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withFaerie) = S.addCreature indigoFaerie S.alice gs0
        (droneId, board) = S.addCreature slaughterDrone S.alice withFaerie
    Spec.assertEqWith s "colourless before" (Projection.colorsOf droneId board) Set.empty
    let gs = S.withEffect droneId (Modification.AddColor (Set.singleton Color.Blue)) board
    Spec.assertEqWith s "blue after" (Projection.colorsOf droneId gs) $ Set.singleton Color.Blue
```

**Then replace `S.withEffect` with a real activation.** `S.withEffect` installs
the modification directly and so proves the projection, not the card. Find how an
existing spec activates an ability through the engine — grep `ColorSpec.hs` and
`Pawl/ActivateSpec.hs` for `Activate.` — and drive Indigo Faerie's `{U}` ability
targeting the drone. The `S.withEffect` version above is the stepping stone, not
the deliverable; the assertion values are the same either way.

- [ ] **Step 10: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`

```bash
git add source/libraries/types/Pawl/Types/Modification.hs source/libraries/codec/Pawl/Codec/Modification.hs source/libraries/codec/Pawl/Codec/ModificationSpec.hs source/libraries/engine/Pawl/Engine/Projection.hs data/cards/indigo-faerie.json data/cards/red-elemental-blast.json source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky fix
git add source/libraries/types/Pawl/Types/Modification.hs source/libraries/codec/Pawl/Codec/Modification.hs source/libraries/codec/Pawl/Codec/ModificationSpec.hs source/libraries/engine/Pawl/Engine/Projection.hs data/cards/indigo-faerie.json data/cards/red-elemental-blast.json source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky run
git commit -m "Add CR 105.3's 'in addition' colour: Indigo Faerie"
```

---

## Task 3: Devoid folds at the start of layer 5

The core of #35. Behaviour-preserving for every existing test; the one new
observable is that layers 2–4 read the printed colours.

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs` (`baseColorsOf`, `viewOfCard`, `baseCharacteristics`, `projectWith`)
- Modify: `source/libraries/types/Pawl/Types/Keyword.hs` (the `Devoid` note at ~line 276)
- Modify: `source/test-suite/Pawl/ColorSpec.hs`

**Interfaces:**
- Consumes: `"Slaughter Drone"` from Task 1.
- Produces: `Projection.printedColorsOf :: Card.Type.Card -> Set Color.Color`
  (no devoid test), `Projection.definesColorless :: Set Keyword -> Bool`, and
  `Projection.applyColorDefining :: ProjectedCharacteristics -> ProjectedCharacteristics`.
  `Projection.baseColorsOf` ceases to exist — this project has no API stability
  obligations, so do not leave a shim.

- [ ] **Step 1: Write the failing test**

Append to `source/test-suite/Pawl/ColorSpec.hs`. This is spec §4's T4, and it is
the assertion that pins #35's second bullet.

```haskell
  Spec.it s "CR 613.3 devoid applies at the START of layer 5, so layers 2-4 read the mana cost" $ do
    -- CR 613.3 applies characteristic-defining abilities first WITHIN a layer,
    -- and devoid's layer is 5 (CR 613.1e). A layer-2, -3 or -4 effect whose
    -- affected set is colour-keyed therefore sees the mana cost's black, not
    -- the colourless devoid produces. Seeding devoid gets this wrong (#35).
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addCreature slaughterDrone S.alice gs0
        cands = Projection.gather gs
        below = Projection.projectUpTo Layer.Color cands droneId gs
    Spec.assertEqWith s "black below layer 5" (PC.colors below) $ Set.singleton Color.Black
    Spec.assertEqWith s "colourless once layer 5 has applied" (Projection.colorsOf droneId gs) Set.empty
```

Add `import qualified Pawl.Types.Layer as Layer` and
`import qualified Pawl.Types.ProjectedCharacteristics as PC` to `ColorSpec.hs`'s
import block, in alphabetical position.

- [ ] **Step 2: Run it and watch it fail**

Run: `direnv exec . cabal test 2>&1 | grep -A6 "START of layer 5"`
Expected: FAIL — "black below layer 5" gets `fromList []`, because
`baseColorsOf` already cleared it at the seed.

- [ ] **Step 3: Split `baseColorsOf`**

In `source/libraries/engine/Pawl/Engine/Projection.hs`, replace `baseColorsOf`
(and the ~65-line comment above it, which argues for the seed shortcut this task
removes) with:

```haskell
-- CR 202.2 / 204.2: an object's PRINTED colours -- the colours of the coloured
-- mana symbols in its mana cost, together with the colours its colour indicator
-- denotes. CR 202.2b: an object with no coloured mana symbols and no indicator is
-- colourless.
--
-- No devoid here. CR 702.114a makes devoid a CHARACTERISTIC-DEFINING ability, and
-- CR 613.3 puts characteristic-defining abilities at the START of their layer --
-- layer 5 for colour (CR 613.1e) -- not before the fold begins. applyColorDefining
-- is where it lands; see projectWith.
printedColorsOf :: Card.Type.Card -> Set Color.Color
printedColorsOf card =
  Set.union
    (Card.Type.colorIndicator card)
    (manaCostColors (Card.Type.manaCost card))

-- CR 702.114a: "Devoid is a characteristic-defining ability. 'Devoid' means
-- 'This object is colorless.'" THE one place that decides what devoid means, so
-- the fold and the off-battlefield card view cannot drift apart on it.
definesColorless :: Set Keyword -> Bool
definesColorless = Set.member Keyword.Devoid

-- CR 613.3 / 613.1e: the object's own colour-defining ability, applied at the
-- START of layer 5 -- "within layers 2-6, apply effects from characteristic-
-- defining abilities first, then all other effects in timestamp order".
--
-- Folded IN PLACE rather than emitted as a synthetic Gathered, for these three
-- reasons: a CDA affects only the object it is on (CR 604.3a(3)) so it has no
-- affected set to gather over; CR 604.3 makes it function in ALL zones while
-- gather walks the battlefield only; and it has no source object and no
-- timestamp, so it has nothing to sort on under CR 613.7. NOT
-- applyCharacteristicPT's Humility reason, which is the one that does not
-- transfer: layer 6 is AFTER layer 5.
--
-- Read from the PARTIAL projection's keywords rather than from the card. CR
-- 604.3a(2)'s list of what makes a static ability characteristic-defining is
-- "printed on the card it affects ... granted to the token ... or acquired ... as
-- the result of a copy effect or text-changing effect", and at layer 5 the map
-- holds exactly those (minus the token clause, covered at the seed); it cannot
-- yet hold a layer-6 grant, because layer 6 has not been applied. Pawl has no
-- text-change keyword writer today, so the rule holds by construction rather
-- than by a test.
--
-- Humility therefore cannot remove it: LoseAllAbilities is layer 6, after this.
--
-- Not implemented: a devoid GRANTED by a layer-6 effect does nothing to colour.
-- Per CR 604.3a(2) such a grant is not characteristic-defining, so it would be an
-- ordinary layer-5 colour effect timestamped when granted (CR 613.7a), which this
-- does not build (#NNN).
applyColorDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applyColorDefining pc =
  if definesColorless (Map.keysSet (PC.keywords pc))
    then pc {PC.colors = Set.empty}
    else pc
```

Leave `#NNN` as a literal placeholder for now; Task 7 files the issue and
substitutes the number. Do not commit `#NNN` — Task 7 runs before the PR opens.

- [ ] **Step 4: Point the two callers at the new pair**

`baseCharacteristics` (~line 745) seeds from the printed colours, devoid or not:

```haskell
            PC.colors = printedColorsOf card,
```

`viewOfCard` (~line 551) keeps devoid, because CR 604.3 makes a CDA function in
all zones and an object off the battlefield never enters the fold at all —
`viewUpTo` falls back to the printed card there (#160):

```haskell
          -- CR 604.3 / 702.114a: a characteristic-defining ability functions in
          -- ALL zones, and nothing off the battlefield is projected (viewUpTo
          -- falls back here, #160) -- so devoid is applied here rather than
          -- inherited from a fold this object never enters.
          Filter.colors =
            if definesColorless (Card.Type.keywords card)
              then Set.empty
              else printedColorsOf card,
```

Update `viewOfCard`'s own header comment, which says "colours from baseColorsOf
(devoid -> empty)" — that function no longer exists.

- [ ] **Step 5: Hook layer 5 into the fold**

In `projectWith` (~line 1779), add `Layer.Color` to the unconditional layer list
beside `Layer.CharacteristicPT`:

```haskell
    -- Layer 5 and layer 7a are ALWAYS visited, even when no gathered effect
    -- lives there: an object's own characteristic-defining abilities are not
    -- gathered candidates (applyColorDefining, applyCharacteristicPT). For an
    -- object with neither, each extra pass is an identity function over an empty
    -- candidate filter.
    layers = filter admits (Set.toAscList (Set.insert Layer.Color (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands)))))
```

And replace the `seeded` binding (~line 1794) with a `case`:

```haskell
            let seeded = case lyr of
                  -- CR 613.3: characteristic-defining abilities first, within
                  -- the layer they define. Colour is layer 5 (CR 613.1e), P/T is
                  -- layer 7a (CR 613.4a).
                  Layer.Color -> applyColorDefining partial
                  Layer.CharacteristicPT -> applyCharacteristicPT lyr cands gs oid partial
                  _ -> partial
```

Also update `projectFrom`'s comment, which says "Layer 7a is ALWAYS in the layer
list" — it is now two layers.

- [ ] **Step 6: Run and verify**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`
Expected: build warning-free; the new test passes; **every other test still
passes**. If a devoid test now fails, the fold is not reaching layer 5 — check
that `Layer.Color` really is in `layers` for an object with no layer-5 candidate.

- [ ] **Step 7: Update the Keyword note**

`source/libraries/types/Pawl/Types/Keyword.hs` (~line 276) currently reads
"Devoid is read only at the projection SEED (Projection.baseColorsOf)". Replace
with:

```haskell
-- Devoid is folded as a characteristic-defining ability at the start of layer 5
-- (Projection.applyColorDefining), per CR 613.3. A devoid GRANTED by a layer-6
-- effect still does nothing to colour: CR 604.3a(2) makes such a grant
-- non-characteristic-defining, so it would be an ordinary layer-5 effect, which
-- is not built. No card in the pool grants devoid (#NNN).
```

- [ ] **Step 8: Commit**

```bash
git add source/libraries/engine/Pawl/Engine/Projection.hs source/libraries/types/Pawl/Types/Keyword.hs source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky fix
git add source/libraries/engine/Pawl/Engine/Projection.hs source/libraries/types/Pawl/Types/Keyword.hs source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky run
git commit -m "Fold devoid at the start of layer 5, per CR 613.3"
```

---

## Task 4: `Affected.MatchingAnywhere`

`affects`'s `Matching` arm gates on battlefield membership. That gate is
load-bearing for every other card in the pool — without it Bad Moon's "black
creatures get +1/+1" reaches creature cards in graveyards. Painter's Servant is
the first card whose affected set is not battlefield-scoped.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Affected.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Affected.hs`
- Modify: `source/libraries/codec/Pawl/Codec/AffectedSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs` (`affects`, `staticallyMovable`, `projectWith`'s `movableReads`)

**Interfaces:**
- Produces: `Affected.MatchingAnywhere :: Filter.Filter Keyword.Keyword -> Affected`,
  wire tag `"MatchingAnywhere"`.

- [ ] **Step 1: Add the constructor**

In `source/libraries/types/Pawl/Types/Affected.hs`, after `Matching`:

```haskell
  | -- | Matching, without the battlefield gate. Painter's Servant's "all cards
    -- that aren't on the battlefield, spells, and permanents" is the first
    -- affected set in the pool that is not scoped to the battlefield, and the
    -- ONLY reason this is a separate arm rather than a zone payload on Matching:
    -- every other card in the pool depends on that gate, since Bad Moon's "black
    -- creatures get +1/+1" must not reach a creature card in a graveyard.
    --
    -- The set the CR describes is every object in every zone. This reaches the
    -- battlefield and the stack, which are the two zones where a projection
    -- exists (Projection.viewOfObject). A card in a hand, library, graveyard or
    -- exile is matched against its PRINTED characteristics by viewOfCard and is
    -- never reached (#160, #NNN).
    MatchingAnywhere (Filter.Filter Keyword.Keyword)
```

- [ ] **Step 2: Let the build enumerate the sites**

Run: `direnv exec . cabal build all 2>&1 | grep -A3 "Pattern match"`

`Projection.affects` — copy the `Matching` arm's body verbatim, minus the
`Set.member oid (GameState.battlefield gs) &&` conjunct. The `perspective`
laziness note above the `Matching` arm applies here too; do not duplicate that
whole comment, cite it:

```haskell
  -- Matching's body without the battlefield conjunct. The `perspective` laziness
  -- caveat in the Matching arm above applies here unchanged (#197).
  Affected.MatchingAnywhere f ->
    let perspective = controllerOf source gs
     in Filter.matches (Filter.MkContext perspective (Just source)) (viewOfCharacteristics oid partial (controllerOf oid gs) gs) f
```

`Projection.staticallyMovable` — movable for `Matching`'s reason:
```haskell
  Affected.MatchingAnywhere _ -> True
```

`projectWith`'s local `movableReads` — same treatment as `Matching`:
```haskell
                    Affected.MatchingAnywhere f ->
                      let aspects = filterReads f
                       in if Set.null aspects then Nothing else Just aspects
```

`Codec.Affected.toJson`:
```haskell
  Affected.MatchingAnywhere f -> Common.tagged "MatchingAnywhere" . Just $ Filter.toJson Keyword.toJson f
```

`Codec.Affected.fromJson`:
```haskell
    "MatchingAnywhere" -> Common.withValue mv (fmap Affected.MatchingAnywhere . Filter.fromJson Keyword.fromJson)
```

- [ ] **Step 3: Pin the wire format**

Add a `MatchingAnywhere` case to
`source/libraries/codec/Pawl/Codec/AffectedSpec.hs`, copying the shape of that
file's existing `Matching` case exactly.

- [ ] **Step 4: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`
Expected: warning-free, suite green, count up by one (the codec case). No
behaviour change — nothing produces the new arm yet.

```bash
git add source/libraries/types/Pawl/Types/Affected.hs source/libraries/codec/Pawl/Codec/Affected.hs source/libraries/codec/Pawl/Codec/AffectedSpec.hs source/libraries/engine/Pawl/Engine/Projection.hs
direnv exec . hooky fix
git add source/libraries/types/Pawl/Types/Affected.hs source/libraries/codec/Pawl/Codec/Affected.hs source/libraries/codec/Pawl/Codec/AffectedSpec.hs source/libraries/engine/Pawl/Engine/Projection.hs
direnv exec . hooky run
git commit -m "Add an affected set that is not scoped to the battlefield"
```

---

## Task 5: The as-enters colour choice

Painter's Servant's "As this creature enters, choose a color" (CR 614.1c), and
the modification that reads it. Five types plus a `Replacement.hs` arm.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Object.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Object.hs`, `.../ObjectSpec.hs`
- Modify: `source/libraries/types/Pawl/Types/EntryRewrite.hs`
- Modify: `source/libraries/codec/Pawl/Codec/EntryRewrite.hs`, `.../EntryRewriteSpec.hs`
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`
- Modify: `source/libraries/types/Pawl/Types/Response.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Response.hs`, `.../ResponseSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replay.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replacement.hs`
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`, `source/libraries/codec/Pawl/Codec/Modification.hs`, `.../ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`
- Modify: `source/benchmark/Main.hs`, `source/test-suite/Pawl/Support.hs`, `.../GameSpec.hs`, `.../CastSpec.hs`, `.../ReplacementSpec.hs`, `.../ReplaySpec.hs`

**Interfaces:**
- Produces:
  - `Object.chosenColor :: Maybe Color.Color`
  - `EntryRewrite.ChooseColor :: EntryRewrite` (nullary), wire tag `"ChooseColor"`
  - `Prompt.ChooseColor :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Color.Color`
  - `Response.ChoseColor :: Color.Color -> Response`
  - `Modification.AddChosenColor :: Modification` (nullary), wire tag `"AddChosenColor"`

- [ ] **Step 1: Add the object field**

`source/libraries/types/Pawl/Types/Object.hs`, after `attachedTo`:

```haskell
    -- | CR 614.1c: a colour this object's controller chose as it entered
    -- ("As this creature enters, choose a color" -- Painter's Servant). Read by
    -- Modification.AddChosenColor off the effect's SOURCE, never off the affected
    -- object.
    --
    -- NOT a copiable value, unlike the P/T an EntryOption writes into the
    -- copiable snapshot. CR 707.5: "If the text that's being copied includes any
    -- abilities that replace the enters-the-battlefield event (such as ... 'as
    -- [this] enters' abilities), those abilities will take effect" -- so a copy
    -- of Painter's Servant runs the copied ability and makes its OWN choice.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one.
    chosenColor :: Maybe Color.Color,
```

Add `import qualified Pawl.Types.Color as Color` in alphabetical position.

Then find every construction site of `MkObject` and every `changeZone`-style
reset; `-Weverything` flags missing record fields. **`changeZone` must reset this
to `Nothing`** — grep for where `damage`, `counters` and `attachedTo` are reset
together and add it there.

- [ ] **Step 2: Codec the field**

Add `chosenColor` to `Pawl.Codec.Object`'s encoder and decoder, following how
`attachedTo` (also a `Maybe`) is handled there, and extend
`Pawl.Codec.ObjectSpec`'s literal-JSON case.

- [ ] **Step 3: Verify green so far**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`
Expected: warning-free, green. The field exists and round-trips; nothing writes it.

Commit: `git commit -m "Record a colour chosen as an object enters"`

- [ ] **Step 4: Add the prompt, response and replay plumbing**

`Prompt.hs`, after `ChooseEntryOption`:

```haskell
  -- | CR 614.1c: as an object enters, its controller chooses a colour
  -- ("As this creature enters, choose a color" -- Painter's Servant). The
  -- ObjectId is the entering object.
  --
  -- No candidate list: CR 105.1 fixes the five colours and no card in the pool
  -- narrows them. Always asked -- five colours are five distinguishable options,
  -- so there is nothing here the engine may decide for a player.
  ChooseColor :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Color.Color
```

`Response.hs`, after `ChoseEntryOption`:

```haskell
  | -- | CR 614.1c: the colour a player chose as an object entered, serialized so
    -- a DecisionLog replays it deterministically.
    ChoseColor Color.Color
```

`Replay.hs` gains three arms, matching `ChooseEntryOption`'s exactly:

```haskell
  Prompt.ChooseColor {} -> Response.ChoseColor answer
```
```haskell
  Prompt.ChooseColor {} -> case response of
    Response.ChoseColor c -> Just c
    _ -> Nothing
```
```haskell
  -- CR 105.1: any of the five colours is a legal answer, and white is the least
  -- eventful fallback when a transcript runs short.
  Prompt.ChooseColor {} -> Color.White
```

`Codec.Response` and `Codec.ResponseSpec` gain a `"ChoseColor"` case, following
the file's existing shapes.

Then run the build and give every remaining `Prompt` case the new arm — the
benchmark's three, `Support.hs`'s four, `GameSpec.hs`'s two, `CastSpec.hs`'s
three, `ReplacementSpec.hs`'s one. Each of those is a decider returning a default;
return `Color.White` to match Replay's fallback.

`ReplaySpec.hs` has a `"ChooseEntryOption records and replays a Natural"` test;
add its `ChooseColor` twin.

- [ ] **Step 5: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`

```bash
git commit -m "Add the CR 614.1c colour-choice prompt"
```

- [ ] **Step 6: Add the entry rewrite**

`EntryRewrite.hs`, after `ChoiceOf`:

```haskell
  | -- | CR 614.1c's other choosing shape: "As [this permanent] enters, choose a
    -- color" (Painter's Servant). Nullary -- CR 105.1's five colours are the
    -- offer, and no card narrows them, so there is nothing to carry.
    --
    -- Written to Object.chosenColor rather than into the copiable snapshot the
    -- two arms above write to: CR 707.5's second sentence means a copy runs the
    -- copied as-enters ability and makes its own choice, so the colour is not a
    -- copiable value.
    ChooseColor
```

Codec, beside `AsCopy` and `UnderSourceControl`:
```haskell
  EntryRewrite.ChooseColor -> Common.nullary "ChooseColor"
```
```haskell
    ("ChooseColor", _) -> Right EntryRewrite.ChooseColor
```

Add a `ChooseColor` case to `Pawl.Codec.EntryRewriteSpec`.

- [ ] **Step 7: Apply the rewrite**

In `source/libraries/engine/Pawl/Engine/Replacement.hs`, add an arm beside
`EntryRewrite.ChoiceOf` (~line 877). It is simpler than `ChoiceOf` because there
is no options list to be empty and no one-option elision:

```haskell
      -- CR 614.1c: Painter's Servant's "As this creature enters, choose a
      -- color". Unlike ChoiceOf above, this is always asked: CR 105.1's five
      -- colours are always all legal and always distinguishable, so there is no
      -- one-option case to elide.
      --
      -- Written to Object.chosenColor, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseColor.
      EntryRewrite.ChooseColor -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner.
          Nothing -> pure Color.White
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Trans.lift (Program.prompt (Prompt.ChooseColor decider controller oid))
        consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenColor = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
```

Also give the new arm its place in `Replacement.hs`'s two other total cases over
`EntryRewrite` (~lines 597 and 651) — `ReplacementBucket.Other` and `False`
respectively, matching `ChoiceOf`.

- [ ] **Step 8: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`

```bash
git commit -m "Apply the CR 614.1c as-enters colour choice"
```

- [ ] **Step 9: Add `Modification.AddChosenColor`**

`Modification.hs`, after `AddColor`:

```haskell
  | -- | layer 5, CR 613.1e / 105.3: this object gains, IN ADDITION to its other
    -- colours, the colour chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenColor). Painter's Servant's "the chosen color".
    --
    -- Payload-free because the colour is DERIVED at projection time from the
    -- source rather than baked into card data -- the same posture
    -- SetControllerToSource takes toward CR 109.5's "you", and two constructors
    -- for the same reason that AddColor above carries a literal set and this
    -- does not. A static ability's modification is card data and cannot name a
    -- colour a player will choose.
    AddChosenColor
```

`Projection.layer`:
```haskell
  Modification.AddChosenColor -> Layer.Color
```

`Projection.applyModification`, beside the `AddColor` arm — note it reads `src`,
the effect's source, not `oid`:
```haskell
        -- CR 105.3's parenthetical again, with the colour read off the SOURCE's
        -- own entry choice. An unchosen source (malformed data, or an entry that
        -- never ran the rewrite) adds nothing rather than guessing a colour.
        Modification.AddChosenColor ->
          case Game.lookupObject src gs >>= Object.chosenColor of
            Nothing -> pc
            Just c -> pc {PC.colors = Set.insert c (PC.colors pc)}
```

`Projection.freezeQuantities`: `Modification.AddChosenColor -> Just m`.
`Projection.removesAbilities`: `Modification.AddChosenColor -> False`.
`Projection.modificationWrites`: `Modification.AddChosenColor -> Set.singleton Colors`.
`Codec.Modification`: `Common.nullary "AddChosenColor"` / `Right Modification.AddChosenColor`.
Add a `ModificationSpec` case for it.

- [ ] **Step 10: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`

```bash
git commit -m "Add the modification that reads a source's chosen colour"
```

---

## Task 6: Painter's Servant, and the gate

**Files:**
- Create: `data/cards/painters-servant.json`
- Modify: `source/test-suite/Pawl/ColorSpec.hs`

**Interfaces:**
- Consumes: everything from Tasks 1–5.

- [ ] **Step 1: Write the card**

Create `data/cards/painters-servant.json` — `{2}`, Artifact Creature —
Scarecrow, 1/3, "As this creature enters, choose a color. All cards that aren't
on the battlefield, spells, and permanents are the chosen color in addition to
their other colors."

- `"replacementEffects"`: one `EntryR` whose filter is `IsSource` and whose
  rewrite is `ChooseColor` — `data/cards/primal-plasma.json`'s shape with the
  rewrite swapped.
- `"staticAbilities"`: one entry, `affected` = `MatchingAnywhere` over the filter
  `{"type":"And","value":[]}` (every object), `modifications` =
  `[{"type":"AddChosenColor"}]`. `data/cards/bad-moon.json` has the static-ability
  shape.
- `"typeLine"`: types `Artifact` and `Creature`, subtype `Scarecrow`. **Check
  `Subtype.Scarecrow` exists**; if not, add it with the full edit-site set.

- [ ] **Step 2: Write the gate test**

Append to `source/test-suite/Pawl/ColorSpec.hs`. This is spec §4's T1 — the test
that discriminates CR 613.3 from CR 613.7.

```haskell
  Spec.it s "CR 613.3 devoid beats an OLDER layer-5 'in addition' effect" $ do
    -- THE GATE for #35. Painter's Servant enters first and names blue, so its
    -- continuous effect carries its source's timestamp (CR 613.7a) and the
    -- drone's is minted later, on entry (CR 613.7d). Pure CR 613.7 timestamp
    -- order would therefore add blue FIRST and apply devoid SECOND, leaving the
    -- drone colourless. CR 613.3 puts the characteristic-defining ability first
    -- within layer 5, so the drone is blue -- and blue, not black, so nothing
    -- can be confused with its printed {B}.
    paintersServant <- S.printingOf s registry "Painter's Servant"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withPainter) = S.addCreature paintersServant S.alice gs0
        (droneId, gs) = S.addCreature slaughterDrone S.alice withPainter
    Spec.assertEqWith s "blue, not colourless" (Projection.colorsOf droneId gs) $ Set.singleton Color.Blue
```

`S.addCreature` puts a permanent on the battlefield directly and so will **not**
run the entry replacement that makes the choice. Two ways forward, in order of
preference:

1. Cast Painter's Servant through the stack so the entry loop runs and the
   `ChooseColor` prompt is answered by the spec's decider. `ColorSpec.hs`'s
   Dragon Fodder test already explains why a real cast is needed for a card whose
   behaviour depends on the entry path, and `Pawl.ReplacementSpec` shows how to
   answer an entry prompt from a decider (see its `ChooseEntryOption -> which`
   decider at line ~196). Use blue as the answer.
2. If that proves impractical, set `Object.chosenColor` on the placed permanent
   directly and say so in a comment — but then add a *separate* test that casts
   Painter's Servant and asserts the prompt was raised, so the entry path is not
   left unproven.

- [ ] **Step 3: Run it and watch it fail before the card exists**

Do Step 2 before Step 1 if you prefer strict TDD; either way, confirm you have
seen this test fail for the right reason — a missing card, then a missing choice
— before you see it pass.

Run: `direnv exec . cabal test 2>&1 | grep -A6 "OLDER layer-5"`

- [ ] **Step 4: Write the stack test**

Spec §4's T2 — the half that needs `MatchingAnywhere`.

Add a `TargetSpec` for Red Elemental Blast's first mode beside the file's
existing `nonblackCreature` binding at the top of the module:

```haskell
-- "target blue spell", Red Elemental Blast's first mode.
blueSpell :: TargetSpec.TargetSpec
blueSpell = TargetSpec.MkTargetSpec Pool.Spells (Just (Filter.Type.HasColor Color.Blue))
```

Then the test. `S.spellOnStack` places the object in the Stack zone directly; its
empty-bindings caveat (see the Dragon Fodder test's comment) does not bite here,
because Slaughter Drone's spell has one mode with no effects and nothing is being
resolved.

```haskell
  Spec.it s "CR 604.3 Painter's Servant colours a devoid SPELL on the stack" $ do
    -- Painter's set is "all cards that aren't on the battlefield, spells, and
    -- permanents", so it is not scoped to the battlefield (Affected.MatchingAnywhere).
    -- CR 604.3 makes devoid function in all zones, so the spell is colourless
    -- first and blue second -- and Red Elemental Blast's "target blue spell" can
    -- name it.
    paintersServant <- S.printingOf s registry "Painter's Servant"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withPainter) = S.addCreature paintersServant S.alice gs0
        (spellId, gs) = S.spellOnStack slaughterDrone S.alice withPainter
        legal = Target.legalRecipients Nothing S.noSource blueSpell gs
    Spec.assertEqWith s "the spell is blue" (Projection.colorsOf spellId gs) $ Set.singleton Color.Blue
    Spec.assertBool s (Set.member (Recipient.ToObject spellId) legal) "and a legal blue-spell target"
```

**Verify `Recipient.ToObject` is the right constructor for a spell** — `Pool.hs`'s
`SpellsAndPermanents` comment names `ToObject`, but check `Recipient.hs` and how
`Pool.Spells` tags its candidates rather than trusting that. This test shares
Step 2's dependency on Painter's Servant having actually made its choice, so
whichever of Step 2's two routes you took applies here unchanged.

- [ ] **Step 5: Verify green and commit**

Run: `direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20`

```bash
git add data/cards/painters-servant.json source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky fix
git add data/cards/painters-servant.json source/test-suite/Pawl/ColorSpec.hs
direnv exec . hooky run
git commit -m "Gate CR 613.3 with Painter's Servant"
```

---

## Task 7: Issues, citations, and the PR

- [ ] **Step 1: File the two deferral issues**

Both bodies come from the spec's §6. Use `gh issue create`; do not set a priority
label — those are the owner's.

Issue A, labels `gap` and `expires:card-driven`:
> **CR 604.3a(2): a layer-6 GRANTED devoid does nothing to colour**
>
> `Projection.applyColorDefining` folds devoid as a characteristic-defining
> ability at the start of layer 5, reading the partial projection's keywords —
> which by construction cannot hold a layer-6 grant. That is correct for a CDA
> (CR 604.3a(2) requires printed, token-granted, or copy/text-change-acquired),
> but a granted devoid is still the static ability "This object is colorless"
> (CR 702.114a) and should apply as an ordinary layer-5 colour effect timestamped
> when it was granted (CR 613.7a). Today it is silently nothing.
>
> **No card grants devoid.** A Scryfall sweep on 2026-08-03 for cards naming
> devoid without having the keyword returns one card, Corrupted Crossroads, whose
> clause is a mana restriction. Building this would be a capability no card
> exercises.
>
> **Sites:** `Pawl/Engine/Projection.hs` (`applyColorDefining`),
> `Pawl/Types/Keyword.hs` (the `Devoid` note).
>
> **Expiry trigger — card-driven.** The first card that grants devoid.

Issue B, labels `gap` and `elision`:
> **Painter's Servant does not colour cards in hidden zones**
>
> `Affected.MatchingAnywhere` reaches the battlefield and the stack, the two
> zones where a projection exists. "All cards that aren't on the battlefield" also
> covers hands, libraries, graveyards and exile, where `viewUpTo` has no
> projection and falls back to `viewOfCard`'s printed characteristics (#160).
>
> **Sites:** `Pawl/Types/Affected.hs` (`MatchingAnywhere`),
> `Pawl/Engine/Projection.hs` (`viewOfCard`).
>
> **Expiry trigger:** the off-battlefield projection sweep, #160.

- [ ] **Step 2: Substitute the issue numbers**

Replace every `#NNN` placeholder with the real numbers:
`Pawl/Engine/Projection.hs` (`applyColorDefining`'s "Not implemented" note),
`Pawl/Types/Keyword.hs`, `Pawl/Types/Affected.hs`.

```bash
grep -rn '#NNN' source/ docs/
```
Expected after the substitution: no matches.

- [ ] **Step 3: Self-review the branch**

Per `CLAUDE.md`, and scaled to a diff this size:

1. **Re-check every CR citation this branch added or touched against
   `docs/rules.txt`.** The ones this plan wrote: 105.1, 105.3, 109.5, 202.2,
   204.2, 604.3, 604.3a, 613.1e, 613.3, 613.7a, 613.7d, 614.1c, 702.114a, 160's
   neighbours. Do not trust this plan's rendering of them.
2. **Re-read every comment the branch touched** for prose the rewrite made wrong.
   Known candidates: `viewOfCard`'s header ("colours from baseColorsOf"),
   `projectFrom`'s "Layer 7a is ALWAYS in the layer list", `ColorSpec.hs`'s module
   header (its covers-list should name the new cards), `Modification.hs`'s
   `SetColor` and `SetCreatureSubtype` comments, `Layer.hs`'s producers list
   (Color's producers are no longer just P3a's).
3. Confirm the diff does **not** make the rules core case on an effect's
   *identity*. `Projection` casing on `Modification` is the sanctioned exception;
   anything else is a violation.

Fix findings on the branch.

- [ ] **Step 4: Final verification**

```bash
direnv exec . cabal build all 2>&1 | tail -20
direnv exec . cabal test 2>&1 | tail -20
```
Record the suite count before → after for the PR body.

- [ ] **Step 5: Open the PR**

Open as a draft, then mark ready once Step 3's findings are pushed and the suite
is green. The body carries the case for merging: what changed and why with
`Closes #35` as **bare text, never in backticks**; the CR citations, each checked
against `rules.txt`; the design calls and the alternatives rejected (spec §5);
verification (build warning-free, `hooky run` clean, suite count before → after,
and the proving test); an explicit "no" on the rules-core-cases-on-identity
question; and what was deferred, with the two new issue numbers.

Note in the body that #35's **second bullet** — a below-layer-5 effect with a
colour-keyed affected set — is fixed by this branch but has no card end-to-end,
and is pinned by the `projectUpTo Layer.Color` assertion instead. A
correct-but-unexercised path is not a deficiency, so it gets no issue.

Do not wait for CI. Do not start the next unit.
