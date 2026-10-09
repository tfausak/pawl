# Stickers unit 1: tickets, sheets and art stickers — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land unit 1 of the stickers spec: ticket counters (CR 107.17), sticker sheet data and CR 103.2d's sheet draw, `Object.stickers` with CR 123.5 retention and CR 613.7k stamps, placement of art stickers (CR 123.3, 123.9), the stickered filters (CR 123.4) and the placement event. Proved by Blorbian Buddy, Ticket Turbotubes, Proficient Pyrodancer, Wee Champion and Croakid Amphibonaut. It does not close #872.

**Architecture:** A player brings sheets in `Deck.stickerSheets`; `Setup.createDeck` copies them to `Player.stickerSheets`, and a new `Setup.drawStickerSheets` step writes `Player.chosenStickerSheets` (three drawn through `Prompt.RandomStickerSheet` when more than three were brought). A sticker is a `StickerRef` (owner, sheet position, kind, index within kind). `Object.stickers :: Seq StickerPlacement` is cleared by `Object.newIncarnation` and written back by `Event.changeZoneWithCause`'s `mkObj` when the destination is public, then restamped (CR 613.7k). Availability is computed in a new `Pawl.Engine.Sticker`. One open-half opcode, `Effect.PutSticker`, places through `Prompt.ChooseSticker`; `GameEvent.StickerPut` and `TriggerCondition.PlacesSticker` carry the event. `Filter.HasSticker` and `Filter.Stickered` read a new `Filter.View` field. Sheets are JSON under `data/sticker-sheets/`, loaded by a new registry module `Pawl.StickerSheets`.

**Tech Stack:** Haskell (GHC via the repo's nix flake), cabal, tasty.
**Spec:** docs/superpowers/specs/2026-10-09-stickers-design.md

## Global Constraints

- Work only in `/Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers` (branch `872-stickers-unit-1`). Never `cd` to the primary checkout.
- Before the first build, as a bare command: `/Volumes/nvme/Developer/pawl/script/warm-worktree.sh seed /Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers`. It copies `cabal.project.local` too; confirm the file is there.
- `$SCRATCH` is the session scratchpad. Prefix every `cabal` call with `script/with-build-lock.sh`, redirect to `$SCRATCH/<name>.log`, read the file. Never pipe `cabal`.
- Build: `script/with-build-lock.sh cabal build -v0 all > "$SCRATCH/build.log" 2>&1`.
- Subtree: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes -p Sticker' > "$SCRATCH/sticker.log" 2>&1`. A `-p` pattern holding a space is mangled by `--test-options`; every pattern below is one word. For one that must hold a space, run the built binary (`find dist-newstyle -name pawl-test-suite -type f -perm -u+x`) with `pawl_datadir=$PWD/data`.
- Full suite before each commit: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes' > "$SCRATCH/suite.log" 2>&1`. Record the count before Task 1.
- Mutations: write the sed script to `$SCRATCH/mN.sed`, run `script/with-build-lock.sh script/mutate.sh FILE @$SCRATCH/mN.sed PATTERN`, one at a time, alone in the checkout. Mutate toward less permissive unless a step says why not. Name the assertion that reddened and confirm it is the gameplay one.
- The rules core never cases on an effect's identity. `Effect.PutSticker` is cased only in `Pawl.Engine.Resolve*` and the exhaustive classification tables. The core cases on `StickerKind` (CR 123.1's classification), never on a sheet or a sticker's text.
- One type per `Pawl.Types.<TypeName>` module, type and instances only. Constructors take `Mk`. A haddock is one line plus its CR citation, except an elision paragraph or a note naming the proving test.
- An elision gets an issue and a code-site comment saying only what is not implemented, ending `(#N)`. Unit-4 work (meld, merge, sticker kicker) cites `(#872)`, which stays open.
- Read CR text from `docs/rules.txt` by rule number. Re-verify Oracle with `curl -s 'https://api.scryfall.com/cards/named?exact=<Name>'` before writing a card.
- Stage, `hooky fix`, stage again. A new module: stage `pawl.cabal`, run `cabal-gild pawl.cabal`. Card and sheet JSON: `script/format-json.sh fix FILE`.
- Never stash; copy aside and move back. Never `git checkout <file>` to revert.
- Extensions only from `.hlint.yaml`. `case` over `maybe`, `let` over `where`, no backticks in new code.
- Commit: a subject, at most two sentences of why, then `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Never put close, fix or resolve beside #872; write "related to #872".

## Review Focus

These are the five input classes most likely to bite. Each has a test in its owning task.

1. **Sheet counts at setup.** Four sheets draw three, one at a time over the undrawn; three keep all, unasked; none plays without stickers. Tests: Task 4, "CR 103.2d four sheets: three are drawn at random, one at a time", "CR 103.2d/123.2b three sheets are all kept, unasked"; Task 7, "CR 608.2d with no sheets Pyrodancer's may is not offered".
2. **A new game reruns the draw and clears every sticker.** CR 727.2 rebuilds every card through `newIncarnation`, and the rebuilt game must ask CR 103.2d again. Test: Task 5, "CR 727.2/103.2d a restart draws the sheets again and every sticker comes off".
3. **Public against hidden, and the restamp.** Graveyard keeps the sticker, restamped after the card's new timestamp; hand drops it. Test: Task 5, "CR 123.5/613.7k an art sticker stays through a death, restamped, and comes off in a bounce".
4. **Availability is computed.** A used sticker is not offered; a second copy of a sheet is its own stickers; a hidden-zone move frees a sticker. Test: Task 7, "CR 123.3/123.3a a used sticker is not offered again until its card reaches a hidden zone".
5. **Not copiable.** A Clone of a stickered permanent is not stickered, so Croakid stops flying when the original goes. Test: Task 6, "CR 123.1 a Clone of a stickered permanent is not stickered".

## Divergences from the spec (decided here; the PR body repeats them)

1. **Two Player fields, not one.** A restart (CR 727.2) or subgame (CR 729.2) reruns CR 103.2d with no `Deck` in hand, so the player must keep what they brought. `Player.stickerSheets` is the brought sheets (as `Player.dungeons` keeps dungeon cards); `Player.chosenStickerSheets :: Set Natural` is the drawn positions. `StickerRef.sheet` indexes the brought sheets, so duplicates stay distinct (CR 123.3a).
2. **`StickerRef`'s "index on the sheet" is a kind plus an index within the kind.** Same identity, and the kind is then read without a sheet lookup.
3. **No `optional`, ticket-cap or paid field on `Effect.PutSticker`.** The printed "may" is `Clause.optionality` (CR 603.5), which `Resolve.exercises` already asks with CR 608.2d's impossibility gate; a second flag would be two encodings of one question. The cap and the payment have no unit-1 reader (art stickers cost nothing) and land in unit 3 with Pin Collection, per CLAUDE.md's no-capability-without-a-card rule.
4. **No name-position field on `StickerPlacement`.** Only art stickers are placeable in unit 1, so nothing writes it; unit 2 adds it with its writer.
5. **The View builders.** `copiableCharacteristics` builds a `ProjectedCharacteristics`, not a `View`; stickers are not in `ProjectedCharacteristics`, so CR 123.1's "not copiable" holds there by construction and that function is untouched. The four `MkView` builders are `Filter`'s player view, `Projection.View.viewOfCard`, `viewOfCharacteristics` and `Count.viewOfSnapshot`.
6. **Wee Champion is two triggered abilities split by kind.** No Condition reads the triggering event's sticker kind. `TriggerCondition.PlacesSticker` carries a kind set, as `PermanentGetsCounters` carries a counter kind; the non-art and art triggers are disjoint, so each placement triggers exactly one ability with the printed result. Wee Champion over Goblin Airbrusher because a +1/+1 counter against an until-end-of-turn pump separates the two branches in one P/T read.
7. **The draw prompt is `Prompt.RandomStickerSheet`, asked once per sheet over the undrawn positions.** `RandomObject`'s shape: a sheet is neither an object nor a card name.
8. **The setup step runs after `Companion.reveal`, before `anteFromLibraries`.** CR 103.2d follows 103.2b; CR 407.2 is not part of 103.2.
9. **The mkObj the spec names lives in `Event.changeZoneWithCause`** (`changeZoneAttaching = changeZoneWithCause Nothing`). CR 613.7k's restamp is made there, after `placeObject`; restamps for a timestamp renewed without a zone change (CR 613.7e–g, `Restamp.reassign`) are unobservable for art stickers and are elided citing `(#872)`.
10. **CR 123.3b's gate in the opcode has no unit-1 observer.** Pyrodancer's "you own" filter narrows first. The line stays, marked a regression fence at the site and in the PR.
11. **Meld and merge drop stickers in unit 1** (component split, `Event.meld`), elided citing `(#872)`, which unit 4 closes.
12. **Gameplay tests are Haskell specs with test-local answerers**, not scenarios: the harness has no sticker-sheet vocabulary and `ChooseSticker` is the subject.

---

## Task 1: ticket counters, Blorbian Buddy and Ticket Turbotubes

**Files:**
- Modify: `source/libraries/types/Pawl/Types/PlayerCounterKind.hs`, `source/libraries/codec/Pawl/Codec/PlayerCounterKindSpec.hs`
- Modify: `data/cards/aetheric-amplifier.json`
- Create: `data/cards/blorbian-buddy.json`, `data/cards/ticket-turbotubes.json`
- Create: `source/libraries/test/Pawl/StickerSpec.hs`; Modify: `source/libraries/test/Pawl/Test.hs`, `pawl.cabal`

**Interfaces:**
- Produces: `PlayerCounterKind.Ticket :: PlayerCounterKind`; `Pawl.StickerSpec.spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()`; spec helper `mainPhaseForAlice :: GameState.GameState -> GameState.GameState`.
- Consumes: `Activatable.abilitiesFor`, `Activate.activateAbility`, `Stack.resolveTop`, `S.playerCounterOf`, `S.addPlayerCounter`.

- [ ] **Step 1: Verify Oracle.** Expected (Scryfall, 2026-10-09):
  - Blorbian Buddy `{G}` Creature — Alien Guest 1/1: "Trample\n{G}, {T}: You get {TK} (a ticket counter)."
  - Ticket Turbotubes `{3}` Artifact: "{T}: Add one mana of any color.\n{3}, {T}: You get {TK} (a ticket counter)."

- [ ] **Step 2: Write the failing tests.** Create `source/libraries/test/Pawl/StickerSpec.hs`:

```haskell
{-# LANGUAGE GADTs #-}

-- Covers CR 107.17's ticket counters, CR 103.2d's sheet draw
-- (Pawl.Engine.Setup), and CR 123's stickers: Pawl.Engine.Sticker,
-- Effect.PutSticker (Pawl.Engine.Resolve.Effect), the CR 123.5 write-back in
-- Pawl.Engine.Event, Filter.HasSticker and Filter.Stickered, and
-- TriggerCondition.PlacesSticker.
module Pawl.StickerSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Prompt as Prompt

-- Alice active with priority in her precombat main phase.
mainPhaseForAlice :: GameState.GameState -> GameState.GameState
mainPhaseForAlice gs = gs {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

-- Aetheric Amplifier's second mode, "each kind of counter you have".
secondMode :: Prompt.Prompt r -> r
secondMode p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex 1)
  _ -> S.identityAnswer p

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Sticker" $ do
  Spec.it s "CR 107.17 Blorbian Buddy's {G}, {T} gets alice a ticket counter" $ do
    buddy <- S.printingOf s registry "Blorbian Buddy"
    forest <- S.printingOf s registry "Forest"
    let (buddyId, board) = S.addPermanent buddy S.alice (mainPhaseForAlice (S.landsFor forest S.alice 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor buddyId board of
          [ability] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice buddyId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  Spec.it s "CR 107.17 Ticket Turbotubes' {3}, {T} gets alice a ticket counter" $ do
    tubes <- S.printingOf s registry "Ticket Turbotubes"
    mountain <- S.printingOf s registry "Mountain"
    let (tubesId, board) = S.addPermanent tubes S.alice (mainPhaseForAlice (S.landsFor mountain S.alice 3 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor tubesId board of
          [_, tickets] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice tubesId tickets >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  -- Three, so a doubling reads six and a no-op three.
  Spec.it s "CR 701.10e Aetheric Amplifier doubles alice's ticket counters" $ do
    amplifier <- S.printingOf s registry "Aetheric Amplifier"
    mountain <- S.printingOf s registry "Mountain"
    let (ampId, board) = S.addPermanent amplifier S.alice (mainPhaseForAlice (S.addPlayerCounter PlayerCounterKind.Ticket 3 S.alice (S.landsFor mountain S.alice 4 (Setup.gameWith GameSettings.plain S.bothPlayers))))
        after = case Activatable.abilitiesFor ampId board of
          [_, doubling] -> S.runPure secondMode board (Activate.activateAbility S.alice ampId doubling >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 701.10e alice has six ticket counters" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 6
```

Read `Activatable.abilitiesFor`'s haddock to confirm it lists a mana ability among the printed ones in printed order; if not, Turbotubes' and Amplifier's patterns become `[tickets]`/`[doubling]`. Wire it into `Pawl.Test` (`import qualified Pawl.StickerSpec`, `Pawl.StickerSpec.spec s registry` beside `Pawl.AnteSpec`), stage `pawl.cabal`, run `cabal-gild pawl.cabal`.

In `PlayerCounterKindSpec`, after "Energy":

```haskell
  Spec.it s "Ticket" $
    Common.assertCodec
      s
      PlayerCounterKind.codec
      PlayerCounterKind.Ticket
      " {\"type\":\"Ticket\"} "
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'PlayerCounterKind.Ticket'".

- [ ] **Step 4: Implement.** In `PlayerCounterKind.hs`, between `Energy` and `Poison`:

```haskell
  = Energy -- CR 107.14
  | Ticket -- CR 107.17
  | Poison -- CR 122.1f
```

`Pawl.Codec.PlayerCounterKind` is `Arm.enum`, so the tag is derived. No exhaustive `case` over the type exists (`grep -rn 'PlayerCounterKind.Rad\b' source --include='*.hs'` hits only `Pawl.Engine.Rad`'s lookups); record that in the PR.

`data/cards/aetheric-amplifier.json`: per the type's haddock, add after the Energy `GainPlayerCounters` in the second mode:

```json
{
  "type": "GainPlayerCounters",
  "value": {
    "kind": { "type": "Ticket" },
    "player": { "type": "Relative", "value": { "type": "You" } },
    "quantity": { "type": "PlayerCounters", "value": { "kind": { "type": "Ticket" }, "player": { "type": "Relative", "value": { "type": "You" } } } }
  }
}
```

`data/cards/blorbian-buddy.json` (Defiant Elf's keyword shape, Aether Hub's `GainPlayerCounters`):

```json
{
  "faces": [
    {
      "activatedAbilities": [
        {
          "cost": {
            "components": [ { "type": "TapThis" } ],
            "mana": [ { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Green" } } } ]
          },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "GainPlayerCounters",
                        "value": {
                          "kind": { "type": "Ticket" },
                          "player": { "type": "Relative", "value": { "type": "You" } },
                          "quantity": { "type": "Literal", "value": 1 }
                        }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        }
      ],
      "keywords": [ { "type": "Trample" } ],
      "manaCost": [ { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Green" } } } ],
      "name": "Blorbian Buddy",
      "oracleText": "Trample\n{G}, {T}: You get {TK} (a ticket counter).",
      "power": { "type": "Literal", "value": 1 },
      "toughness": { "type": "Literal", "value": 1 },
      "typeLine": { "subtypes": [ { "type": "Alien" }, { "type": "Guest" } ], "types": [ { "type": "Creature" } ] }
    }
  ]
}
```

`data/cards/ticket-turbotubes.json` (Aetheric Amplifier's mana ability):

```json
{
  "faces": [
    {
      "activatedAbilities": [
        {
          "cost": { "components": [ { "type": "TapThis" } ], "mana": [] },
          "modal": { "modes": [ { "clauses": [ { "effects": [ { "type": "AddMana", "value": { "production": { "type": "AnyColor" } } } ] } ] } ] }
        },
        {
          "cost": { "components": [ { "type": "TapThis" } ], "mana": [ { "type": "Generic", "value": 3 } ] },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "GainPlayerCounters",
                        "value": {
                          "kind": { "type": "Ticket" },
                          "player": { "type": "Relative", "value": { "type": "You" } },
                          "quantity": { "type": "Literal", "value": 1 }
                        }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        }
      ],
      "manaCost": [ { "type": "Generic", "value": 3 } ],
      "name": "Ticket Turbotubes",
      "oracleText": "{T}: Add one mana of any color.\n{3}, {T}: You get {TK} (a ticket counter).",
      "typeLine": { "types": [ { "type": "Artifact" } ] }
    }
  ]
}
```

Run `script/format-json.sh fix` on all three cards.

- [ ] **Step 5: Run** `-p Sticker`, then `-p PlayerCounterKind`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.** `m1.sed` on `data/cards/blorbian-buddy.json`: `s/"type": "Ticket"/"type": "Energy"/`, pattern `Sticker`. Expected red: "CR 107.17 alice has one ticket counter" (Buddy's case). `m1b.sed` on `data/cards/aetheric-amplifier.json`: delete the Ticket entry's kind line by turning it to Energy, `0,/"type": "Ticket"/s/"type": "Ticket"/"type": "Energy"/`. Expected red: "CR 701.10e alice has six ticket counters".

- [ ] **Step 7: Commit.** Subject: "Add ticket counters, Blorbian Buddy and Ticket Turbotubes (related to #872)".

---

## Task 2: sticker vocabulary types and codecs

**Files:**
- Create types: `source/libraries/types/Pawl/Types/{StickerKind,StickerRef,StickerPlacement,AbilitySticker,PowerToughnessSticker,StickerSheet}.hs`
- Create codecs: `source/libraries/codec/Pawl/Codec/{StickerKind,StickerRef,StickerPlacement,AbilitySticker,PowerToughnessSticker,StickerSheet}.hs`
- Create specs: `source/libraries/codec/Pawl/Codec/StickerKindSpec.hs`, `source/libraries/codec/Pawl/Codec/StickerSheetSpec.hs`; Modify `Pawl.Test`, `pawl.cabal`

**Interfaces:**
- Produces: `StickerKind = Name | Ability | PowerToughness | Art`; `StickerRef.MkStickerRef {owner :: PlayerId, sheet :: Natural, kind :: StickerKind, index :: Natural}`; `StickerPlacement.MkStickerPlacement {sticker :: StickerRef, timestamp :: Timestamp}`; `AbilitySticker.MkAbilitySticker {tickets :: Natural, keywords :: Map Keyword Natural, abilities :: [GrantedAbility Card]}`; `PowerToughnessSticker.MkPowerToughnessSticker {tickets :: Natural, power :: Integer, toughness :: Integer}`; `StickerSheet.MkStickerSheet {number, name, names, art, abilities, powerToughness, oracleText}`; one `codec` per module.

- [ ] **Step 1: Write the failing codec tests.** `StickerKindSpec.hs`:

```haskell
module Pawl.Codec.StickerKindSpec where

import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.StickerKind as StickerKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StickerKind" $ do
  Spec.it s "Art" $ Common.assertCodec s StickerKind.codec StickerKind.Art " {\"type\":\"Art\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s StickerKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s StickerKind.codec
```

`StickerSheetSpec.hs` (decode, then round trip, so the test does not pin key order):

```haskell
module Pawl.Codec.StickerSheetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.StickerSheet as StickerSheet
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.Types.StickerSheet as StickerSheet

-- CR 123.2: Ancestral Hot Dog Minotaur, whose middle name sticker is two words.
minotaur :: StickerSheet.StickerSheet
minotaur =
  StickerSheet.MkStickerSheet
    { StickerSheet.number = 23,
      StickerSheet.name = Text.pack "Ancestral Hot Dog Minotaur",
      StickerSheet.names = Seq.fromList (fmap Text.pack ["Ancestral", "Hot Dog", "Minotaur"]),
      StickerSheet.art = 3,
      StickerSheet.abilities =
        Seq.fromList
          [ AbilitySticker.MkAbilitySticker {AbilitySticker.tickets = 2, AbilitySticker.keywords = Map.singleton (Keyword.Afflict 2) 1, AbilitySticker.abilities = []},
            AbilitySticker.MkAbilitySticker {AbilitySticker.tickets = 3, AbilitySticker.keywords = Map.singleton Keyword.Flying 1, AbilitySticker.abilities = []}
          ],
      StickerSheet.powerToughness =
        Seq.fromList
          [ PowerToughnessSticker.MkPowerToughnessSticker {PowerToughnessSticker.tickets = 2, PowerToughnessSticker.power = 1, PowerToughnessSticker.toughness = 4},
            PowerToughnessSticker.MkPowerToughnessSticker {PowerToughnessSticker.tickets = 5, PowerToughnessSticker.power = 8, PowerToughnessSticker.toughness = 6}
          ],
      StickerSheet.oracleText = Nothing
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StickerSheet" $ do
  Spec.it s "decodes a sheet" $ do
    v <- Common.assertJson s " {\"number\":23,\"name\":\"Ancestral Hot Dog Minotaur\",\"names\":[\"Ancestral\",\"Hot Dog\",\"Minotaur\"],\"art\":3,\"abilities\":[{\"tickets\":2,\"keywords\":[{\"type\":\"Afflict\",\"value\":2}]},{\"tickets\":3,\"keywords\":[{\"type\":\"Flying\"}]}],\"powerToughness\":[{\"tickets\":2,\"power\":1,\"toughness\":4},{\"tickets\":5,\"power\":8,\"toughness\":6}]} "
    Spec.assertEq s (Codec.decode StickerSheet.codec v) (Right minotaur)
  Spec.it s "round trips" $
    Spec.assertEq s (Codec.decode StickerSheet.codec (Codec.encode StickerSheet.codec minotaur)) (Right minotaur)
  Spec.it s "has a schema" $ Common.assertHasSchema s StickerSheet.codec
```

Check `Codec.decode`'s argument order against `Pawl.Registry.parseCard` (`Codec.decode Card.codec`) and that `Common.assertJson` returns the parsed value (FaceSpec's "an ante card decodes its flag"). Wire both specs into `Pawl.Test`.

- [ ] **Step 2: Run; expected failure** "Could not find module 'Pawl.Codec.StickerKind'".

- [ ] **Step 3: Write the types.**

`Pawl/Types/StickerKind.hs`:

```haskell
module Pawl.Types.StickerKind where

-- | CR 123.1: the four kinds of sticker. A classification the closed half
-- cases on, CounterKind's standing; Ord is load-bearing (Set keys).
data StickerKind
  = -- | CR 123.6.
    Name
  | -- | CR 123.7.
    Ability
  | -- | CR 123.8.
    PowerToughness
  | -- | CR 123.9.
    Art
  deriving (Bounded, Enum, Eq, Ord, Show)
```

`Pawl/Types/StickerRef.hs`:

```haskell
module Pawl.Types.StickerRef where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3a: one sticker, by where it is printed, never by its text.
data StickerRef = MkStickerRef
  { -- | CR 123.3: the player whose sheets it is on.
    owner :: PlayerId.PlayerId,
    -- | CR 123.3a: the sheet's position in that player's Player.stickerSheets.
    sheet :: Natural.Natural,
    kind :: StickerKind.StickerKind,
    -- | Which sticker of that kind on the sheet, from 0.
    index :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
```

`Pawl/Types/StickerPlacement.hs`:

```haskell
module Pawl.Types.StickerPlacement where

import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 123.1: a sticker on an object.
data StickerPlacement = MkStickerPlacement
  { sticker :: StickerRef.StickerRef,
    -- | CR 613.7k.
    timestamp :: Timestamp.Timestamp
  }
  deriving (Eq, Ord, Show)
```

`Pawl/Types/AbilitySticker.hs`:

```haskell
module Pawl.Types.AbilitySticker where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword

-- | CR 123.7: an ability sticker, its abilities in the card DSL.
data AbilitySticker = MkAbilitySticker
  { -- | CR 123.3c / 107.17a.
    tickets :: Natural.Natural,
    -- | CR 702: Face.keywords' shape.
    keywords :: Map.Map Keyword.Keyword Natural.Natural,
    -- | CR 113.3: every other printed ability.
    abilities :: [GrantedAbility.GrantedAbility Card.Card]
  }
  deriving (Eq, Ord, Show)
```

`Pawl/Types/PowerToughnessSticker.hs`:

```haskell
module Pawl.Types.PowerToughnessSticker where

import qualified Numeric.Natural as Natural

-- | CR 123.8: a power and toughness sticker.
data PowerToughnessSticker = MkPowerToughnessSticker
  { -- | CR 123.3c / 107.17a.
    tickets :: Natural.Natural,
    power :: Integer,
    toughness :: Integer
  }
  deriving (Eq, Ord, Show)
```

`Pawl/Types/StickerSheet.hs`:

```haskell
module Pawl.Types.StickerSheet where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker

-- | CR 123.2: one sticker sheet. Not a card; no characteristics.
data StickerSheet = MkStickerSheet
  { -- | Its collector number in Scryfall's set sunf.
    number :: Natural.Natural,
    name :: Text.Text,
    -- | CR 123.6: the name stickers' words, one entry per sticker.
    names :: Seq.Seq Text.Text,
    -- | CR 123.9: how many art stickers; they carry no data.
    art :: Natural.Natural,
    abilities :: Seq.Seq AbilitySticker.AbilitySticker,
    powerToughness :: Seq.Seq PowerToughnessSticker.PowerToughnessSticker,
    -- | MTGJSON's text, which Pawl.StickerSpec's lint ties the fields above to.
    oracleText :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Write the codecs.** `StickerKind`: `codec = Arm.enum` (PlayerCounterKind's module, renamed). The rest are `Fields.object` with `{-# LANGUAGE ApplicativeDo #-}`, fields in declaration order:

```haskell
-- Pawl/Codec/StickerRef.hs
codec :: Codec.Codec StickerRef.StickerRef
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec StickerRef.owner
  sheet <- Fields.required "sheet" Common.natural StickerRef.sheet
  kind <- Fields.required "kind" StickerKind.codec StickerRef.kind
  index <- Fields.required "index" Common.natural StickerRef.index
  pure StickerRef.MkStickerRef {StickerRef.owner = owner, StickerRef.sheet = sheet, StickerRef.kind = kind, StickerRef.index = index}

-- Pawl/Codec/StickerPlacement.hs
codec :: Codec.Codec StickerPlacement.StickerPlacement
codec = Fields.object $ do
  sticker <- Fields.required "sticker" StickerRef.codec StickerPlacement.sticker
  timestamp <- Fields.required "timestamp" Timestamp.codec StickerPlacement.timestamp
  pure StickerPlacement.MkStickerPlacement {StickerPlacement.sticker = sticker, StickerPlacement.timestamp = timestamp}

-- Pawl/Codec/AbilitySticker.hs
codec :: Codec.Codec AbilitySticker.AbilitySticker
codec = Fields.object $ do
  tickets <- Fields.required "tickets" Common.natural AbilitySticker.tickets
  keywords <- Fields.defaulted "keywords" Map.empty (Common.repeats Keyword.codec) AbilitySticker.keywords
  abilities <- Fields.defaulted "abilities" [] (Common.list (GrantedAbility.codec Card.codec)) AbilitySticker.abilities
  pure AbilitySticker.MkAbilitySticker {AbilitySticker.tickets = tickets, AbilitySticker.keywords = keywords, AbilitySticker.abilities = abilities}

-- Pawl/Codec/PowerToughnessSticker.hs
codec :: Codec.Codec PowerToughnessSticker.PowerToughnessSticker
codec = Fields.object $ do
  tickets <- Fields.required "tickets" Common.natural PowerToughnessSticker.tickets
  power <- Fields.required "power" Common.integer PowerToughnessSticker.power
  toughness <- Fields.required "toughness" Common.integer PowerToughnessSticker.toughness
  pure PowerToughnessSticker.MkPowerToughnessSticker {PowerToughnessSticker.tickets = tickets, PowerToughnessSticker.power = power, PowerToughnessSticker.toughness = toughness}

-- Pawl/Codec/StickerSheet.hs
codec :: Codec.Codec StickerSheet.StickerSheet
codec = Fields.object $ do
  number <- Fields.required "number" Common.natural StickerSheet.number
  name <- Fields.required "name" Common.text StickerSheet.name
  names <- Fields.required "names" (Common.seq Common.text) StickerSheet.names
  art <- Fields.required "art" Common.natural StickerSheet.art
  abilities <- Fields.required "abilities" (Common.seq AbilitySticker.codec) StickerSheet.abilities
  powerToughness <- Fields.required "powerToughness" (Common.seq PowerToughnessSticker.codec) StickerSheet.powerToughness
  oracleText <- Fields.defaulted "oracleText" Nothing (Common.maybe Common.text) StickerSheet.oracleText
  pure StickerSheet.MkStickerSheet {StickerSheet.number = number, StickerSheet.name = name, StickerSheet.names = names, StickerSheet.art = art, StickerSheet.abilities = abilities, StickerSheet.powerToughness = powerToughness, StickerSheet.oracleText = oracleText}
```

Import aliases: `Pawl.Codec.PlayerId as PlayerId`, `Pawl.Codec.Timestamp as Timestamp`, `Pawl.Codec.Keyword as Keyword`, `Pawl.Codec.GrantedAbility as GrantedAbility`, `Pawl.Codec.Card as Card` (DelayedTrigger's pair), `Pawl.JsonCodec.{Codec,Common,Fields,Arm}`. A codec module's haddock: one line, Pawl.Codec.CounterPlacement's "A bare object keyed by the record's field names".

- [ ] **Step 5: Run** `-p StickerKind`, `-p StickerSheet`, then the full suite. Expected: green.
- [ ] **Step 6: Mutate.** Not applicable: codec round trips are their own observers. Say so in the PR.
- [ ] **Step 7: Commit.** "Add the sticker vocabulary and its codecs (related to #872)".

---

## Task 3: sticker sheet data, its loader and its lint

**Files:**
- Create: `data/sticker-sheets/{night-brushwagg-ringmaster,slimy-burrito-illusion,contortionist-otter-storm,ancestral-hot-dog-minotaur}.json`
- Create: `source/libraries/registry/Pawl/StickerSheets.hs`
- Modify: `pawl.cabal` (`data-files: sticker-sheets/*.json`), `.hooky.kdl` (the `json` hook's `files` gains `"data/sticker-sheets/*.json"`)
- Modify: `source/libraries/executable/Pawl/Executable.hs` (`ingest` stamps sheets)
- Modify: `source/libraries/test/Pawl/StickerSpec.hs` (the lint and `committedSheets`)

**Interfaces:**
- Produces: `StickerSheets.defaultRoot :: IO FilePath`; `StickerSheets.parse :: ByteString -> Either Text StickerSheet`; `StickerSheets.loadRoot :: FilePath -> IO [(FilePath, Either Text StickerSheet)]`; spec helper `committedSheets :: IO [StickerSheet.StickerSheet]`.
- Consumes: `Pawl.Codec.StickerSheet.codec`, `Pawl.Oracle.normalise`, `Pawl.Oracle.printed`, `Pawl.Slug.fromText`.

Sheets chosen: the fewest that give a test more than three (four), each ability sticker already expressed by a card in `data/cards/`:

| Sheet | Ability stickers | Expressed today by |
|---|---|---|
| 3 Night Brushwagg Ringmaster | Menace; Persist | boggart-brute.json; gravelgill-axeshark.json |
| 9 Slimy Burrito Illusion | Bushido 2; Double strike | inner-chamber-guard.json; boros-swiftblade.json |
| 15 Contortionist Otter Storm | {T}: Target creature gains haste until end of turn; Deathtouch, lifelink | hanweir-battlements.json's `{R}, {T}` haste ability, without the `{R}`; gifted-aetherborn.json |
| 23 Ancestral Hot Dog Minotaur | Afflict 2; Flying | eternal-of-harsh-truths.json; abbey-griffin.json |

Sheet 15 puts a `GrantedAbility.Activated` through the codec; sheet 23 exercises the hand-split two-word name sticker.

- [ ] **Step 1: Verify the sheets.** `curl -s -H 'User-Agent: pawl' 'https://api.scryfall.com/cards/search?q=set%3Asunf+cn%3A3'` (and 9, 15, 23); diff `oracle_text` against the files below. They are Scryfall's text as of 2026-10-09.

- [ ] **Step 2: Write the failing lint.** In `StickerSpec`, add imports (`Data.Foldable as Foldable`, `Data.List as List`, `Data.Map.Strict as Map`, `Data.Maybe as Maybe`, `Data.Text as Text`, `Pawl.Oracle as Oracle`, `Pawl.Slug as Slug`, `Pawl.StickerSheets as StickerSheets`, `Pawl.Types.AbilitySticker as AbilitySticker`, `Pawl.Types.PowerToughnessSticker as PowerToughnessSticker`, `Pawl.Types.StickerSheet as StickerSheet`), then:

```haskell
-- The four committed sheets, in this order; fewer if one is missing, which
-- each case's first assertion catches.
committedSheets :: IO [StickerSheet.StickerSheet]
committedSheets = do
  root <- StickerSheets.defaultRoot
  loaded <- StickerSheets.loadRoot root
  let byName = Map.fromList [(StickerSheet.name sheet, sheet) | (_, Right sheet) <- loaded]
  pure (Maybe.mapMaybe (\n -> Map.lookup (Text.pack n) byName) ["Night Brushwagg Ringmaster", "Slimy Burrito Illusion", "Contortionist Otter Storm", "Ancestral Hot Dog Minotaur"])

-- One Oracle line, "{TK}{TK} — rest", as its ticket count and its rest.
ticketLine :: Text.Text -> (Int, Text.Text)
ticketLine line =
  let (cost, rest) = Text.breakOn (Text.pack " \8212 ") line
   in (Text.count (Text.pack "{TK}") cost, Text.drop 3 rest)
```

and in `spec`:

```haskell
  -- CR 123.2: a sheet is three name, three art, two ability and two P/T
  -- stickers, and each structured field says what MTGJSON's text says.
  Spec.it s "CR 123.2 every sticker sheet says what its Oracle text says" $ do
    root <- StickerSheets.defaultRoot
    loaded <- StickerSheets.loadRoot root
    Spec.assertBool s (length loaded >= 4) "at least four sheets are committed"
    let offends (path, result) = case result of
          Left reason -> Just (path <> ": " <> Text.unpack reason)
          Right sheet ->
            let lines_ = maybe [] Text.lines (StickerSheet.oracleText sheet)
                abilityLines = fmap ticketLine (take 2 lines_)
                ptLines = fmap ticketLine (drop 2 lines_)
                abilityStickers = Foldable.toList (StickerSheet.abilities sheet)
                ptStickers = Foldable.toList (StickerSheet.powerToughness sheet)
                ptText p = Text.pack (show (PowerToughnessSticker.power p) <> "/" <> show (PowerToughnessSticker.toughness p))
                keywordsAgree a (_, rest) =
                  let printed = [Oracle.printed k | k <- Map.keys (AbilitySticker.keywords a)]
                   in not (null (AbilitySticker.abilities a)) || any Maybe.isNothing printed || List.sort (Oracle.normalise rest) == List.sort (fmap Text.toLower (Maybe.catMaybes printed))
                checks =
                  [ ("file named for the sheet", Slug.unwrap (Slug.fromText (StickerSheet.name sheet)) <> Text.pack ".json" == Text.pack (reverse (takeWhile (/= '/') (reverse path)))),
                    ("names join to the name", Text.unwords (Foldable.toList (StickerSheet.names sheet)) == StickerSheet.name sheet),
                    ("three name, three art, two ability, two P/T stickers", (length (StickerSheet.names sheet), StickerSheet.art sheet, length abilityStickers, length ptStickers) == (3, 3, 2, 2)),
                    ("four Oracle lines", length lines_ == 4),
                    ("ability ticket costs", fmap (fromIntegral . AbilitySticker.tickets) abilityStickers == fmap fst abilityLines),
                    ("P/T ticket costs", fmap (fromIntegral . PowerToughnessSticker.tickets) ptStickers == fmap fst ptLines),
                    ("P/T values", fmap ptText ptStickers == fmap snd ptLines),
                    ("keyword stickers", and (zipWith keywordsAgree abilityStickers abilityLines))
                  ]
             in case [what | (what, False) <- checks] of
                  [] -> Nothing
                  failed -> Just (path <> ": " <> show failed)
    Spec.assertEqWith s "every sheet agrees with its text" (Maybe.mapMaybe offends loaded) []
```

`Text.pack " \8212 "` is " — " (U+2014), the separator Scryfall and MTGJSON print. Confirm `Slug.unwrap` and `Slug.fromText` against `Pawl.Registry.slugFor`.

- [ ] **Step 3: Run; expected failure** "Could not find module 'Pawl.StickerSheets'".

- [ ] **Step 4: Write the loader.** `source/libraries/registry/Pawl/StickerSheets.hs`, `Pawl.Registry`'s `parseCard`/`loadRoot`/`defaultRoot` over the sheet codec:

```haskell
-- CR 123.2: the sticker sheets pawl ships, one per file under
-- data/sticker-sheets, read the way Pawl.Registry reads cards. Sheets are not
-- cards, so they are not in the card registry.
module Pawl.StickerSheets where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.StickerSheet as StickerSheet
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.StickerSheet as StickerSheet.Type

-- The sheet corpus's default root, Pawl.Registry.defaultRoot's posture.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "sticker-sheets"

-- What a sheet file's bytes mean, Pawl.Registry.parseCard's posture.
parse :: ByteString.ByteString -> Either Text.Text StickerSheet.Type.StickerSheet
parse bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode StickerSheet.codec

-- Every sheet file in a root, ascending by path, Pawl.Registry.loadRoot's posture.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text StickerSheet.Type.StickerSheet)]
loadRoot root = do
  entries <- fmap List.sort (Directory.listDirectory root)
  let paths = fmap (\entry -> root <> "/" <> entry) (filter (List.isSuffixOf ".json") entries)
  mapM (\path -> fmap (\bytes -> (path, parse bytes)) (ByteString.readFile path)) paths
```

Add `import qualified System.Directory as Directory`. In `pawl.cabal`, add `sticker-sheets/*.json` to `data-files`. Stage `pawl.cabal`, run `cabal-gild pawl.cabal`.

- [ ] **Step 5: Write the sheets.** `data/sticker-sheets/night-brushwagg-ringmaster.json`:

```json
{
  "abilities": [
    { "keywords": [ { "type": "Menace" } ], "tickets": 2 },
    { "keywords": [ { "type": "Persist" } ], "tickets": 3 }
  ],
  "art": 3,
  "name": "Night Brushwagg Ringmaster",
  "names": [ "Night", "Brushwagg", "Ringmaster" ],
  "number": 3,
  "oracleText": "{TK}{TK} — Menace\n{TK}{TK}{TK} — Persist (When this permanent dies, if it had no -1/-1 counters on it, return it to the battlefield under its owner's control with a -1/-1 counter on it.)\n{TK}{TK} — 2/3\n{TK}{TK}{TK}{TK}{TK}{TK} — 10/10",
  "powerToughness": [
    { "power": 2, "tickets": 2, "toughness": 3 },
    { "power": 10, "tickets": 6, "toughness": 10 }
  ]
}
```

`data/sticker-sheets/slimy-burrito-illusion.json`:

```json
{
  "abilities": [
    { "keywords": [ { "type": "Bushido", "value": 2 } ], "tickets": 2 },
    { "keywords": [ { "type": "DoubleStrike" } ], "tickets": 3 }
  ],
  "art": 3,
  "name": "Slimy Burrito Illusion",
  "names": [ "Slimy", "Burrito", "Illusion" ],
  "number": 9,
  "oracleText": "{TK}{TK} — Bushido 2 (Whenever this creature blocks or becomes blocked, it gets +2/+2 until end of turn.)\n{TK}{TK}{TK} — Double strike\n{TK}{TK} — 2/4\n{TK}{TK}{TK}{TK} — 5/6",
  "powerToughness": [
    { "power": 2, "tickets": 2, "toughness": 4 },
    { "power": 5, "tickets": 4, "toughness": 6 }
  ]
}
```

`data/sticker-sheets/contortionist-otter-storm.json`:

```json
{
  "abilities": [
    {
      "abilities": [
        {
          "type": "Activated",
          "value": {
            "cost": { "components": [ { "type": "TapThis" } ], "mana": [] },
            "modal": {
              "modes": [
                {
                  "clauses": [
                    {
                      "effects": [
                        {
                          "type": "ModifyTarget",
                          "value": {
                            "duration": { "type": "UntilEndOfTurn" },
                            "modification": { "type": "GainKeyword", "value": { "type": "Haste" } },
                            "ref": { "type": "InSlot", "value": "target" }
                          }
                        }
                      ]
                    }
                  ],
                  "targetSlots": { "target": { "pool": { "type": "Creatures" } } }
                }
              ]
            }
          }
        }
      ],
      "tickets": 2
    },
    { "keywords": [ { "type": "Deathtouch" }, { "type": "Lifelink" } ], "tickets": 4 }
  ],
  "art": 3,
  "name": "Contortionist Otter Storm",
  "names": [ "Contortionist", "Otter", "Storm" ],
  "number": 15,
  "oracleText": "{TK}{TK} — {T}: Target creature gains haste until end of turn.\n{TK}{TK}{TK}{TK} — Deathtouch, lifelink\n{TK}{TK} — 5/1\n{TK}{TK}{TK} — 3/5",
  "powerToughness": [
    { "power": 5, "tickets": 2, "toughness": 1 },
    { "power": 3, "tickets": 3, "toughness": 5 }
  ]
}
```

`data/sticker-sheets/ancestral-hot-dog-minotaur.json`:

```json
{
  "abilities": [
    { "keywords": [ { "type": "Afflict", "value": 2 } ], "tickets": 2 },
    { "keywords": [ { "type": "Flying" } ], "tickets": 3 }
  ],
  "art": 3,
  "name": "Ancestral Hot Dog Minotaur",
  "names": [ "Ancestral", "Hot Dog", "Minotaur" ],
  "number": 23,
  "oracleText": "{TK}{TK} — Afflict 2 (Whenever this creature becomes blocked, defending player loses 2 life.)\n{TK}{TK}{TK} — Flying\n{TK}{TK} — 1/4\n{TK}{TK}{TK}{TK}{TK} — 8/6",
  "powerToughness": [
    { "power": 1, "tickets": 2, "toughness": 4 },
    { "power": 8, "tickets": 5, "toughness": 6 }
  ]
}
```

Add `"data/sticker-sheets/*.json"` to `.hooky.kdl`'s `json` hook `files`; run `script/format-json.sh fix data/sticker-sheets/*.json`.

- [ ] **Step 6: Stamp sheets in `pawl ingest`.** In `Pawl.Executable.ingest`, after the `stamped` line:

```haskell
      sheetRoot <- StickerSheets.defaultRoot
      sheets <- StickerSheets.loadRoot sheetRoot
      sheetsStamped <- fmap length (Monad.filterM (stampSheet known) [(file, sheet) | (file, Right sheet) <- sheets])
```

print `putStrLn $ "sticker sheets given Oracle text: " <> show sheetsStamped`, and add:

```haskell
-- True when the sheet's file changed: CR 123.2's sheet given MTGJSON's text,
-- stampOne's posture for a sheet. A sheet has one face, its own name.
stampSheet :: (Map.Map (Text.Text, Text.Text) Text.Text, Map.Map Text.Text Text.Text) -> (FilePath, StickerSheet.Type.StickerSheet) -> IO Bool
stampSheet (exact, _) (file, sheet) =
  let name = StickerSheet.Type.name sheet
      stamped = sheet {StickerSheet.Type.oracleText = Map.lookup (name, name) exact}
   in if stamped == sheet || Maybe.isNothing (StickerSheet.Type.oracleText stamped)
        then pure False
        else True <$ ByteString.writeFile file (Encoding.encodeUtf8 (Common.render (Codec.encode StickerSheet.codec stamped)))
```

Imports: `Pawl.StickerSheets as StickerSheets`, `Pawl.Codec.StickerSheet as StickerSheet`, `Pawl.Types.StickerSheet as StickerSheet.Type`; check the module's existing aliases for `Maybe`, `Map`, `Encoding`. If `_scratch/AtomicCards.json` is present, run the ingest (CLAUDE.md's `jq -f script/ingest/candidates.jq` then `pawl ingest`) and `script/format-json.sh fix` any sheet it rewrites; MTGJSON's `texts` reduction covers every card, the funny ones included. If the dump is absent, say so in the PR.

- [ ] **Step 7: Run** `-p Sticker`, then the full suite. Expected: green.
- [ ] **Step 8: Mutate.** `m3.sed` on `data/sticker-sheets/ancestral-hot-dog-minotaur.json`: `s/"power": 8, "tickets": 5/"power": 8, "tickets": 4/` (one line once formatted; anchor on the `"power": 8` line if jq splits it). Pattern `Sticker`. Expected red: "every sheet agrees with its text", naming "P/T ticket costs".
- [ ] **Step 9: Commit.** "Ship four sticker sheets, their loader and their Oracle lint (related to #872)".

---

## Task 4: bringing sheets and CR 103.2d's draw

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Deck.hs`, every `Deck.MkDeck` site (Step 3)
- Modify: `source/libraries/types/Pawl/Types/Player.hs`, `source/libraries/codec/Pawl/Codec/Player.hs`, `source/libraries/codec/Pawl/Codec/PlayerSpec.hs`, `source/libraries/test/Pawl/DamageSpec.hs` (line 1434)
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, `source/libraries/types/Pawl/Types/Response.hs`, `source/libraries/engine/Pawl/Engine/Replay.hs`, `source/libraries/engine/Pawl/Engine/Game.hs` (the random-prompt comment), `source/libraries/scenario/Pawl/Scenario/Prompt.hs`, `source/libraries/scenario/Pawl/Scenario.hs`, `source/libraries/scenario/Pawl/Scenario/Reply.hs`, `source/libraries/test/Pawl/ReplaySpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Setup.hs` (`gameWith`, `createDeck`, `newGame`, `startGameFromCards`, `resetPlayers`, new `drawStickerSheets`)
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Deck.stickerSheets :: Seq StickerSheet`; `Player.stickerSheets :: Seq StickerSheet`; `Player.chosenStickerSheets :: Set Natural`; `Prompt.RandomStickerSheet :: NonEmpty Natural -> Prompt Natural`; `Response.SelectedStickerSheetAtRandom Natural`; `Setup.drawStickerSheets :: [PlayerId] -> Game ()`; spec helpers `sheetDraws`, `startedWith`, `chosenOf`.

- [ ] **Step 1: Write the failing tests.** In `StickerSpec`:

```haskell
-- Keeps every hand and records each CR 103.2d draw's candidates, answering
-- with the LAST, pinned by position.
sheetDraws :: Prompt.Prompt r -> State.State [[Natural]] r
sheetDraws p = case p of
  Prompt.RandomStickerSheet slots -> do
    State.modify' (NonEmpty.toList slots :)
    pure (NonEmpty.last slots)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Setup.newGame over this matchup, with what sheetDraws was asked, in order.
startedWith :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [[Natural]])
startedWith matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame sheetDraws (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

chosenOf :: PlayerId.PlayerId -> GameState.GameState -> Set.Set Natural
chosenOf pid gs = foldMap Player.chosenStickerSheets (Map.lookup pid (GameState.players gs))

withSheetsDeck :: [StickerSheet.StickerSheet] -> Deck.Deck -> Deck.Deck
withSheetsDeck sheets deck = deck {Deck.stickerSheets = Seq.fromList sheets}
```

```haskell
  Spec.it s "CR 103.2d four sheets: three are drawn at random, one at a time" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck sheets plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "the four committed sheets load" (length sheets) 4
    Spec.assertEqWith s "CR 103.2d three of the four sheets are chosen" (chosenOf S.alice gs) (Set.fromList [1, 2, 3])
    Spec.assertEqWith s "each drawn from those not yet drawn" asked [[0, 1, 2, 3], [0, 1, 2], [0, 1]]
    Spec.assertEqWith s "and bob, who brought none, plays without stickers" (chosenOf S.bob gs) Set.empty
  Spec.it s "CR 103.2d/123.2b three sheets are all kept, unasked" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck (take 3 sheets) plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "CR 123.2b all three are chosen" (chosenOf S.alice gs) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "and nothing was drawn at random" asked []
```

Imports to add: `Control.Monad.Trans.State.Strict as State`, `Data.List.NonEmpty as NonEmpty`, `Data.Set as Set`, `Numeric.Natural (Natural)`, `Pawl.Engine.Engine as Engine`, `Pawl.Types.Deck as Deck`, `Pawl.Types.MulliganDecision as MulliganDecision`, `Pawl.Types.Player as Player`, `Pawl.Types.PlayerId as PlayerId`.

In `ReplaySpec`, beside "ChooseDungeon round-trips through the transcript":

```haskell
        -- CR 103.2d: the sheet randomness drew is part of the transcript.
        Spec.it s "RandomStickerSheet round-trips through the transcript" $ do
          let p = Prompt.RandomStickerSheet (0 NonEmpty.:| [3])
          Spec.assertEqWith s "drawing the second round trips" (Replay.decode p (Replay.encode p 3)) (Just 3)
          Spec.assertEqWith s "drawing the first round trips" (Replay.decode p (Replay.encode p 0)) (Just 0)
          Spec.assertEqWith s "a short transcript draws the first offered" (Replay.defaultAnswer p) 0
```

- [ ] **Step 2: Run; expected failure** "'Deck.stickerSheets' is not a (visible) field".

- [ ] **Step 3: Deck.** After `schemes` in `Deck.hs` (comma on the line before):

```haskell
    -- | CR 123.2: the sticker sheets this player brings. A Seq, not a Set:
    -- CR 123.2b lets a limited player bring two of one sheet, and CR 123.3a
    -- keeps their stickers apart.
    stickerSheets :: Seq.Seq StickerSheet.StickerSheet
```

`fromCards` gains `stickerSheets = Seq.empty`. Sites (`grep -rn 'Deck.MkDeck' source --include='*.hs'`): `SetupSpec` 254, 267, 279, 293, 310; `VanguardSpec` 56; `CommanderSpec` 132, 239, 282, 337, 430, 1009, 1069, 1100; `OutsideTheGameSpec` 349; `DungeonSpec` 515. Each gains `Deck.stickerSheets = Seq.empty`; add `import qualified Data.Sequence as Seq` where absent.

- [ ] **Step 4: Player.** After `outsideTheGame`, in `Player.hs`:

```haskell
    -- | CR 123.2 / 123.2c: the sticker sheets this player brought, revealed.
    -- Kept on the player for `dungeons`' reason: CR 727.2 and CR 729.2 rerun
    -- CR 103.2d's draw with no Deck in hand.
    stickerSheets :: Seq.Seq StickerSheet.StickerSheet,
    -- | CR 103.2d: the positions in `stickerSheets` drawn for this game. Only
    -- their stickers are available (Pawl.Engine.Sticker.available).
    chosenStickerSheets :: Set.Set Natural.Natural,
```

Rewrite the `outsideTheGame` haddock's last sentence. Replace "Sticker sheets (#872) are\n    -- outside the game too but are not cards and have no characteristics (CR\n    -- 123.2), so they need a field of their own rather than this one." with "Sticker sheets are outside the game too but are not cards (CR 123.2); they\n    -- ride `stickerSheets` below." That removes the `#872` elision; `Keyword.hs`'s stays.

`Codec/Player.hs`:

```haskell
  stickerSheets <- Fields.defaulted "stickerSheets" Seq.empty (Common.seq StickerSheet.codec) Player.stickerSheets
  chosenStickerSheets <- Fields.defaulted "chosenStickerSheets" Set.empty (Common.set Common.natural) Player.chosenStickerSheets
```

and the two fields in the construction. Construction sites (`grep -rn 'Player.MkPlayer\b' source --include='*.hs'`): `Setup.hs:165` (`gameWith`; `Player.stickerSheets = Seq.empty, Player.chosenStickerSheets = Set.empty` with "CR 123.2: which sheets a player brought is their deck's business -- createDeck below."), `Codec/Player.hs:56`, `PlayerSpec` 34 and 67 (one of them non-empty: `Player.chosenStickerSheets = Set.fromList [0, 2]`, its JSON string gaining `,"chosenStickerSheets":[0,2]` in the codec's field order; read `Fields.defaulted`'s encoder to see whether an empty default is written, and spell the other case accordingly), `DamageSpec.hs:1434`.

- [ ] **Step 5: The prompt.** `Prompt.hs`, after `RandomObject`:

```haskell
  -- | CR 103.2d: which of these sheet positions randomness draws next.
  -- RandomObject's reasons for carrying neither Decider nor PlayerId.
  RandomStickerSheet :: NonEmpty.NonEmpty Natural.Natural -> Prompt Natural.Natural
```

`Response.hs`, after `ChoseCompanion`:

```haskell
  | -- | CR 103.2d: the sheet position randomness drew.
    SelectedStickerSheetAtRandom Natural.Natural
```

Every Prompt site (`grep -rn 'Prompt\.\(Type\.\)\?RandomDepth\b' source --include='*.hs'` is the enumeration):
- `Replay.encode`: `Prompt.RandomStickerSheet _ -> Response.SelectedStickerSheetAtRandom answer`.
- `Replay.decode`: `Prompt.RandomStickerSheet _ -> case response of Response.SelectedStickerSheetAtRandom n -> Just n; _ -> Nothing` (laid out as its neighbours).
- `Replay.defaultAnswer`: `-- The head of the offer is always an undrawn sheet, and FIXED for the reason RandomObject gives above.` then `Prompt.RandomStickerSheet slots -> NonEmpty.head slots`.
- `Scenario/Prompt.deciderOf`: `Prompt.RandomStickerSheet {} -> Nothing`; `kindOf`: `Prompt.RandomStickerSheet {} -> "RandomStickerSheet"`.
- `Scenario.withinOffer`: beside `RandomObject`, `Prompt.Type.RandomStickerSheet slots -> chosen \`elem\` slots` (that function's own spelling).
- `Reply.shapeOf`: `Prompt.RandomStickerSheet {} -> natural`.
- `Game.hs`'s comment listing the randomness prompts (line 343): add `Prompt.RandomStickerSheet` and CR 103.2d.

- [ ] **Step 6: The setup step.** `createDeck`'s `Map.adjust` adds `Player.stickerSheets = Deck.stickerSheets deck`. In `Setup.hs`, after `anteFromLibraries`:

```haskell
-- CR 103.2d / 123.2: each player who brought more than three sticker sheets
-- has three drawn at random; three or fewer are all chosen, which is exact for
-- CR 123.2b's limited three. Randomness, so Prompt.RandomStickerSheet, one draw
-- at a time over the undrawn positions, filtered rather than trusted. None
-- brought means none chosen: that player plays without stickers.
--
-- Not implemented: CR 123.2a's at least ten unique sheets, which is deck
-- validation (#N1). Not implemented: a ruling on whether a Shahrazad subgame
-- (CR 729.2) plays with sticker sheets; this draws again there (#N2).
drawStickerSheets :: [PlayerId] -> Game ()
drawStickerSheets seated = Monad.forM_ seated $ \pid -> do
  gs <- State.get
  let brought = foldMap Player.stickerSheets (Map.lookup pid (GameState.players gs))
      slots = zipWith const [0 :: Natural ..] (Foldable.toList brought)
      drawThree n remaining chosen = case remaining of
        first : rest | n > (0 :: Int) -> do
          picked <-
            if null rest
              then pure first
              else do
                answer <- Game.ask (Prompt.RandomStickerSheet (first NonEmpty.:| rest))
                pure (if List.elem answer remaining then answer else first)
          drawThree (n - 1) (List.delete picked remaining) (Set.insert picked chosen)
        _ -> pure chosen
  chosen <- if length slots <= 3 then pure (Set.fromList slots) else drawThree 3 slots Set.empty
  State.modify' (\g -> g {GameState.players = Map.adjust (\p -> p {Player.chosenStickerSheets = chosen}) pid (GameState.players g)})
```

Replace `#N1`/`#N2` once Task 9 files them; until then leave the literal and grep for it in Task 10.

Call it in `newGame` and `startGameFromCards`, after `Monad.forM_ seated Companion.reveal` and before `anteFromLibraries seated`, with the comment `-- CR 103.2d, after CR 103.2b's reveal.` (in `startGameFromCards`: `-- CR 103.2d again: CR 727.1 and CR 729.2 each start a new game following rule 103.`).

`resetPlayers`'s `Status.Playing` update gains, after `Player.companion = Nothing`:

```haskell
              -- CR 103.2d: the new game draws again. Player.stickerSheets is
              -- NOT reset, for Player.dungeons' reason.
              Player.chosenStickerSheets = Set.empty
```

(comma on the line before). Check `Setup.hs`'s imports for `Numeric.Natural (Natural)`, `Data.Foldable as Foldable`, `Data.List as List`, `Data.Set as Set`, `Prompt`.

- [ ] **Step 7: Run** `-p Sticker`, `-p Replay`, `-p Setup`, `-p Player`, then the full suite.
- [ ] **Step 8: Mutate.** `m4.sed` on `Setup.hs`: `s/if length slots <= 3 then/if length slots <= 4 then/`, pattern `Sticker`. Expected red: "CR 103.2d three of the four sheets are chosen". (More permissive, but it asks nothing, so it cannot hang.)
- [ ] **Step 9: Commit.** "Bring sticker sheets and draw three at setup (related to #872)".

---

## Task 5: `Object.stickers`, CR 123.5 retention and the CR 613.7k restamp

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Object.hs` (field, `newIncarnation`)
- Modify: `source/libraries/codec/Pawl/Codec/Object.hs`, `source/libraries/codec/Pawl/Codec/ObjectSpec.hs`
- Modify: every `Object.MkObject` site (Step 3)
- Modify: `source/libraries/engine/Pawl/Engine/Reversal.hs`, `source/libraries/engine/Pawl/Engine/Game.hs` (`restampStickers`), `source/libraries/engine/Pawl/Engine/Event.hs` (`mkObj`, `asComponent`, the restamp call, `meld`), `source/libraries/engine/Pawl/Engine/Restamp.hs` (elision comment)
- Create: `source/libraries/engine/Pawl/Engine/Sticker.hs`; Modify `pawl.cabal`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Object.stickers :: Seq StickerPlacement`; `Game.restampStickers :: ObjectId -> GameState -> GameState`; `Sticker.refsOn :: PlayerId -> Natural -> StickerSheet -> [StickerRef]`; `Sticker.available :: PlayerId -> Set StickerKind -> GameState -> [StickerRef]`; `Sticker.put :: PlayerId -> ObjectId -> StickerRef -> GameState -> GameState`; spec helpers `withSheets`, `stickerOn`, `namingTarget`.

- [ ] **Step 1: Write the failing tests.** In `StickerSpec`:

```haskell
-- alice brings these sheets and has every one chosen, as CR 103.2d leaves
-- three or fewer.
withSheets :: [StickerSheet.StickerSheet] -> GameState.GameState -> GameState.GameState
withSheets sheets gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.stickerSheets = Seq.fromList sheets, Player.chosenStickerSheets = Set.fromList (zipWith const [0 ..] sheets)}) S.alice (GameState.players gs)}

-- alice's first available art sticker on `oid`.
stickerOn :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
stickerOn oid gs = case Sticker.available S.alice (Set.singleton StickerKind.Art) gs of
  ref : _ -> Sticker.put S.alice oid ref gs
  [] -> gs

-- FILTERS the offered set, so CR 608.2b's re-read finds the target.
namingTarget :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namingTarget oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  _ -> S.identityAnswer p

stickersIn :: Zone.Zone -> GameState.GameState -> [(Seq.Seq StickerPlacement.StickerPlacement, Timestamp.Timestamp)]
stickersIn zone gs = [(Object.stickers obj, Object.timestamp obj) | oid <- Game.zoneMembers zone S.alice gs, Just obj <- [Game.lookupObject oid gs]]
```

```haskell
  -- Review Focus 3. One board, two bob spells: a Bolt kills the Piker (public
  -- to public) and an Unsummon bounces it (public to hidden).
  Spec.it s "CR 123.5/613.7k an art sticker stays through a death, restamped, and comes off in a bounce" $ do
    sheets <- committedSheets
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    unsummon <- S.printingOf s registry "Unsummon"
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (S.landsFor mountain S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (pikerId, g1) = S.addPermanent piker S.alice base
        stickered = stickerOn pikerId g1
        placed = foldMap (Foldable.toList . Object.stickers) (Game.lookupObject pikerId stickered)
        (boltId, g2) = S.addHandCard bolt S.bob stickered
        (unsummonId, g3) = S.addHandCard unsummon S.bob g2
        bobsWindow = g3 {GameState.priority = Just S.bob}
        killed = S.settleSba (S.runPure (namingTarget pikerId) bobsWindow (S.cast S.bob boltId >> Stack.resolveTop))
        bounced = S.runPure (namingTarget pikerId) bobsWindow (S.cast S.bob unsummonId >> Stack.resolveTop)
    case (stickersIn Zone.Graveyard killed, placed) of
      ([(kept, stamp)], [placement]) -> do
        Spec.assertEqWith s "CR 123.5 the same art sticker is on the card in the graveyard" (fmap StickerPlacement.sticker (Foldable.toList kept)) [StickerPlacement.sticker placement]
        Spec.assertBool s (all (\p -> StickerPlacement.timestamp p > stamp) kept) "CR 613.7k restamped right after the card's new timestamp"
      other -> Spec.assertFailure s ("expected one graveyard card and one placement, got " <> show other)
    Spec.assertEqWith s "CR 123.5 the card bounced to hand has none" (fmap fst (stickersIn Zone.Hand bounced)) [Seq.empty]
  -- Review Focus 2. Twenty cards a deck so nobody is decked.
  Spec.it s "CR 727.2/103.2d a restart draws the sheets again and every sticker comes off" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let plain = Deck.fromCards (Map.singleton mountain 20)
        (started, _) = startedWith ((S.alice, withSheetsDeck sheets plain) NonEmpty.:| [(S.bob, plain)])
        (pikerId, withPiker) = S.addPermanent piker S.alice started
        stickered = stickerOn pikerId withPiker
        ((_, restarted), asked) = State.runState (Engine.runGame sheetDraws stickered (Setup.restartGame S.performer Set.empty S.alice)) []
        stickeredObjects g = Map.keys (Map.filter (not . Seq.null . Object.stickers) (GameState.objects g))
    Spec.assertEqWith s "the Piker was stickered before the restart" (stickeredObjects stickered) [pikerId]
    Spec.assertEqWith s "CR 727.2 no object is stickered after it" (stickeredObjects restarted) []
    Spec.assertEqWith s "CR 103.2d the restart drew three sheets again" (reverse asked) [[0, 1, 2, 3], [0, 1, 2], [0, 1]]
```

Imports to add: `Pawl.Engine.Game as Game`, `Pawl.Engine.Sticker as Sticker`, `Pawl.Types.Object as Object`, `Pawl.Types.ObjectId as ObjectId`, `Pawl.Types.Recipient as Recipient`, `Pawl.Types.StickerKind as StickerKind`, `Pawl.Types.StickerPlacement as StickerPlacement`, `Pawl.Types.Timestamp as Timestamp`, `Pawl.Types.Zone as Zone`.

In `ObjectSpec`, give one of the two `MkObject` cases a non-empty `Object.stickers` (`Seq.singleton (StickerPlacement.MkStickerPlacement (StickerRef.MkStickerRef (PlayerId.MkPlayerId 1) 0 StickerKind.Art 2) (Timestamp.MkTimestamp 5))`) and its JSON its `"stickers":[...]` entry, in the codec's field order.

- [ ] **Step 2: Run; expected failure** "Could not find module 'Pawl.Engine.Sticker'".

- [ ] **Step 3: The field.** In `Object.hs`, LAST, after `duplicate` (comma moves), so the positional sites only append:

```haskell
    -- | CR 123.1: the stickers on this object, oldest first. Per-incarnation,
    -- save for CR 123.5's exception: Pawl.Engine.Event.changeZoneWithCause's
    -- mkObj carries them across a move into a public zone. Not copiable (CR
    -- 123.1): no ProjectedCharacteristics holds them.
    stickers :: Seq.Seq StickerPlacement.StickerPlacement
```

`newIncarnation` gains, after `paired = Nothing` (comma on the line before):

```haskell
      -- CR 123.5's exception is written back by
      -- Pawl.Engine.Event.changeZoneWithCause's mkObj, for a public destination.
      stickers = Seq.empty
```

Construction sites (`grep -rn 'Object.MkObject' source --include='*.hs'`), each gaining `Object.stickers = Seq.empty` (an object minted from nothing has none): `Codec/Object.hs:153` (reads `stickers <- Fields.defaulted "stickers" Seq.empty (Common.seq StickerPlacement.codec) Object.stickers` and writes it), `Activate.hs:126`, `Dungeon.hs:250`, `Engine.hs:877`, `Event.hs` 1063, 1296, 8038, `Monarch.hs:182`, `Prepare.hs:164`, `Setup.hs:358`; `Event.hs:8320` (`meld`) gains the elision `-- Not implemented: CR 123.5a's stickers on a melded permanent (#872).` above its `Object.stickers = Seq.empty`. `Reversal.hs:415`: `stickers <- field Object.stickers` beside `paired`, and `Object.stickers = stickers` in the construction, since a reversal rebuilds the object as it was. Test record sites: `Support.hs` 909, 985, 1066, 1151, 1241, 1340, 1415, 1496, 2063, 2326; `CastSpec.hs:483`; `GameSpec.hs` 179, 2124, 2595; `CopySpec.hs:1075`; `ResolveSpec.hs` 608, 698, 785, 1427, 1540, 1663, 1943, 2692; `ObjectSpec.hs` 66, 147. POSITIONAL sites, which absorb a field in argument order and must gain a trailing ` Seq.empty`: `ResolveSpec.hs` 882, 895, 928, 957 and `ZoneChangeSpec.hs:338`. A positional site that is missed is a type error, not a silent one; read each anyway.

- [ ] **Step 4: Retention and restamp.** `Game.hs`, beside `freshTimestamp`:

```haskell
-- CR 613.7k: each sticker on the object takes a new timestamp immediately
-- after the object's own, keeping their relative order. Called right after the
-- object is stamped, so nothing is minted in between.
restampStickers :: ObjectId -> GameState -> GameState
restampStickers oid gs = case lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let step (acc, g) placement =
          let (ts, g') = freshTimestamp g
           in (acc Seq.|> placement {StickerPlacement.timestamp = ts}, g')
        (restamped, stamped) = Foldable.foldl' step (Seq.empty, gs) (Object.stickers obj)
     in stamped {GameState.objects = Map.adjust (\o -> o {Object.stickers = restamped}) oid (GameState.objects stamped)}
```

`Event.hs`, `changeZoneWithCause`'s `mkObj` record, after `Object.chosenPlayer = ...` (comma moves):

```haskell
                    -- CR 123.5 / 400.7m: stickers stay on an object moving to
                    -- a public zone and apply to the new object; a hidden zone
                    -- drops them. Restamped after placeObject (CR 613.7k).
                    Object.stickers = if Game.isHiddenZone dest then Seq.empty else Object.stickers obj
```

`asComponent`'s `Just component` arm:

```haskell
                        Just component -> (Game.representComponent component (mkObj entrySeed ts)) {Object.stickers = Seq.empty}
```

with the comment `-- Not implemented: CR 123.5c's owner choosing which split object keeps the stickers (#872).` above the `case`. After `newId <- placeObject pid (asComponent dest leading) dest position`:

```haskell
              -- CR 613.7k.
              State.modify' (Game.restampStickers newId)
```

`Restamp.hs:150`'s remap: add above it `-- Not implemented: CR 613.7k's sticker restamp under this reorder, and under a new timestamp without a zone change (CR 613.7e-g); unobservable while stickers are art (#872).`

- [ ] **Step 5: `Pawl.Engine.Sticker`.**

```haskell
-- | CR 123.3: the stickers a player has access to, and putting one on an
-- object. Availability is computed, never stored: a move to a hidden zone frees
-- a sticker with no bookkeeping (CR 123.5).
module Pawl.Engine.Sticker where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.StickerKind as StickerKind
import qualified Pawl.Types.StickerPlacement as StickerPlacement
import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.StickerSheet as StickerSheet

-- CR 123.3a: every sticker on one sheet, by position.
refsOn :: PlayerId -> Natural -> StickerSheet.StickerSheet -> [StickerRef.StickerRef]
refsOn pid slot sheet =
  let refs kind n = [StickerRef.MkStickerRef {StickerRef.owner = pid, StickerRef.sheet = slot, StickerRef.kind = kind, StickerRef.index = i} | i <- List.genericTake n [0 :: Natural ..]]
   in refs StickerKind.Name (Natural.length (StickerSheet.names sheet))
        <> refs StickerKind.Ability (Natural.length (StickerSheet.abilities sheet))
        <> refs StickerKind.PowerToughness (Natural.length (StickerSheet.powerToughness sheet))
        <> refs StickerKind.Art (StickerSheet.art sheet)

-- CR 123.3 / 123.2c: the stickers of these kinds on the player's chosen
-- sheets that are on no object they own, in any zone.
--
-- Not implemented: a ruling on a stickered card that changed owners; its
-- stickers count only against its current owner (#N3).
available :: PlayerId -> Set.Set StickerKind.StickerKind -> GameState -> [StickerRef.StickerRef]
available pid kinds gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> []
  Just player ->
    let used = Set.fromList [StickerPlacement.sticker p | obj <- Map.elems (GameState.objects gs), Object.owner obj == pid, p <- Foldable.toList (Object.stickers obj)]
        offered =
          [ ref
          | (slot, sheet) <- zip [0 :: Natural ..] (Foldable.toList (Player.stickerSheets player)),
            Set.member slot (Player.chosenStickerSheets player),
            ref <- refsOn pid slot sheet,
            Set.member (StickerRef.kind ref) kinds
          ]
     in filter (\ref -> Set.notMember ref used) offered

-- CR 123.3 / 613.7k: put the sticker on the object, stamped now.
put :: PlayerId -> ObjectId -> StickerRef.StickerRef -> GameState -> GameState
put _placer oid ref gs =
  let (ts, stamped) = Game.freshTimestamp gs
      placement = StickerPlacement.MkStickerPlacement {StickerPlacement.sticker = ref, StickerPlacement.timestamp = ts}
   in stamped {GameState.objects = Map.adjust (\o -> o {Object.stickers = Object.stickers o Seq.|> placement}) oid (GameState.objects stamped)}
```

`put` takes the placer for Task 8's event; `_placer` until then. Confirm `Pawl.Extra.Natural.length`'s spelling (`Pawl/Extra/Natural.hs:14`). Stage `pawl.cabal`, run `cabal-gild pawl.cabal`.

- [ ] **Step 6: Silent sites, read and recorded in the PR.** `Resolve.Effect.happenedBetween`: compares `objects` (a placement is state) and copies `nextTimestamp` across, so no edit. `Interchangeable.objects`: compares the whole Object less `timestamp` and `identity`, so two objects with stickers differ, which is right (conservative). `Resolve/Effect.hs:1384`'s copy goes through `newIncarnation`, so a spell copy carries none (CR 123.1). `castFromOutside` (`Event.hs:5755`): outside to the stack, nothing to keep. `Setup.splitComponents`, `toLibraryCard`, `toCommandCard`, `strandedAnte`: `newIncarnation`, so CR 727.2 and 729.2 rebuilds clear stickers (Review Focus 2 proves it). `Planechase.hs:189`: a plane card, never stickered. Phasing and a type change are no zone change, so CR 702.26d and 205.1a hold by construction.

- [ ] **Step 7: Run** `-p Sticker`, `-p Object`, the full suite.
- [ ] **Step 8: Mutate.**
  - `m5a.sed` on `Event.hs`: `s/Object.stickers = if Game.isHiddenZone dest then Seq.empty else Object.stickers obj/Object.stickers = Seq.empty/`. Pattern `Sticker`. Expected red: "CR 123.5 the same art sticker is on the card in the graveyard".
  - `m5b.sed` on `Event.hs`: `s/Object.stickers = if Game.isHiddenZone dest then Seq.empty else Object.stickers obj/Object.stickers = Object.stickers obj/`. Expected red: "CR 123.5 the card bounced to hand has none". (More permissive; no loop is reachable.)
  - `m5c.sed` on `Event.hs`: `s/State.modify. (Game.restampStickers newId)/State.modify' id/`. Expected red: "CR 613.7k restamped right after the card's new timestamp".
  - `m5d.sed` on `Object.hs`: `s/^      stickers = Seq.empty$/      stickers = stickers object/` (the accessor, in the record update inside `newIncarnation`). Pattern `restart`. Expected red: "CR 727.2 no object is stickered after it" (mkObj still decides a zone change, so only the rebuilds observe this line).
- [ ] **Step 9: Commit.** "Keep stickers on an object through a public move, restamped (related to #872)".

---

## Task 6: the stickered filters and Croakid Amphibonaut

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Filter.hs`, `source/libraries/codec/Pawl/Codec/Filter.hs`, `source/libraries/codec/Pawl/Codec/FilterSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Filter.hs` (`View`, the player view at line 796, `matches`, `rewrite`, `bakeBound`, `manaValueThresholds`, `statesAQuality`, `readsSourcePower`)
- Modify: `source/libraries/engine/Pawl/Engine/Projection/View.hs` (`viewOfCard`, `viewOfCharacteristics`), `source/libraries/engine/Pawl/Engine/Count.hs` (`viewOfSnapshot`, `bakePerspective`), `source/libraries/engine/Pawl/Engine/Projection.hs` (`filterReads`, `filterReadsPeers`), `source/libraries/engine/Pawl/Engine/Interchangeable/Mentions.hs` (`filterNames`)
- Modify: `source/libraries/test/Pawl/FilterSpec.hs` (38, 131), `source/libraries/test/Pawl/Support.hs` (2429), `source/libraries/test/Pawl/CardSpec.hs` (`filterSlotsReadSingly`), `source/libraries/test/Pawl/FilterPositionLintSpec.hs` (`canHostSubjects`)
- Create: `data/cards/croakid-amphibonaut.json`

**Interfaces:**
- Produces: `Filter.HasSticker StickerKind`, `Filter.Stickered`; `Filter.View`'s `stickerKinds :: Seq StickerKind`.

- [ ] **Step 1: Verify Oracle.** Croakid Amphibonaut `{4}{U}` Creature — Frog Guest 4/3: "This creature has flying as long as you control a stickered permanent." Chosen over Big Winner, Grabby Tabby, Sanguine Sipper and Scared Stiff because flying is on no other card in these boards.

- [ ] **Step 2: Write the failing tests.** In `StickerSpec`:

```haskell
-- Answers ChooseCopyTarget with `oid`.
copying :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copying oid p = case p of
  Prompt.ChooseCopyTarget {} -> Just oid
  _ -> S.identityAnswer p
```

```haskell
  -- Three readings of one board: no sticker, a sticker, and the stickered
  -- Piker bounced (CR 123.4: not sticky).
  Spec.it s "CR 123.4 Croakid Amphibonaut flies beside a stickered permanent and stops when the sticker leaves" $ do
    sheets <- committedSheets
    croakid <- S.printingOf s registry "Croakid Amphibonaut"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (croakidId, g1) = S.addPermanent croakid S.alice base
        (pikerId, unstickered) = S.addPermanent piker S.alice g1
        stickered = stickerOn pikerId unstickered
        (unsummonId, g2) = S.addHandCard unsummon S.bob stickered
        bounced = S.runPure (namingTarget pikerId) (g2 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
        flies = Projection.hasKeyword Keyword.Flying croakidId
    Spec.assertEqWith s "CR 123.4 flying with no sticker, with one, and after the bounce" (flies unstickered, flies stickered, flies bounced) (False, True, False)
  -- Review Focus 5. The Clone copies the stickered Piker; then the Piker
  -- leaves. A Clone that copied the sticker would keep Croakid flying.
  Spec.it s "CR 123.1 a Clone of a stickered permanent is not stickered" $ do
    sheets <- committedSheets
    croakid <- S.printingOf s registry "Croakid Amphibonaut"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (croakidId, g1) = S.addPermanent croakid S.alice base
        (pikerId, g2) = S.addPermanent piker S.alice g1
        (_, staged) = S.spellOnStack clone S.alice (stickerOn pikerId g2)
        cloned = S.settleSba (S.runPure (copying pikerId) staged Stack.resolveTop)
        (unsummonId, g3) = S.addHandCard unsummon S.bob cloned
        bounced = S.runPure (namingTarget pikerId) (g3 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.1 with the stickered Piker gone, its Clone does not keep Croakid flying" (Projection.hasKeyword Keyword.Flying croakidId bounced) False
    Spec.assertEqWith s "the Clone is on the battlefield beside Croakid" (length (filter (\oid -> Set.notMember oid (GameState.battlefield base)) (Game.zoneMembers Zone.Battlefield S.alice bounced))) 2
```

Imports: `Pawl.Engine.Projection as Projection`, `Pawl.Types.Keyword as Keyword`. The proxy assertion counts Croakid and the Clone, which `base` did not hold; confirm `GameState.battlefield` is a `Set ObjectId`.

In `Codec/FilterSpec.hs`, after "HasCountersOfAnyKind":

```haskell
  Spec.it s "HasSticker" $ Common.assertCodec s codec (Filter.HasSticker StickerKind.Art) " {\"type\":\"HasSticker\",\"value\":{\"type\":\"Art\"}} "
  Spec.it s "Stickered" $ Common.assertCodec s codec Filter.Stickered " {\"type\":\"Stickered\"} "
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Filter.HasSticker'".

- [ ] **Step 4: The atoms.** `Types/Filter.hs`, after `HasCountersOfAnyKind`:

```haskell
  | -- | CR 123.6-123.9: does the CANDIDATE have a sticker of this kind on it
    -- (Proficient Pyrodancer's "with an art sticker on it")? Uncharacteristic,
    -- HasCounters' reason.
    HasSticker StickerKind.StickerKind
  | -- | CR 123.4: is the CANDIDATE stickered, any kind, now?
    Stickered
```

`Codec/Filter.hs`: `Arm.payload "HasSticker" StickerKind.codec Filter.HasSticker (\x -> case x of Filter.HasSticker y -> Just y; _ -> Nothing),` and `Arm.nullary "Stickered" Filter.Stickered,` beside `HasCountersOfAnyKind`; `tagOf` gains `Filter.HasSticker {} -> "HasSticker"` and `Filter.Stickered {} -> "Stickered"`. `Arm.tagged` forces the tags, not the arms; the two FilterSpec cases are the arms' round trips.

Every `Filter` site (`grep -rn 'HasCountersOfAnyKind' source --include='*.hs'` is the enumeration), each answered as `HasCountersOfAnyKind` is, for its reason:
- `Engine/Filter.matches`: `-- CR 123.6-123.9: read off the object, never a projection (CR 123.1).` `Filter.HasSticker kind -> elem kind (stickerKinds view)`; `-- CR 123.4.` `Filter.Stickered -> not (null (stickerKinds view))`.
- `Engine/Filter.rewrite`, `bakeBound`: `-> predicate` (no word, no slot). `manaValueThresholds`: `-> []`. `statesAQuality`: `-> True`. `readsSourcePower`: `-> False`.
- `Count.bakePerspective`: `-> predicate`. `Projection.filterReads`: `-> Set.empty`, comment "CR 109.3's characteristics hold no stickers." `filterReadsPeers`: `-> False`.
- `Mentions.filterNames`: `-> False` (names no object, slot or Filter).
- `CardSpec.filterSlotsReadSingly`: `-> []`. `FilterPositionLintSpec.canHostSubjects`: the `HasCountersOfAnyKind` answer.
- Wildcard sites read and right as they stand: `Engine/Filter.overBoundSlots`' `_ -> pure predicate` (so `boundSlots` and `renameBound`): neither atom holds a slot. CardSpec's keyword traversals: neither atom holds a keyword.

- [ ] **Step 5: The View field.** `Engine/Filter.hs`'s `View`, after `counters` (add `import qualified Data.Sequence as Seq` and `Pawl.Types.StickerKind as StickerKind`):

```haskell
    -- | CR 123.1 / 123.4: the kinds of the stickers on the candidate, one per
    -- sticker. Off the object: CR 123.1 keeps stickers out of the copiable values.
    stickerKinds :: Seq.Seq StickerKind.StickerKind,
```

Every builder (record syntax, so `-Werror`'s missing-fields names each; fill every one anyway):
- `Engine/Filter.hs:796`, the player view: `-- CR 123.1: a sticker is on an object, and CR 109.1 makes a player none.` `stickerKinds = Seq.empty,`.
- `Projection/View.viewOfCard`: `-- CR 123.1: a printed face has no object to be on.` `Filter.stickerKinds = Seq.empty,`.
- `Projection/View.viewOfCharacteristics`: `-- CR 123.1: off the object, live, \`designations\`' posture; CR 608.2h's record keeps none.` `Filter.stickerKinds = foldMap (fmap (StickerRef.kind . StickerPlacement.sticker) . Object.stickers) (Game.lookupObject oid gs),`.
- `Count.viewOfSnapshot`: `-- CR 123.1: a snapshot records no sticker, \`designations\`' posture.` `Filter.stickerKinds = Seq.empty,` (add the `Seq` import). `Count.overlaySnapshot` copies only PC-derived fields, so the live value stands; record that.
- Tests: `FilterSpec.hs` 38 and 131, `Support.hs:2429`: `Filter.stickerKinds = Seq.empty`.
- `lastKnownView` reuses `viewOfCharacteristics`; a departed object reads empty. No unit-1 reader asks a last-known sticker; say so in the PR.

- [ ] **Step 6: The card.** `data/cards/croakid-amphibonaut.json` (Kird Ape's condition, Skymarcher Aspirant's `GainKeyword`):

```json
{
  "faces": [
    {
      "manaCost": [
        { "type": "Generic", "value": 4 },
        { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Blue" } } }
      ],
      "name": "Croakid Amphibonaut",
      "oracleText": "This creature has flying as long as you control a stickered permanent.",
      "power": { "type": "Literal", "value": 4 },
      "staticAbilities": [
        {
          "affected": { "type": "Matching", "value": { "type": "IsSource" } },
          "condition": {
            "type": "Compares",
            "value": {
              "comparison": { "type": "AtLeast" },
              "measured": {
                "type": "Count",
                "value": {
                  "aggregation": { "type": "Members" },
                  "filter": { "type": "And", "value": [ { "type": "Stickered" }, { "type": "ControlledBy", "value": { "type": "You" } } ] },
                  "scope": { "type": "InZone", "value": { "player": { "type": "EachPlayer" }, "zone": { "type": "Battlefield" } } }
                }
              },
              "threshold": { "type": "Literal", "value": 1 }
            }
          },
          "modifications": [ { "type": "GainKeyword", "value": { "type": "Flying" } } ]
        }
      ],
      "toughness": { "type": "Literal", "value": 3 },
      "typeLine": { "subtypes": [ { "type": "Frog" }, { "type": "Guest" } ], "types": [ { "type": "Creature" } ] }
    }
  ]
}
```

- [ ] **Step 7: Run** `-p Sticker`, `-p Filter`, the full suite.
- [ ] **Step 8: Mutate.** `m6.sed` on `Engine/Filter.hs`: `s/Filter.Stickered -> not (null (stickerKinds view))/Filter.Stickered -> False/`. Pattern `Sticker`. Expected red: "CR 123.4 flying with no sticker, with one, and after the bounce". The Clone case holds by construction (no copy road reads `Object.stickers`) and has no mutation that keeps the source compiling; report it as a regression fence.
- [ ] **Step 9: Commit.** "Read stickered objects, and add Croakid Amphibonaut (related to #872)".

---

## Task 7: placing a sticker, and Proficient Pyrodancer

**Files:**
- Create: `source/libraries/types/Pawl/Types/PutSticker.hs`, `source/libraries/codec/Pawl/Codec/PutSticker.hs`
- Modify: `source/libraries/types/Pawl/Types/Effect.hs`, `source/libraries/codec/Pawl/Codec/Effect.hs`, `source/libraries/codec/Pawl/Codec/EffectSpec.hs`, every exhaustive `Effect` site (Step 4)
- Modify: `Prompt.hs`, `Response.hs`, `Replay.hs`, `Scenario/Prompt.hs`, `Scenario.hs`, `Scenario/Reply.hs`, `ReplaySpec.hs` (`ChooseSticker`)
- Modify: `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs` (`applyOneEffect`, `effectIsImpossible`)
- Create: `data/cards/proficient-pyrodancer.json`; Modify `pawl.cabal`, `StickerSpec`

**Interfaces:**
- Produces: `PutSticker.MkPutSticker {player :: PlayerRef, ref :: ObjectRef, kinds :: Set StickerKind}`; `Effect.PutSticker PutSticker`; `Prompt.ChooseSticker :: Decider -> PlayerId -> ObjectId -> NonEmpty StickerRef -> Prompt StickerRef`; `Response.ChoseSticker StickerRef`; spec helpers `Offers`, `placing`, `drain`, `pyrodancerEnters`.
- Consumes: `Sticker.available`, `Sticker.put`, `chosenPermanentOf`, `battlefieldMatching`, `objectRefObjects`, `playerRefPlayers`.

- [ ] **Step 1: Verify Oracle.** Proficient Pyrodancer `{2}{R}` Creature — Human Performer 2/3: "When this creature enters, you may put an art sticker on a nonland permanent you own.\n{2}{R}: Another target creature with an art sticker on it gets +2/+0 and gains menace until end of turn."

- [ ] **Step 2: Write the failing tests.** In `StickerSpec`:

```haskell
-- What a placement asked: the permanents offered, the stickers offered and the
-- "may"s, each newest first.
data Offers = MkOffers {permanents :: [[ObjectId.ObjectId]], stickers :: [[StickerRef.StickerRef]], mays :: Int}

-- Accepts every "may", names `onto` where a permanent is chosen and it is
-- offered, and answers ChooseSticker with the FIRST offered, pinned by position.
placing :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> State.State Offers r
placing onto p = case p of
  Prompt.ChooseOptional {} -> do
    State.modify' (\o -> o {mays = mays o + 1})
    pure OptionalDecision.Exercises
  Prompt.ChoosePermanent _ _ _ offered -> do
    State.modify' (\o -> o {permanents = NonEmpty.toList offered : permanents o})
    pure (case onto of Just oid | List.elem oid offered -> oid; _ -> NonEmpty.head offered)
  Prompt.ChooseSticker _ _ _ offered -> do
    State.modify' (\o -> o {stickers = NonEmpty.toList offered : stickers o})
    pure (NonEmpty.head offered)
  _ -> pure (S.identityAnswer p)

-- Settle and resolve until the stack is empty.
drain :: Game.Type.Game ()
drain = do
  Engine.settleForPriority
  stack <- State.gets GameState.stack
  Monad.unless (null stack) (Stack.resolveTop >> drain)

-- A Pyrodancer enters under alice with its enters event, and everything it
-- triggers resolves under `placing onto`.
pyrodancerEnters :: Printing.Printing -> Maybe ObjectId.ObjectId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState, Offers)
pyrodancerEnters pyrodancer onto gs0 =
  let (pyro, entered) = S.entersWithTrigger pyrodancer S.alice gs0
      ((_, after), offers) = State.runState (Engine.runGame (placing onto) entered drain) (MkOffers [] [] 0)
   in (pyro, after, offers)
```

```haskell
  -- The spec's retention bullet as the whole card: Pyrodancer stickers itself
  -- (the only nonland permanent alice owns) and dies to a Bolt.
  Spec.it s "CR 123.9 whole card: Pyrodancer's art sticker is on it, and stays through its death" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let base = withSheets (take 1 sheets) (S.landsFor mountain S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (pyro, stickered, _) = pyrodancerEnters pyrodancer Nothing base
        (boltId, g1) = S.addHandCard bolt S.bob stickered
        killed = S.settleSba (S.runPure (namingTarget pyro) (g1 {GameState.priority = Just S.bob}) (S.cast S.bob boltId >> Stack.resolveTop))
    Spec.assertEqWith s "CR 123.9 Pyrodancer's art sticker is on it" (fmap (fmap (StickerRef.kind . StickerPlacement.sticker) . Foldable.toList . Object.stickers) (Game.lookupObject pyro stickered)) (Just [StickerKind.Art])
    Spec.assertEqWith s "CR 123.5 and on the card in the graveyard" (fmap (Seq.length . fst) (stickersIn Zone.Graveyard killed)) [1]
  -- Review Focus 4. Two copies of one sheet: six art stickers, all distinct.
  -- Two placements on the Piker, then a bounce frees both.
  Spec.it s "CR 123.3/123.3a a used sticker is not offered again until its card reaches a hidden zone" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let sheet = take 1 sheets
        (pikerId, g1) = S.addPermanent piker S.alice (withSheets (sheet <> sheet) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (_, g2, first) = pyrodancerEnters pyrodancer (Just pikerId) g1
        (_, g3, second) = pyrodancerEnters pyrodancer (Just pikerId) g2
        (unsummonId, g4) = S.addHandCard unsummon S.bob g3
        bounced = S.runPure (namingTarget pikerId) (g4 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
        (_, _, third) = pyrodancerEnters pyrodancer Nothing bounced
        offered o = concat (take 1 (stickers o))
    Spec.assertEqWith s "CR 123.3 the used sticker is not offered again" (offered second) (drop 1 (offered first))
    Spec.assertEqWith s "CR 123.5 the bounce frees both" (offered third) (offered first)
    Spec.assertEqWith s "CR 123.3a two copies of one sheet offer six art stickers" (length (offered first)) 6
  -- CR 123.3b through the card's "you own": alice controls bob's Piker, and
  -- only her own nonland permanents are offered.
  Spec.it s "CR 123.3b Pyrodancer offers only a permanent alice owns" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bobsPiker, g1) = S.addPermanent piker S.bob (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))
        (bearsId, g2) = S.addPermanent bears S.alice (S.giveControl bobsPiker S.alice g1)
        (pyro, after, offers) = pyrodancerEnters pyrodancer (Just bearsId) g2
    Spec.assertEqWith s "CR 123.3b bob's Piker is not offered, alice's two permanents are" (fmap List.sort (permanents offers)) [List.sort [pyro, bearsId]]
    Spec.assertEqWith s "and it took no sticker" (fmap (Seq.length . Object.stickers) (Game.lookupObject bobsPiker after)) (Just 0)
  -- Review Focus 1's gameplay half: no sheets, nothing to place, no "may".
  Spec.it s "CR 608.2d with no sheets Pyrodancer's may is not offered" $ do
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    let (pyro, after, offers) = pyrodancerEnters pyrodancer Nothing (Setup.gameWith GameSettings.plain S.bothPlayers)
    Spec.assertEqWith s "CR 608.2d alice was not asked" (mays offers) 0
    Spec.assertEqWith s "and nothing is stickered" (fmap (Seq.length . Object.stickers) (Game.lookupObject pyro after)) (Just 0)
  -- Two Pikers, one stickered: only it is a legal target, and it gets +2/+0
  -- and menace.
  Spec.it s "CR 123.9 Pyrodancer's ability targets only a creature with an art sticker" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (marked, g1) = S.addPermanent piker S.alice (mainPhaseForAlice (S.landsFor mountain S.alice 3 (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (plainPiker, g2) = S.addPermanent piker S.alice g1
        (pyro, g3, _) = pyrodancerEnters pyrodancer (Just marked) g2
        board = mainPhaseForAlice g3
        recording :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems sets))
            pure (namingTarget marked p)
          _ -> pure (S.identityAnswer p)
        ((_, after), offered) = case Activatable.abilitiesFor pyro board of
          [ability] -> State.runState (Engine.runGame recording board (Activate.activateAbility S.alice pyro ability >> Stack.resolveTop)) []
          _ -> (((), board), [])
    Spec.assertEqWith s "CR 115.1 only the Piker with an art sticker is offered" offered [marked]
    Spec.assertEqWith s "it gets +2/+0 and menace" (Projection.powerOf marked after, Projection.hasKeyword Keyword.Menace marked after) (Just 4, True)
    Spec.assertEqWith s "the other Piker is untouched" (Projection.powerOf plainPiker after) (Just 2)
```

The Pyrodancer's ability has no `{T}`, so summoning sickness does not gate it. `recording` carries its own signature because `GADTs` implies `MonoLocalBinds`. Imports: `Control.Monad as Monad`, `Data.Maybe as Maybe`, `Pawl.Types.Game as Game.Type`, `Pawl.Types.OptionalDecision as OptionalDecision`, `Pawl.Types.Printing as Printing`, `Pawl.Types.StickerRef as StickerRef`.

`EffectSpec`, beside "Ante":

```haskell
  -- CR 123.3: who places, onto what, and which kinds.
  Spec.it s "PutSticker" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutSticker (PutSticker.MkPutSticker (PlayerRef.Relative PlayerRelation.You) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))) (Set.singleton StickerKind.Art)))
      " {\"type\":\"PutSticker\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"ref\":{\"type\":\"InSlot\",\"value\":\"self\"},\"kinds\":[{\"type\":\"Art\"}]}} "
```

`ReplaySpec`, beside the Task 4 case:

```haskell
        -- CR 123.3: which sticker the placer chose is a decision.
        Spec.it s "ChooseSticker round-trips through the transcript" $ do
          let a = StickerRef.MkStickerRef S.alice 0 StickerKind.Art 0
              b = StickerRef.MkStickerRef S.alice 1 StickerKind.Art 2
              p = Prompt.ChooseSticker decider S.alice (ObjectId.MkObjectId 7) (a NonEmpty.:| [b])
          Spec.assertEqWith s "choosing the second round trips" (Replay.decode p (Replay.encode p b)) (Just b)
          Spec.assertEqWith s "choosing the first round trips" (Replay.decode p (Replay.encode p a)) (Just a)
          Spec.assertEqWith s "a short transcript places the first offered" (Replay.defaultAnswer p) a
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Effect.PutSticker'".

- [ ] **Step 4: The opcode.** `Pawl/Types/PutSticker.hs`:

```haskell
module Pawl.Types.PutSticker where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: the players `player` names each put an available sticker of
-- one of `kinds` on each named object they own.
--
-- Not implemented: CR 123.3d's move of a sticker already on an object (#N4).
data PutSticker = MkPutSticker
  { player :: PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef,
    kinds :: Set.Set StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
```

`Pawl/Codec/PutSticker.hs`: `Fields.object`, `player` (`PlayerRef.codec`), `ref` (`ObjectRef.codec`), `kinds` (`Common.set StickerKind.codec`), all required, Pawl.Codec.Ante's layout. `Effect.hs`, beside `Ante`: `| -- | CR 123.3: put a sticker on each named object.` `PutSticker PutSticker.PutSticker`.

Sites (`grep -rn 'Effect\.Ante\b' source --include='*.hs'` is the enumeration; answer as `Ante` does unless stated):
- `Codec/Effect.hs`: `Arm.payload "PutSticker" PutSticker.codec Effect.PutSticker (\x -> case x of Effect.PutSticker y -> Just y; _ -> Nothing),`; tagOf `Effect.PutSticker {} -> "PutSticker"`.
- `EffectZone.zoneFunctionedAgainst`: `Nothing`. `ManaAbility.manaProduced`, `playerChoice`: `Nothing`; `movesLibraryCard`: `False` (it moves nothing).
- `Resolve/Slots`: `effectObjectRefs` `[ref]`; `effectPlayerRefs` `[player]`; `slotsOf` `Map.empty`; `ownSlotsAreExhaustive` `True`; `readsX` `False`; `boundSlots` `Set.empty`.
- `Mentions`: `Effect.PutSticker putSticker -> putStickerNames asking putSticker`, with

  ```haskell
  putStickerNames :: Asking -> PutSticker.PutSticker -> Bool
  putStickerNames asking x = case x of
    PutSticker.MkPutSticker player ref _kinds -> playerRefNames asking player || objectRefNames asking ref
  ```

- `Rewrite.rewriteEffect` (SILENT: a constructor match that must descend): `Effect.PutSticker (PutSticker.MkPutSticker player ref kinds) -> Effect.PutSticker (PutSticker.MkPutSticker player (rewriteObjectRef pairs ref) kinds)`.
- `CardSpec`: `objectRefPositions` gains `("put-sticker", Effect.PutSticker (PutSticker.MkPutSticker (PlayerRef.Relative PlayerRelation.You) (plantedRef "ps") (Set.singleton StickerKind.Art)), [plantedRef "ps"]),`; `playerRefPositions` gains `("put-sticker", Effect.PutSticker (PutSticker.MkPutSticker (plantedPlayer "pp") (plantedRef "pp") (Set.singleton StickerKind.Art)), [plantedPlayer "pp"]),`; `ownCounts`, `effectNestedEffects`, `effectReplacements`, `effectMintedFaces` `[]`; `effectFilters` `Effect.PutSticker (PutSticker.MkPutSticker _ ref _) -> frame SourceHostFramed (objectRefFilters ref)`.
- `EffectLintSpec`: `ownQuantities` `[]`; `effectObjectRefs` `Effect.PutSticker (PutSticker.MkPutSticker _ ref _) -> read_ [ref]`.
- `happenedBetween`: no edit. A placement writes `objects` and `nextTimestamp` (copied across); one that places nothing writes nothing, so a "when you do" reads it as not having happened.

- [ ] **Step 5: The prompt.** `Prompt.hs`, after `ChooseDungeon`:

```haskell
  -- | CR 123.3: which available sticker the placer puts on this object.
  -- ChooseDungeon's posture: asked at two or more, filtered rather than trusted.
  ChooseSticker :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty StickerRef.StickerRef -> Prompt StickerRef.StickerRef
```

`Response.hs`: `| -- | CR 123.3: the sticker a player put on an object.` `ChoseSticker StickerRef.StickerRef`. Sites, as Task 4 Step 5: `Replay.encode` `Prompt.ChooseSticker {} -> Response.ChoseSticker answer`; `decode` its `case`; `defaultAnswer` `-- CR 123.3: asked only at two or more, every one available.` `Prompt.ChooseSticker _ _ _ candidates -> NonEmpty.head candidates`; `deciderOf` `Prompt.ChooseSticker decider _ _ _ -> Just (Decider.unwrap decider)`; `kindOf` `"ChooseSticker"`; `withinOffer` `Prompt.Type.ChooseSticker _ _ _ offered -> chosen \`elem\` offered`; `shapeOf` `Prompt.ChooseSticker {} -> viaCodec Codec.StickerRef.codec` (import `Pawl.Codec.StickerRef as Codec.StickerRef`).

- [ ] **Step 6: The resolution arm.** `applyOneEffect`, beside `Effect.Ante`:

```haskell
  -- CR 123.3: each placer chooses a sticker of an allowed kind not on any
  -- object they own and puts it on each named object. CR 123.3b: an object
  -- the placer does not own takes nothing; a regression fence, since the one
  -- producer's own filter already says "you own". Placing nothing writes
  -- nothing, so happenedBetween reads it as not having happened.
  Effect.PutSticker (PutSticker.MkPutSticker player ref kinds) -> do
    named <- case ref of
      ObjectRef.ChosenPermanent (ChosenPermanent.MkChosenPermanent filter_ chooser) -> chosenPermanentOf legal resolving controller source filter_ chooser
      _ -> fmap (\gs -> objectRefObjects legal resolving controller source gs ref) State.get
    placers <- State.gets (\gs -> playerRefPlayers legal controller gs player)
    Monad.forM_ placers $ \placer -> Monad.forM_ (ListUtils.nubOrd named) $ \oid -> do
      gs <- State.get
      let owned = fmap Object.owner (Game.lookupObject oid gs) == Just placer
      Monad.when owned $ case Sticker.available placer kinds gs of
        [] -> pure ()
        [only] -> State.modify' (Sticker.put placer oid only)
        first : rest -> do
          let offered = first NonEmpty.:| rest
          answer <- Game.choose (Prompt.ChooseSticker (Decide.deciderFor placer gs) placer oid offered)
          State.modify' (Sticker.put placer oid (if List.elem answer offered then answer else first))
```

`effectIsImpossible`, beside `Effect.Ante`:

```haskell
  -- CR 608.2d / 123.3: no placer has an available sticker of an allowed kind
  -- and a named object they own. The ChosenPermanent read is the candidate set
  -- chosenPermanentOf offers, the pure sweep answering nothing for it.
  Effect.PutSticker (PutSticker.MkPutSticker player ref kinds) ->
    let placers = playerRefPlayers legal controller gs player
        named = case ref of
          ObjectRef.ChosenPermanent (ChosenPermanent.MkChosenPermanent filter_ _) -> battlefieldMatching legal resolving controller source gs filter_
          _ -> objectRefObjects legal resolving controller source gs ref
        owns pid oid = fmap Object.owner (Game.lookupObject oid gs) == Just pid
        placeable pid = not (null (Sticker.available pid kinds gs)) && any (owns pid) named
     in not (null placers) && not (any placeable placers)
```

Import `Pawl.Engine.Sticker as Sticker`, `Pawl.Types.PutSticker as PutSticker`, `Pawl.Types.ChosenPermanent as ChosenPermanent` if absent. `Pawl.Engine.Sticker` imports only `Game`, so no cycle.

- [ ] **Step 7: The card.** `data/cards/proficient-pyrodancer.json` (Llanowar Augur's pump-and-keyword, Flensing Raptor's "another target creature" filter, Wormfang Crab's `ChosenPermanent`, Aetherplasm's optional clause):

```json
{
  "faces": [
    {
      "activatedAbilities": [
        {
          "cost": {
            "mana": [
              { "type": "Generic", "value": 2 },
              { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Red" } } }
            ]
          },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "ModifyTarget",
                        "value": {
                          "duration": { "type": "UntilEndOfTurn" },
                          "modification": { "type": "ModifyPowerToughness", "value": { "power": { "type": "Literal", "value": 2 }, "toughness": { "type": "Literal", "value": 0 } } },
                          "ref": { "type": "InSlot", "value": "target" }
                        }
                      },
                      {
                        "type": "ModifyTarget",
                        "value": {
                          "duration": { "type": "UntilEndOfTurn" },
                          "modification": { "type": "GainKeyword", "value": { "type": "Menace" } },
                          "ref": { "type": "InSlot", "value": "target" }
                        }
                      }
                    ]
                  }
                ],
                "targetSlots": {
                  "target": {
                    "filter": { "type": "And", "value": [ { "type": "Not", "value": { "type": "IsSource" } }, { "type": "HasSticker", "value": { "type": "Art" } } ] },
                    "pool": { "type": "Creatures" }
                  }
                }
              }
            ]
          }
        }
      ],
      "manaCost": [
        { "type": "Generic", "value": 2 },
        { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Red" } } }
      ],
      "name": "Proficient Pyrodancer",
      "oracleText": "When this creature enters, you may put an art sticker on a nonland permanent you own.\n{2}{R}: Another target creature with an art sticker on it gets +2/+0 and gains menace until end of turn.",
      "power": { "type": "Literal", "value": 2 },
      "toughness": { "type": "Literal", "value": 3 },
      "triggeredAbilities": [
        {
          "condition": { "type": "SelfEnters" },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "PutSticker",
                        "value": {
                          "kinds": [ { "type": "Art" } ],
                          "player": { "type": "Relative", "value": { "type": "You" } },
                          "ref": {
                            "type": "ChosenPermanent",
                            "value": {
                              "chooser": { "type": "Relative", "value": { "type": "You" } },
                              "filter": { "type": "And", "value": [ { "type": "Not", "value": { "type": "HasCardType", "value": { "type": "Land" } } }, { "type": "OwnedBy", "value": { "type": "You" } } ] }
                            }
                          }
                        }
                      }
                    ],
                    "optionality": { "type": "Optional" }
                  }
                ]
              }
            ]
          }
        }
      ],
      "typeLine": { "subtypes": [ { "type": "Human" }, { "type": "Performer" } ], "types": [ { "type": "Creature" } ] }
    }
  ]
}
```

- [ ] **Step 8: Run** `-p Sticker`, `-p Effect`, `-p Replay`, the full suite.
- [ ] **Step 9: Mutate.**
  - `m7a.sed` on `Resolve/Effect.hs`: `s/      Monad.when owned \$ case Sticker.available placer kinds gs of/      Monad.when (owned \&\& False) $ case Sticker.available placer kinds gs of/` (keeps `owned` referenced). Pattern `Sticker`. Expected red: "CR 123.9 Pyrodancer's art sticker is on it".
  - `m7b.sed` on `Engine/Sticker.hs`: `s/     in filter (\\ref -> Set.notMember ref used) offered/     in filter (\\ref -> used == used) offered/`. Expected red: "CR 123.3 the used sticker is not offered again". (More permissive; finite, cannot loop.)
  - `m7c.sed` on `Resolve/Effect.hs`, anchored to the PutSticker arm: `/Effect.PutSticker (PutSticker.MkPutSticker player ref kinds) ->$/,/in not (null placers)/s/     in not (null placers) && not (any placeable placers)/     in null placers \&\& not (any placeable placers)/`. Expected red: "CR 608.2d alice was not asked".
  - `m7d.sed` on `Engine/Filter.hs`: `s/Filter.HasSticker kind -> elem kind (stickerKinds view)/Filter.HasSticker kind -> kind \/= kind/`. Expected red: "CR 115.1 only the Piker with an art sticker is offered".
  - The CR 123.3b test: mutate the opcode's `owned` toward MORE permissive (`let owned = True`) and expect NOTHING red, since the card's `OwnedBy` filter narrows first; record it as the regression fence Divergence 10 names. Then `m7e.sed` on the card, `/"OwnedBy"/d` after formatting puts it on its own line, expecting red "CR 123.3b bob's Piker is not offered, alice's two permanents are"; that proves the transcription.
- [ ] **Step 10: Commit.** "Put art stickers, and add Proficient Pyrodancer (related to #872)".

---

## Task 8: the placement event, its trigger, and Wee Champion

**Files:**
- Create: `source/libraries/types/Pawl/Types/StickerPut.hs`, `source/libraries/types/Pawl/Types/PlacesSticker.hs`, their codecs
- Modify: `GameEvent.hs` + `Codec/GameEvent.hs` + `GameEventSpec.hs`; `TriggerCondition.hs` + `Codec/TriggerCondition.hs` + `TriggerConditionSpec.hs`
- Modify: every exhaustive `GameEvent` and `TriggerCondition` site (Step 4); `ZoneTriggerSpec.hs` (`representativeEvents`, `everyTriggerCondition`); `Engine/Sticker.hs` (`put` records the event)
- Create: `data/cards/wee-champion.json`

**Interfaces:**
- Produces: `StickerPut.MkStickerPut {placer :: PlayerId, object :: ObjectId, kind :: StickerKind}`; `GameEvent.StickerPut StickerPut`; `PlacesSticker.MkPlacesSticker {placer :: PlayerRelation, kinds :: Set StickerKind}`; `TriggerCondition.PlacesSticker PlacesSticker`.

- [ ] **Step 1: Verify Oracle.** Wee Champion `{R}` Creature — Minotaur Child Guest 0/1: "Whenever you place a sticker, this creature gets +1/+1 until end of turn. If it's an art sticker, instead put a +1/+1 counter on this creature."

- [ ] **Step 2: Write the failing tests.** In `StickerSpec`:

```haskell
  -- Divergence 6: an art placement triggers the art ability alone, so one
  -- counter and no pump; both firing would read power 2.
  Spec.it s "CR 123.9 an art sticker puts one +1/+1 counter on Wee Champion and no pump" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    wee <- S.printingOf s registry "Wee Champion"
    let (weeId, g1) = S.addPermanent wee S.alice (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))
        (pyro, after, _) = pyrodancerEnters pyrodancer Nothing g1
    Spec.assertEqWith s "CR 123.9 Wee Champion is a 1/2 with one +1/+1 counter" (Projection.powerOf weeId after, S.counterOf CounterKind.PlusOnePlusOne weeId after) (Just 1, 1)
    Spec.assertEqWith s "one art sticker went on alice's two permanents" (sum (fmap (\oid -> maybe 0 (Seq.length . Object.stickers) (Game.lookupObject oid after)) [weeId, pyro])) 1
```

Which of the two takes the sticker is `ChoosePermanent`'s head; either proves the trigger. Import `Pawl.Types.CounterKind as CounterKind`.

`GameEventSpec`, after "Firebent":

```haskell
  Spec.it s "StickerPut" $
    Common.assertCodec
      s
      GameEvent.codec
      (GameEvent.StickerPut (StickerPut.MkStickerPut (PlayerId.MkPlayerId 9) (ObjectId.MkObjectId 3) StickerKind.Art))
      " {\"type\":\"StickerPut\",\"value\":{\"placer\":9,\"object\":3,\"kind\":{\"type\":\"Art\"}}} "
```

`TriggerConditionSpec`, after "PlayerFirebends round-trips":

```haskell
  Spec.it s "PlacesSticker round-trips" $
    Common.assertCodec
      s
      TriggerCondition.codec
      (TriggerCondition.PlacesSticker (PlacesSticker.MkPlacesSticker PlayerRelation.You (Set.singleton StickerKind.Art)))
      " {\"type\":\"PlacesSticker\",\"value\":{\"placer\":{\"type\":\"You\"},\"kinds\":[{\"type\":\"Art\"}]}} "
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'GameEvent.StickerPut'".

- [ ] **Step 4: The event and the condition.** Types:

```haskell
module Pawl.Types.StickerPut where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: a player put a sticker of this kind on this object.
data StickerPut = MkStickerPut
  { placer :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId,
    kind :: StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
```

```haskell
module Pawl.Types.PlacesSticker where

import qualified Data.Set as Set
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: "whenever [a player] places a sticker", narrowed to these
-- kinds, PermanentGetsCounters' posture for a counter kind.
data PlacesSticker = MkPlacesSticker
  { placer :: PlayerRelation.PlayerRelation,
    kinds :: Set.Set StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
```

Codecs: `Fields.object`, fields as declared (`PlayerId.codec`, `ObjectId.codec`, `StickerKind.codec`; `PlayerRelation.codec`, `Common.set StickerKind.codec`). `GameEvent.hs`: append after `CardArrived`, `| -- | CR 123.3.` `StickerPut StickerPut.StickerPut`. `TriggerCondition.hs`, after `PlayerFirebends`: `| -- | CR 123.3: Wee Champion's "whenever you place a sticker".` `PlacesSticker PlacesSticker.PlacesSticker`. Both codecs are `Arm.tagged`: add the `Arm.payload` and the `tagOf` arm beside `Firebent` / `PlayerFirebends`.

`Sticker.put` now records the event (Task 5 left `_placer` for this): rename `_placer` to `placer` and end with

```haskell
   in Event.recordEvent (GameEvent.StickerPut StickerPut.MkStickerPut {StickerPut.placer = placer, StickerPut.object = oid, StickerPut.kind = StickerRef.kind ref}) placed
```

where `placed` is Task 5's result. `Pawl.Engine.Sticker` now imports `Pawl.Engine.Event`; `Event` imports only `Game` for the restamp, so there is no cycle. Confirm with `grep -n 'import.*Pawl.Engine.Sticker' source/libraries/engine/Pawl/Engine/Event.hs` (expected none).

GameEvent sites (`grep -rn 'GameEvent\.Firebent\b' source --include='*.hs'` is the enumeration). In `Event/Match.hs` every `GameEvent.Firebent _ -> False` and `GameEvent.Firebent _ -> Nothing` gains a same-indented `GameEvent.StickerPut _ -> False` / `-> Nothing` after it: write `$SCRATCH/add_sticker_put.py`

```python
import re, sys
path = sys.argv[1]
src = open(path).read().split("\n")
out = []
for line in src:
    out.append(line)
    m = re.match(r"^(\s*)GameEvent\.Firebent _ -> (False|Nothing)$", line)
    if m:
        out.append(m.group(1) + "GameEvent.StickerPut _ -> " + m.group(2))
open(path, "w").write("\n".join(out))
```

run it as its own command, `python3 -I "$SCRATCH/add_sticker_put.py" source/libraries/engine/Pawl/Engine/Event/Match.hs`, then read `git diff --stat` and spot-check: the count of inserted lines equals `grep -c 'GameEvent.Firebent _ -> \(False\|Nothing\)'`. The `PlayerFirebends` arm's `GameEvent.Firebent bender -> ...` gets `GameEvent.StickerPut _ -> False` by hand. Then the new matcher, a copy of the `PlayerFirebends` arm whose Firebent row is `False` and whose StickerPut row is:

```haskell
  -- CR 123.3: a player the relation names put a sticker of one of these kinds.
  TriggerCondition.PlacesSticker (PlacesSticker.MkPlacesSticker relation kinds) -> case event of
    ...
    GameEvent.StickerPut put -> PlayerRelation.holds (Game.teams gs) relation you (StickerPut.placer put) && Set.member (StickerPut.kind put) kinds
```

Other GameEvent sites, by function: `Count.snapshotView` `Nothing`; `Game.castOf`, `abilityResolved`, `discardOf`, `movedChange`, `damageDealt`, `lifeGainOf` `Nothing`; `Event.damageOf`, `revealOf`, `abilityTriggeredOf` `Nothing`; `Match.countersRemovedFrom` `Nothing`, `boundDeparts` `False`; `Trigger.movedOf` `Nothing`; `Trigger.participants` `GameEvent.StickerPut put -> ([StickerPut.object put], [StickerPut.placer put])`; the four `eventTriggers` sub-arms `Map.empty`; `Trigger.resnapshot` `Just event`. Wildcard sites read and right: `Fragmented.bent` and its siblings (a sticker is not their event); `Fragmented.historyReaders` needs no entry (the trigger reads the current action's events, which that function's haddock exempts).

TriggerCondition sites (`grep -rn 'TriggerCondition.PlayerFirebends' source --include='*.hs'`): `Trigger.looksBack` `False`; `batchScoped` `False`; `zonesTriggeredFrom` `battlefield`; `stateTriggers` `False`; `Event.controllerTurnScoped` `False`; `Rewrite` `-> condition`; `Mentions` `TriggerCondition.PlacesSticker _placesSticker -> False`; `Binding.eventBindingSlots` `Set.empty` (pinned by `Pawl.LeavesTriggerSpec`'s "eventBindingSlots names exactly"); `CardSpec.triggerConditionCounts`, `triggerConditionFilters`, `triggerConditionSlots` `[]`. SILENT: `Binding.eventBindings`' fallthrough answers `Map.empty` for the new pair, which is right (the condition binds nothing). Hand-kept lists in `ZoneTriggerSpec`: `representativeEvents` gains `TriggerCondition.PlacesSticker _ -> one (GameEvent.StickerPut StickerPut.MkStickerPut {StickerPut.placer = S.bob, StickerPut.object = departed, StickerPut.kind = StickerKind.Art})` (use the object id the `PermanentGetsCounters` row uses), and `everyTriggerCondition` gains `TriggerCondition.PlacesSticker (PlacesSticker.MkPlacesSticker PlayerRelation.You (Set.singleton StickerKind.Art))`.

- [ ] **Step 5: The card.** `data/cards/wee-champion.json`:

```json
{
  "faces": [
    {
      "manaCost": [ { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Red" } } } ],
      "name": "Wee Champion",
      "oracleText": "Whenever you place a sticker, this creature gets +1/+1 until end of turn. If it's an art sticker, instead put a +1/+1 counter on this creature.",
      "power": { "type": "Literal", "value": 0 },
      "toughness": { "type": "Literal", "value": 1 },
      "triggeredAbilities": [
        {
          "condition": { "type": "PlacesSticker", "value": { "kinds": [ { "type": "Name" }, { "type": "Ability" }, { "type": "PowerToughness" } ], "placer": { "type": "You" } } },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "ModifyTarget",
                        "value": {
                          "duration": { "type": "UntilEndOfTurn" },
                          "modification": { "type": "ModifyPowerToughness", "value": { "power": { "type": "Literal", "value": 1 }, "toughness": { "type": "Literal", "value": 1 } } },
                          "ref": { "type": "InSlot", "value": "self" }
                        }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        },
        {
          "condition": { "type": "PlacesSticker", "value": { "kinds": [ { "type": "Art" } ], "placer": { "type": "You" } } },
          "modal": {
            "modes": [
              {
                "clauses": [
                  {
                    "effects": [
                      {
                        "type": "PutCounters",
                        "value": { "kind": { "type": "PlusOnePlusOne" }, "quantity": { "type": "Literal", "value": 1 }, "ref": { "type": "InSlot", "value": "self" } }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        }
      ],
      "typeLine": { "subtypes": [ { "type": "Child" }, { "type": "Guest" }, { "type": "Minotaur" } ], "types": [ { "type": "Creature" } ] }
    }
  ]
}
```

Subtypes are in `Subtype`'s declaration order (Child, Guest, Minotaur); `format-json.sh` sorts keys only, so the order is the codec's, which `Pawl.CardsSpec` checks.

- [ ] **Step 6: Run** `-p Sticker`, `-p Trigger`, `-p GameEvent`, the full suite.
- [ ] **Step 7: Mutate.**
  - `m8a.sed` on `Event/Match.hs`: `s/(StickerPut.placer put) && Set.member (StickerPut.kind put) kinds/(StickerPut.placer put)/`. Pattern `Sticker`. Expected red: "CR 123.9 Wee Champion is a 1/2 with one +1/+1 counter" (both triggers: power 2). More permissive, but a trigger fires once per event, so it cannot loop.
  - `m8b.sed` on `Engine/Sticker.hs`: `s/   in Event.recordEvent (GameEvent.StickerPut/   in const placed (GameEvent.StickerPut/`. Expected red: the same assertion, reading (Just 0, 0).
- [ ] **Step 8: Commit.** "Trigger on placing a sticker, and add Wee Champion (related to #872)".

---

## Task 9: file the unit-1 issues

**Files:** none but the citations: replace `#N1`–`#N4` in `Setup.hs`, `Engine/Sticker.hs` and `Types/PutSticker.hs`.

- [ ] **Step 1: Write the bodies** to `$SCRATCH/issue-N.md`, each opening with the dated summary, then file each with `gh issue create --title ... --label ... --body-file $SCRATCH/issue-N.md`.

N1, title "Sticker decks are not checked for ten unique sheets", labels `gap,expires:subsystem,area:cards`:

```markdown
> **Summary (2026-10-09).** In constructed play, a player who plays with stickers must bring at least ten sticker sheets, all different. Pawl accepts any number, duplicates included, and draws three. Checking it is deck validation, which pawl does not do yet; it lands with that check. Proficient Pyrodancer in a constructed deck that brings two sheets is the case that should be refused.

### Details

- CR 123.2a. `Pawl.Engine.Setup.drawStickerSheets` takes `Player.stickerSheets` as given; the elision is cited there.
- Related to #4458, which is the deck-legality check this would join.
```

Then record the dependency: `gh api -X POST repos/tfausak/pawl/issues/<N1>/dependencies/blocked_by -F issue_id="$(gh api repos/tfausak/pawl/issues/4458 --jq .id)"`.

N2, title "Unclear whether a Shahrazad subgame uses sticker sheets", labels `question,area:variants`:

```markdown
> **Summary (2026-10-09).** When a Shahrazad subgame starts, the rules say each new game follows the start-of-game steps, but the sticker-sheet step does not say whether a subgame draws its own sheets. Pawl draws three again. The rules also leave open whether stickers on main-game cards count as used inside the subgame, and whether a stickered commander keeps its stickers as it moves into the subgame; pawl says no to both. Shahrazad with Proficient Pyrodancer is the card pair that shows all three.

### Details

- CR 103.2d, CR 729.2, CR 729.2c, CR 123.3. `Pawl.Engine.Setup.drawStickerSheets` reruns in `startGameFromCards`; `Pawl.Engine.Sticker.available` reads the subgame's own objects; `toCommandCard` rebuilds through `Object.newIncarnation`.
```

N3, title "A card that changes owners frees its stickers for its first owner", labels `question,area:cards`:

```markdown
> **Summary (2026-10-09).** Stickers come from a player's own sheets, and a player cannot reuse a sticker that is on a card they own. If a stickered card changes owners (Darkpact, Tempest Efreet), its stickers are on a card its first owner no longer owns, so pawl lets them use those stickers again. No rule addresses it. Darkpact taking a card that Proficient Pyrodancer stickered is the case.

### Details

- CR 123.3, CR 108.3. `Pawl.Engine.Sticker.available` subtracts the stickers on objects the player owns; the elision is cited there.
```

N4, title "Nothing can move a sticker to another object", labels `gap,expires:card-driven,area:effects`:

```markdown
> **Summary (2026-10-09).** The rules say a sticker moved from one object to another does not pay its ticket cost again. No printed card moves or removes a sticker, so pawl has no instruction for it. A card that does would be added here.

### Details

- CR 123.3d. Scryfall `o:sticker o:move` and `o:sticker o:remove`, both empty 2026-10-09; a card that would refute this prints "move a sticker".
- `Pawl.Types.PutSticker` cites this issue.

### Synthetic card draft

Synthetic Sticker Swap {1} Artifact: "{1}, {T}: Move a sticker from a permanent you own to another permanent you own." It needs a `MoveSticker` opcode reading `Object.stickers` and the CR 123.3d waiver.
```

- [ ] **Step 2: Cite them.** Replace `#N1`–`#N4` with the filed numbers; `grep -rn '#N[0-9]' source data docs` must return nothing.
- [ ] **Step 3: Commit.** "Cite the stickers unit-1 follow-ups (related to #872)".

---

## Task 10: sweeps, self-review and the PR

- [ ] **Step 1: Sweeps.**
  - `grep -rn '872' source docs data --include='*.hs' --include='*.md' --include='*.json'`. Expected: `Keyword.hs`'s sticker-kicker elision, and this unit's meld/merge/restamp elisions; the `Player.outsideTheGame` one is gone.
  - Capability-widening sweep (`docs/agents/implementing.md`): `grep -rn -i 'sticker\|ticket' source --include='*.hs'` and read every comment; `grep -rn 'Energy, Poison\|player counters are' source --include='*.hs'` for prose listing the player counter kinds.
  - Counting absolutes: `grep -rn 'ChosenPermanent' source --include='*.hs'` comments that count its producers (Wormfang Crab, Mirkwood Trapper) are now falsified by Pyrodancer; fix them. Same for `GainPlayerCounters` producer counts.
  - CR citations: for each rule in `git diff origin/main -U0 | grep -o 'CR [0-9][0-9.a-z/-]*' | sort -u`, read it in `docs/rules.txt`.
- [ ] **Step 2: Self-review.** `git fetch`, `git merge origin/main`, run the full suite, re-run m4, m5a, m5c, m6, m7b, m7d, m8a. Re-read every comment the diff touched against the one-line haddock rule. Read `gh api repos/tfausak/pawl/issues/872/dependencies/blocking` and name what this unblocked.
- [ ] **Step 3: Open the PR.** `gh pr create --draft --title "Stickers unit 1: tickets, sheets and art stickers" --body-file $SCRATCH/pr.md`, the body:

```
- What and why: unit 1 of the stickers spec, related to #872 (open until unit 4). Ticket counters with Blorbian Buddy and Ticket Turbotubes; four sticker sheets, their loader and Oracle lint; CR 103.2d's sheet draw; Object.stickers with CR 123.5 retention and CR 613.7k restamps; Effect.PutSticker with Proficient Pyrodancer; Filter.HasSticker/Stickered with Croakid Amphibonaut; GameEvent.StickerPut/TriggerCondition.PlacesSticker with Wee Champion. Commits the spec and this plan.
- CR: 103.2d, 107.17, 123.1-123.5, 123.9, 400.7m, 608.2d, 613.7k, 727.2, 729.2.
- Design calls: the twelve Divergences in docs/superpowers/plans/2026-10-09-stickers-unit-1.md, chiefly brought+chosen sheets on Player, no optional/cap/paid on PutSticker (Clause.optionality; unit 3), Wee Champion as two kind-split triggers.
- Verified: suite <before> -> <after>. Mutations: <m1 ... m8b, each with the assertion it reddened, named>. Regression fences: CR 123.3b's opcode gate (Pyrodancer's OwnedBy narrows first), the Clone case (no copy road reads Object.stickers).
- Sites read and right as they stand: happenedBetween, Interchangeable.objects, Rewrite.rewriteEffect (arm added), eventBindings' fallthrough, overBoundSlots/boundSlots/renameBound, overlaySnapshot, lastKnownView, Fragmented.historyReaders, the positional MkObject sites (5), copiableCharacteristics.
- Does the rules core case on an effect's identity? No: it cases on StickerKind (CR 123.1).
- Deferred: name, ability and P/T stickers and ticket costs (units 2-3); meld/merge/sticker kicker (unit 4, cited #872); #N1-#N4.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
```

Leave it a draft if a drain loop dispatched you; otherwise mark it ready per CLAUDE.md. Report and stop.
