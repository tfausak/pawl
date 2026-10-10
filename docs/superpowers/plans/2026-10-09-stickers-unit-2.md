# Stickers unit 2: name stickers — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land unit 2 of the stickers spec: name stickers as a CR 123.6 / 612.9 layer-3 text change at each placement's timestamp, the controller's position prompt (CR 123.6b), words and blanks (CR 123.6a), several names, face-down names (CR 708.2), and the letter, unique-vowel, name-sticker and word reads (CR 123.6d–e). Proved by Baaallerina, Sword-Swallowing Seraph, Wizards of the _____, Wolf in _____ Clothing, Angelic Harold, _____ _____ _____ Trespasser, _____ Balls of Fire, _____-o-saurus, and CR 123.6c's three examples (Fae of Wishes; It That Betrays with Seeker of the Way and Mirrorweave; Witness Protection). It does not close #872.

**Architecture:** `StickerPlacement` gains `position`, written by `Sticker.put`. A pure `Pawl.Engine.NameWords` owns CR 123.6a's words, the insertion, and the letter and vowel counts. `Game.stickerWords` reads a name sticker's words off its owner's sheet. `Projection.stickerGathered` emits one layer-3 `Modification.InsertNameWords` per name sticker at the placement's timestamp, joined into `gatherGiven`, so it folds in timestamp order with `AddNamesMatching`, `HasFullText` and the new `Modification.SetName` (CR 612.8, Witness Protection), over the copiable values. `Effect.PutSticker` asks the object's controller `Prompt.ChooseNamePosition` and may bind the placed sticker (`bound`) into a new `Binding.sticker` field, which `Filter.Context.slotStickers` carries to `Quantity.UniqueVowelsOnSticker`. `Filter.View.nameStickers` feeds `Quantity.LettersOnNameStickers` and `Quantity.NameStickers`. `Filter.NameWordsAtLeast` reads `View.names`. `PlacesSticker` gains an `object` filter.

**Tech Stack:** Haskell (GHC via the repo's nix flake), cabal, tasty.
**Spec:** docs/superpowers/specs/2026-10-09-stickers-design.md
**Unit 1:** docs/superpowers/plans/2026-10-09-stickers-unit-1.md (its Divergences 2 and 4 hold: `StickerRef` is kind + index within kind; the position field lands here with its writer).

## Global Constraints

- Work only in `/Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-2` (branch `872-stickers-unit-2`). Never `cd` to the primary checkout.
- Before the first build, as a bare command: `/Volumes/nvme/Developer/pawl/script/warm-worktree.sh seed /Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-2`. It copies `cabal.project.local` too; confirm the file is there.
- `$SCRATCH` is the session scratchpad. Prefix every `cabal` call with `script/with-build-lock.sh`, redirect to `$SCRATCH/<name>.log`, read the file. Never pipe `cabal`.
- Build: `script/with-build-lock.sh cabal build -v0 all > "$SCRATCH/build.log" 2>&1`.
- Subtree: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes -p Sticker' > "$SCRATCH/sticker.log" 2>&1`. Every pattern below is one word; for one holding a space, run the built binary (`find dist-newstyle -name pawl-test-suite -type f -perm -u+x`) with `pawl_datadir=$PWD/data`.
- Full suite before each commit: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes' > "$SCRATCH/suite.log" 2>&1`. Record the count before Task 1.
- Mutations build with `cabal.project.local`'s `-Werror`, so a mutation must leave no binding unused. Write the sed script to `$SCRATCH/mN.sed`, run `script/with-build-lock.sh script/mutate.sh FILE @$SCRATCH/mN.sed Sticker`, one at a time, alone in the checkout. Name the assertion that reddened and confirm it is the one named below; a green or a different red is a finding, diagnosed before moving on.
- The rules core never cases on an effect's identity. `Effect.PutSticker` is cased only in `Pawl.Engine.Resolve*` and the exhaustive classification tables. The core cases on `StickerKind` (CR 123.1), never on a sheet or a sticker's text; the letter, vowel and word reads are open-half `Quantity`/`Filter` arms.
- One type per `Pawl.Types.<TypeName>` module, type and instances only. Constructors take `Mk`. A haddock is one line plus its CR citation, except an elision paragraph or a note naming the proving test.
- An elision gets an issue and a code-site comment saying only what is not implemented, ending `(#N)`. `#N1`–`#N6` are placeholders Task 8 replaces.
- Read CR text from `docs/rules.txt` by rule number. Re-verify Oracle with `curl -s 'https://api.scryfall.com/cards/named?exact=<Name>'` (URL-encode the name; one request at a time) before writing a card; the texts below are Scryfall's of 2026-10-09.
- Stage, `hooky fix`, stage again. A new module: stage `pawl.cabal`, run `cabal-gild pawl.cabal`. Card JSON: `script/format-json.sh fix FILE`. A card file is named by `Pawl.Slug.fromText` of its name: a run of underscores becomes one `_` (`Wizards of the _____` → `wizards-of-the-_.json`); CardSpec's slug lint names the expected file if one is wrong.
- Never stash; copy aside and move back. Never `git checkout <file>` to revert.
- Extensions only from `.hlint.yaml`. `case` over `maybe`, `let` over `where`, no backticks in new code.
- Commit: a subject, at most two sentences of why, then `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Never put close, fix or resolve beside #872; write "related to #872".
- Gameplay tests are Haskell specs in `Pawl.StickerSpec` with test-local answerers (unit 1's Divergence 12). Sheet positions in `committedSheets`' order: 0 Night Brushwagg Ringmaster, 1 Slimy Burrito Illusion, 2 Contortionist Otter Storm, 3 Ancestral Hot Dog Minotaur.

## Review Focus

These are the five input classes most likely to bite. Each has a test in its owning task.

1. **Timestamp order inside layer 3, and the copy beneath it.** A text change later than the sticker hides the word; one earlier does not; a copy effect (layer 1) changes the text the sticker modifies. Tests: Task 3, "CR 123.6c/613.7 a later Witness Protection hides the word, and a sticker placed after it shows" and "CR 123.6c It That Betrays Otter, as a copy of Seeker of the Way, is Seeker of the Otter Way".
2. **Not copiable.** A name sticker is a layer-3 effect, not a copiable value. Test: Task 2, "CR 123.1/707.2 a Clone of Grizzly Otter Bears is named Grizzly Bears".
3. **Who chooses the position, and when nobody is asked.** The controller, not the placer; a nameless object has one position and takes the word as its name. Tests: Task 4, "CR 123.6b the controller, not the placer, chooses where the word goes" and "CR 708.2/123.6b a face-down permanent is named Night, and face up Night Ainok Tracker".
4. **Words and blanks.** A blank is not a word and does not count toward k; the word goes before a blank that follows word k. Test: Task 5, "CR 123.6a Wolf in _____ Clothing offers four positions, and the word goes before a following blank".
5. **"That sticker" across a reflexive trigger, with Y and case.** The binding reaches a "when you do" ability's target count; Y is a vowel; case is ignored. Tests: Task 5, "CR 123.6e Slimy has two unique vowels, Y one of them: Wolf in _____ Clothing kills two of bob's Pikers" and "CR 123.6e Otter has two unique vowels, its O capital: Wizards of the _____ looks at two cards".

## Divergences from the spec (decided here; the PR body repeats them)

1. **The position prompt is skipped only at one position.** The spec skips it "when every position yields the same name". Positions that give the same name now ("Otter" on an object named Otter) give different names after a later change (CR 123.6c's It That Betrays example), so they are distinguishable and CLAUDE.md forbids eliding them. Only an object whose names hold no word, which has one position, is not asked.
2. **Letter counts land as Balls of Fire's specific letters, and the name-sticker count as Trespasser's, without a letter condition.** Fight the _____ Fight needs "enchanted creature fights" (no `Effect.Fight` road names a host) and Last Voyage of the _____ needs "becomes an Aura with enchant creature" plus a return-and-attach: neither is a sticker capability. _____ Balls of Fire proves CR 123.6d itself ("the number of o's", and Otter's capital O proves case); its "whenever you put a sticker on this enchantment" needs `PlacesSticker.object`, which unit 3's Tusk and Whiskers also needs. The letter condition on the count is filed (#N3) with Fight the _____ Fight as its card.
3. **Witness Protection brings `Modification.SetName`.** CR 612.8's name-setting effect does not exist; Witness Protection exercises it.
4. **Mirrorweave is the becomes-a-copy card** for CR 123.6c's second example: it is in the pool and makes every other creature a copy of a nonlegendary one. It That Betrays and Seeker of the Way are added whole.
5. **Several names: positions run to the longest name's word count**, and each name takes the word after k of its words or at its end. Filed as #N2 with the spec's insert-into-each reading.
6. **"That sticker" is a `Binding.sticker` field read through a `Filter.Context.slotStickers` map**, filled by `Resolve.Slots.effectContext` and `Target.slotContext` only. The intervening-if contexts (`Stack.interveningStillHolds`, `Event.Trigger`'s CR 603.4 check) keep `contextFor`'s empty map: no card reads a sticker in an intervening "if".
7. **A name sticker's words are baked into `InsertNameWords` at the gather** (`Game.stickerWords`), so the apply arm reads no sheet. The arm is engine-minted, `Quantity.StationMeasure`'s posture; nothing stops card JSON authoring it.
8. **`View.nameStickers` beside unit 1's `stickerKinds`**, filled in every `MkView` builder; `Count.viewOfSnapshot` leaves it empty under #4890 with `stickerKinds`.
9. **No test of a face-down exiled card's stickers.** No pool card exiles a battlefield or graveyard card face down (data/cards "exile … face down", 2026-10-09: Ignorant Bliss, foretell, Extract Power, Ethereal Valkyrie, all from a hand or library, where the sticker is already gone). Unit 1's write-back keys on the zone, not the facing.
10. **Several cards place by `Sticker.put` in their tests** (Fae of Wishes in exile, It That Betrays, the Witness Protection pair, Spy Kit's host): no unit-2 card puts a name sticker on a card in exile, and placing directly isolates CR 123.6c's ordering from the prompt.

---

## Task 1: words, positions and a sticker's words

**Files:**
- Create: `source/libraries/engine/Pawl/Engine/NameWords.hs`; Modify: `pawl.cabal`
- Modify: `source/libraries/types/Pawl/Types/StickerPlacement.hs`, `source/libraries/codec/Pawl/Codec/StickerPlacement.hs`, `source/libraries/codec/Pawl/Codec/ObjectSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Sticker.hs`, `source/libraries/engine/Pawl/Engine/Game.hs`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `NameWords.isBlank :: Text -> Bool`, `NameWords.wordsOf :: CardName -> [Text]`, `NameWords.wordCount :: CardName -> Natural`, `NameWords.insertAfter :: Natural -> Text -> CardName -> CardName`, `NameWords.letterCount :: Text -> Text -> Natural`, `NameWords.uniqueVowels :: Text -> Natural`; `StickerPlacement.position :: Maybe Natural`; `Sticker.put :: PlayerId -> ObjectId -> StickerRef -> Maybe Natural -> GameState -> GameState`; `Game.stickerWords :: StickerRef -> GameState -> Maybe Text`; spec constants `night`, `slimy`, `otter`.
- Consumes: `Pawl.Extra.Natural.length`, `Natural.toInt`, `Player.stickerSheets`, `StickerSheet.names`.

- [ ] **Step 0: Commit this plan** alone: "Add the stickers unit 2 plan".

- [ ] **Step 1: Write the failing tests.** In `Pawl.StickerSpec`, add the imports `qualified Pawl.Engine.NameWords as NameWords` and `qualified Pawl.Types.CardName as CardName`, these top-level helpers after `stickerOn`:

```haskell
-- One of alice's name stickers, by its sheet's position in committedSheets'
-- order and its index among that sheet's name stickers.
nameSticker :: Natural -> Natural -> StickerRef.StickerRef
nameSticker slot i = StickerRef.MkStickerRef {StickerRef.owner = S.alice, StickerRef.sheet = slot, StickerRef.kind = StickerKind.Name, StickerRef.index = i}

-- "Night", "Slimy" and "Otter".
night :: StickerRef.StickerRef
night = nameSticker 0 0

slimy :: StickerRef.StickerRef
slimy = nameSticker 1 0

otter :: StickerRef.StickerRef
otter = nameSticker 2 1

-- The names an object shows, as text.
nameTexts :: ObjectId.ObjectId -> GameState.GameState -> [Text.Text]
nameTexts oid gs = fmap CardName.unwrap (Set.toList (Projection.namesOf oid gs))
```

change `stickerOn`'s placement to `Sticker.put S.alice oid ref Nothing gs`, and add these cases at the end of `spec`:

```haskell
  -- CR 123.6a over the three card names this unit adds that hold a blank.
  Spec.it s "CR 123.6a a blank is not a word, and _____-o-saurus is one" $ do
    let named = CardName.MkCardName . Text.pack
    Spec.assertEqWith s "CR 123.6a Wolf in _____ Clothing has three words" (NameWords.wordCount (named "Wolf in _____ Clothing")) 3
    Spec.assertEqWith s "CR 123.6a _____-o-saurus is one hyphenated word" (NameWords.wordCount (named "_____-o-saurus")) 1
    Spec.assertEqWith s "CR 123.6a three blanks and Trespasser are one word" (NameWords.wordCount (named "_____ _____ _____ Trespasser")) 1
  -- CR 123.6b's own example, then CR 123.6c's "fewer words" and a blank.
  Spec.it s "CR 123.6b-c a word goes after k words, before a blank that follows, or at the end" $ do
    let named = CardName.MkCardName . Text.pack
        dark k = CardName.unwrap (NameWords.insertAfter k (Text.pack "Dark") (named "Bear Cub"))
    Spec.assertEqWith s "CR 123.6b Dark Bear Cub, Bear Dark Cub, Bear Cub Dark" (fmap dark [0, 1, 2]) (fmap Text.pack ["Dark Bear Cub", "Bear Dark Cub", "Bear Cub Dark"])
    Spec.assertEqWith s "CR 123.6c after five words of two: at the end" (dark 5) (Text.pack "Bear Cub Dark")
    Spec.assertEqWith s "CR 123.6a after two words, before the blank" (CardName.unwrap (NameWords.insertAfter 2 (Text.pack "Otter") (named "Wolf in _____ Clothing"))) (Text.pack "Wolf in Otter _____ Clothing")
  Spec.it s "CR 123.6d-e letters and unique vowels ignore case, and Y is a vowel" $ do
    Spec.assertEqWith s "CR 123.6e Slimy, Otter, Ringmaster" (fmap (NameWords.uniqueVowels . Text.pack) ["Slimy", "Otter", "Ringmaster"]) [2, 2, 3]
    Spec.assertEqWith s "CR 123.6d the o's in Otter Storm" (NameWords.letterCount (Text.pack "o") (Text.pack "Otter Storm")) 2
  Spec.it s "CR 123.6 a name sticker's words come off its owner's sheet" $ do
    sheets <- committedSheets
    let gs = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        art = StickerRef.MkStickerRef S.alice 2 StickerKind.Art 0
    Spec.assertEqWith s "Night, Slimy, Otter" (fmap (\ref -> Game.stickerWords ref gs) [night, slimy, otter]) (fmap (Just . Text.pack) ["Night", "Slimy", "Otter"])
    Spec.assertEqWith s "an art sticker has none" (Game.stickerWords art gs) Nothing
```

In `ObjectSpec`'s line building `StickerPlacement.MkStickerPlacement (StickerRef.MkStickerRef ...) (Timestamp.MkTimestamp 5)`, append `Nothing`; the JSON stays as it is (the field is defaulted).

- [ ] **Step 2: Run; expected failure** "Could not find module 'Pawl.Engine.NameWords'".

- [ ] **Step 3: Implement.** Create `source/libraries/engine/Pawl/Engine/NameWords.hs`:

```haskell
-- | CR 123.6a: the words of a name as name stickers count them, and CR
-- 123.6d-e's letter and vowel reads. Pure text, no game state.
module Pawl.Engine.NameWords where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.CardName as CardName

-- | CR 123.6a: a blank line, a run of underscores, is not a word.
isBlank :: Text.Text -> Bool
isBlank token = not (Text.null token) && Text.all (== '_') token

-- | CR 123.6a: each maximal run of non-space characters, blanks left out;
-- "_____-o-saurus" is one hyphenated word.
wordsOf :: CardName.CardName -> [Text.Text]
wordsOf = filter (not . isBlank) . Text.words . CardName.unwrap

wordCount :: CardName.CardName -> Natural
wordCount = Natural.length . wordsOf

-- | CR 123.6b-c: `word` after the first `k` words, or at the end of a name
-- with fewer. A blank stays, and the word goes before a blank that follows
-- word k, pawl's reading (#N1).
insertAfter :: Natural -> Text.Text -> CardName.CardName -> CardName.CardName
insertAfter k word name =
  let go :: Natural -> [Text.Text] -> [Text.Text]
      go n tokens = case (n, tokens) of
        (0, _) -> word : tokens
        (_, []) -> [word]
        (_, token : rest) -> token : go (if isBlank token then n else n - 1) rest
   in CardName.MkCardName (Text.unwords (go k (Text.words (CardName.unwrap name))))

-- | CR 123.6d: how many of the text's characters are one of these letters,
-- either case.
letterCount :: Text.Text -> Text.Text -> Natural
letterCount letters text =
  let wanted = Set.fromList (Text.unpack (Text.toLower letters))
   in Natural.length (filter (\c -> Set.member c wanted) (Text.unpack (Text.toLower text)))

-- | CR 123.6e: how many different vowels, A, E, I, O, U and Y, either case.
uniqueVowels :: Text.Text -> Natural
uniqueVowels text = Natural.length (Set.filter (\c -> elem c "aeiouy") (Set.fromList (Text.unpack (Text.toLower text))))
```

Stage `pawl.cabal`, run `cabal-gild pawl.cabal`.

`StickerPlacement.hs`, after `timestamp`:

```haskell
    -- | CR 123.6b-c: a name sticker's position, the number of words before it
    -- as it was placed; Nothing for the other kinds.
    position :: Maybe Natural.Natural
```

with `import qualified Numeric.Natural as Natural`. Its codec gains `position <- Fields.defaulted "position" Nothing (Common.maybe Common.natural) StickerPlacement.position` and the field in the record (import `Pawl.JsonCodec.Common as Common`).

`Sticker.put` takes `Maybe Natural` after the `StickerRef` and writes it as `StickerPlacement.position`; extend its haddock: "CR 123.6b: `position` is a name sticker's".

`Game.hs`, beside `restampStickers` (add `Data.Text`, `StickerKind`, `StickerRef`, `StickerSheet` imports):

```haskell
-- | CR 123.6: the words printed on a name sticker, off its owner's sheet;
-- Nothing for another kind or a reference naming no sticker.
stickerWords :: StickerRef.StickerRef -> GameState -> Maybe Text.Text
stickerWords ref gs = case StickerRef.kind ref of
  StickerKind.Name -> do
    player <- Map.lookup (StickerRef.owner ref) (GameState.players gs)
    slot <- Natural.toInt (StickerRef.sheet ref)
    sheet <- Seq.lookup slot (Player.stickerSheets player)
    i <- Natural.toInt (StickerRef.index ref)
    Seq.lookup i (StickerSheet.names sheet)
  StickerKind.Ability -> Nothing
  StickerKind.PowerToughness -> Nothing
  StickerKind.Art -> Nothing
```

Sites the new field reaches (CLAUDE.md item 4): `grep -rn 'MkStickerPlacement\|StickerPlacement.timestamp = ' . --include='*.hs'`. Expected: the codec and `Sticker.put` (records, compile-forced), `ObjectSpec` (positional, compile-forced), and `Game.restampStickers`' update `placement {StickerPlacement.timestamp = ts}`, which keeps `position`: right, CR 123.6c remembers the position across a public move. `Event.changeZoneWithCause`'s write-back and `Reversal`'s read copy whole placements.

- [ ] **Step 4: Run** `-p Sticker`, `-p Object`, then the full suite. Expected: green.

- [ ] **Step 5: Mutate.** `m1.sed` on `NameWords.hs`: `/^isBlank token =/c\isBlank _ = False`. Expected red: "CR 123.6a Wolf in _____ Clothing has three words". (Pure reads; Tasks 4–7 name the gameplay assertions.)

- [ ] **Step 6: Commit.** "Count a name's words and read a sticker's (related to #872)".

---

## Task 2: name stickers in layer 3

**Files:**
- Create: `source/libraries/types/Pawl/Types/NameInsertion.hs`, `source/libraries/codec/Pawl/Codec/NameInsertion.hs`; Modify: `pawl.cabal`
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`, `source/libraries/codec/Pawl/Codec/Modification.hs`, `source/libraries/codec/Pawl/Codec/ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`, `source/libraries/engine/Pawl/Engine/Projection/Rewrite.hs`, `source/libraries/engine/Pawl/Engine/Interchangeable/Mentions.hs`, `source/libraries/types/Pawl/Types/ProjectedCharacteristics.hs` (haddock)
- Modify: `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `NameInsertion.MkNameInsertion {word :: Text, after :: Natural}`; `Modification.InsertNameWords NameInsertion`; `Projection.stickerGathered :: GameState -> [Gathered]`.
- Consumes: Task 1's `NameWords.insertAfter`, `Game.stickerWords`, `StickerPlacement.position`.

- [ ] **Step 1: Write the failing tests.** In `Pawl.StickerSpec` (imports `Pawl.Engine.Cast as Cast`, `Pawl.Types.Facing as Facing`):

```haskell
  -- CR 123.6c's first example, with a committed word: Otter after Fae of
  -- Wishes' second word. Exile to stack to exile is public to public (CR 123.5).
  Spec.it s "CR 123.6c Fae of Otter Wishes is cast as Granted Otter and exiled as Fae of Otter Wishes again" $ do
    sheets <- committedSheets
    fae <- S.printingOf s registry "Fae of Wishes"
    island <- S.printingOf s registry "Island"
    let base = mainPhaseForAlice (S.landsFor island S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (faeId, exiled) = S.addExiledCard fae S.alice base
        stickered = Sticker.put S.alice faeId otter (Just 2) exiled
        granted = CardName.MkCardName (Text.pack "Granted")
        casting = S.runPure S.identityAnswer stickered (Cast.castSpell S.manaPerformer S.alice faeId granted Facing.FaceUp)
        resolved = S.runPure S.identityAnswer casting Stack.resolveTop
        shown g oids = concatMap (\oid -> nameTexts oid g) oids
    Spec.assertEqWith s "CR 123.6c in exile it is Fae of Otter Wishes" (nameTexts faeId stickered) [Text.pack "Fae of Otter Wishes"]
    Spec.assertEqWith s "CR 123.6c on the stack it is Granted Otter" (shown casting (GameState.stack casting)) [Text.pack "Granted Otter"]
    Spec.assertEqWith s "CR 123.6c/715.3d exiled again it is Fae of Otter Wishes" (shown resolved (Game.zoneMembers Zone.Exile S.alice resolved)) [Text.pack "Fae of Otter Wishes"]
  -- Review Focus 2.
  Spec.it s "CR 123.1/707.2 a Clone of Grizzly Otter Bears is named Grizzly Bears" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    clone <- S.printingOf s registry "Clone"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        stickered = Sticker.put S.alice bearsId otter (Just 1) g1
        (_, staged) = S.spellOnStack clone S.alice stickered
        cloned = S.settleSba (S.runPure (copying bearsId) staged Stack.resolveTop)
        clones = [oid | oid <- Set.toList (GameState.battlefield cloned), Set.notMember oid (GameState.battlefield stickered)]
    Spec.assertEqWith s "CR 707.2 the Clone is named Grizzly Bears" (fmap (\oid -> nameTexts oid cloned) clones) [[Text.pack "Grizzly Bears"]]
    Spec.assertEqWith s "while the original is Grizzly Otter Bears" (nameTexts bearsId cloned) [Text.pack "Grizzly Otter Bears"]
  -- Divergence 5 (#N2): the sticker is later than Spy Kit, so it reaches every
  -- name Spy Kit gave. Goblin Piker is in the game, so its name is in the reference.
  Spec.it s "CR 123.6c/612.7 a sticker placed after Spy Kit goes into every name its host has" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    piker <- S.printingOf s registry "Goblin Piker"
    kit <- S.printingOf s registry "Spy Kit"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, g2) = S.addPermanent piker S.bob g1
        (kitId, g3) = S.addPermanent kit S.alice g2
        stickered = Sticker.put S.alice bearsId otter (Just 1) (S.attach kitId bearsId g3)
        shown = Set.fromList (nameTexts bearsId stickered)
    Spec.assertBool s (Set.member (Text.pack "Goblin Otter Piker") shown) "CR 612.7 the Piker's name, with the word after its first"
    Spec.assertBool s (Set.member (Text.pack "Grizzly Otter Bears") shown) "and its own, Grizzly Otter Bears"
    Spec.assertBool s (Set.notMember (Text.pack "Goblin Piker") shown) "and no name without the word"
```

In `ModificationSpec`, after "HasFullText":

```haskell
  Spec.it s "InsertNameWords carries the word and its position" $
    Common.assertCodec
      s
      codec
      (Modification.InsertNameWords (NameInsertion.MkNameInsertion (Text.pack "Hot Dog") 2))
      " {\"type\":\"InsertNameWords\",\"value\":{\"word\":\"Hot Dog\",\"after\":2}} "
```

- [ ] **Step 2: Run; expected failure** "Not in scope: data constructor 'Modification.InsertNameWords'".

- [ ] **Step 3: Implement.** `Pawl.Types.NameInsertion`:

```haskell
module Pawl.Types.NameInsertion where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural

-- | CR 123.6b-c: a name sticker's effect, its word after this many words.
data NameInsertion = MkNameInsertion
  { word :: Text.Text,
    after :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
```

`Pawl.Codec.NameInsertion` (unit 1's `Pawl.Codec.StickerPlacement` shape):

```haskell
codec :: Codec.Codec NameInsertion.NameInsertion
codec = Fields.object $ do
  word <- Fields.required "word" Common.text NameInsertion.word
  after <- Fields.required "after" Common.natural NameInsertion.after
  pure NameInsertion.MkNameInsertion {NameInsertion.word = word, NameInsertion.after = after}
```

`Modification.hs`, after `HasFullText`:

```haskell
  | -- | layer 3, CR 123.6 / 612.9: a name sticker's word in this object's
    -- names. Engine-minted (Pawl.Engine.Projection.stickerGathered); no card
    -- authors it.
    InsertNameWords NameInsertion.NameInsertion
```

Codec: `Arm.payload "InsertNameWords" NameInsertion.codec Modification.InsertNameWords (\x -> case x of Modification.InsertNameWords y -> Just y; _ -> Nothing)` and `Modification.InsertNameWords {} -> "InsertNameWords"`.

`Projection.hs`, after `counterGathered`:

```haskell
-- CR 123.6 / 612.9 / 613.7k: each name sticker is a layer-3 effect on the
-- object it is on, at the sticker's own timestamp, putting its word after the
-- position recorded as it was placed. Every object: CR 612.9 reaches a card in
-- any zone, and a hidden-zone move has already taken the sticker off (CR
-- 123.5). Game.stickerWords answers Nothing for every other kind.
stickerGathered :: GameState -> [Gathered]
stickerGathered gs =
  [ MkGathered
      { gEffect = Nothing,
        gSource = oid,
        gAffected = Affected.TheseObjects (Set.singleton oid),
        gLayer = Layer.Text,
        gLowest = Layer.Text,
        gTimestamp = StickerPlacement.timestamp placement,
        gModification = Modification.InsertNameWords NameInsertion.MkNameInsertion {NameInsertion.word = ws, NameInsertion.after = k}
      }
  | (oid, obj) <- Map.toList (GameState.objects gs),
    placement <- Foldable.toList (Object.stickers obj),
    Just k <- [StickerPlacement.position placement],
    Just ws <- [Game.stickerWords (StickerPlacement.sticker placement) gs]
  ]
```

In `gatherGiven`'s `let`, add `stickers = stickerGathered gs` beside `counters`, and end the list `... <> castGrants <> encodings <> stickers)`.

`applyModification`, after the `AddNamesMatching` arm:

```haskell
        -- CR 123.6b-c / 612.9: the sticker's word after the first `k` words of
        -- each name the fold has reached (#N2), or the name of an object with
        -- none.
        Modification.InsertNameWords (NameInsertion.MkNameInsertion ws k) ->
          pc {PC.names = if Set.null (PC.names pc) then Set.singleton (CardName.MkCardName ws) else Set.map (NameWords.insertAfter k ws) (PC.names pc)}
```

The other arms, each mirroring `AddNamesMatching`'s (grep `Modification.AddNamesMatching` and read every hit): `layer` → `Layer.Text`; `cardTypesAfter` → `types`; `freezeQuantities` → `Just m`; `quantitiesOf` → `[]`; `referenceQuery` → `Nothing`; `setsLandSubtype`, `removesAbilities`, `grantsKeywordWhere`, `grantsMintingType`, `grantsAbilityWhere` → `False`; `modificationWrites`, `modificationReads` → `Set.empty` (names have no Aspect, the `AddNamesMatching` comment's reason); `Rewrite.rewriteModification` → `acc` (a sticker's word is no subtype word, CR 612.2); `Mentions.modificationNames` → `Modification.InsertNameWords _nameInsertion -> False`; `CardSpec.modificationCounts` and `modificationFilters` → `[]`. The three `_ -> False` fallthroughs (`fullTextOf`, `carriesCondition`, `withStaticGrants`' `readsBack`) are right as they stand: none of them is about names.

`ProjectedCharacteristics.names`' haddock: "Layer 3 adds to it: CR 612.7's Spy Kit ... and CR 123.6's name stickers insert into it (Modification.InsertNameWords)."

Not copiable by construction: `copiableCharacteristics` and `Game.copyStampOf` stop at layer 1, and `stickerGathered` is gathered only by `gatherGiven`. `grep -rn 'stickerGathered' . --include='*.hs'` must show only `gatherGiven`.

- [ ] **Step 4: Run** `-p Sticker`, `-p Modification`, then the full suite. Expected: green. If the Fae case reads `Granted` alone on the stack, the CR 601.2a move did not carry the sticker: read `Event.changeZoneCasting`'s route to `mkObj` before anything else.

- [ ] **Step 5: Mutate.**
  - `m2a.sed` on `Projection.hs`: `s/stickers = stickerGathered gs/stickers = []/` (dropping the list entry would leave the binding unused, which `-Werror` refuses). Expected red: "CR 123.6c in exile it is Fae of Otter Wishes".
  - `m2b.sed` on `NameWords.hs`: `s/(_, \[\]) -> \[word\]/(_, []) -> []/`. Expected red: "CR 123.6c on the stack it is Granted Otter".
  - `m2c.sed` on `Projection.hs`: `s/else Set.map (NameWords.insertAfter k ws) (PC.names pc)/else Set.singleton (NameWords.insertAfter k ws (Set.findMin (PC.names pc)))/`. Expected red: "and its own, Grizzly Otter Bears" (Goblin Piker sorts first).
  - The Clone case is a regression fence: no copy road reads `gatherGiven`, and no one-line mutation routes a layer-3 effect into the copiable values. Say so in the PR.

- [ ] **Step 6: Commit.** "Insert a name sticker's word in layer 3 (related to #872)".

---

## Task 3: CR 612.8's set name, Witness Protection, and a copy beneath the sticker

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`, `source/libraries/codec/Pawl/Codec/Modification.hs`, `source/libraries/codec/Pawl/Codec/ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`, `Projection/Rewrite.hs`, `Interchangeable/Mentions.hs`, `source/libraries/test/Pawl/CardSpec.hs`
- Create: `data/cards/witness-protection.json`, `data/cards/it-that-betrays.json`, `data/cards/seeker-of-the-way.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Modification.SetName CardName`.
- Consumes: Task 2's gather; `Effect.BecomeCopy` through Mirrorweave.

- [ ] **Step 1: Verify Oracle.** Expected:
  - Witness Protection `{U}` Enchantment — Aura: "Enchant creature\nEnchanted creature loses all abilities and is a green and white Citizen creature with base power and toughness 1/1 named Legitimate Businessperson. (It loses all other colors, card types, creature types, and names.)"
  - It That Betrays `{12}` Creature — Eldrazi 11/11: "Annihilator 2 (Whenever this creature attacks, defending player sacrifices two permanents of their choice.)\nWhenever an opponent sacrifices a nontoken permanent, put that card onto the battlefield under your control."
  - Seeker of the Way `{1}{W}` Creature — Human Warrior 2/2: "Prowess (Whenever you cast a noncreature spell, this creature gets +1/+1 until end of turn.)\nWhenever you cast a noncreature spell, this creature gains lifelink until end of turn."

- [ ] **Step 2: Write the failing tests.**

```haskell
  -- Review Focus 1, two boards differing in the order of one Aura and one
  -- sticker: CR 613.7 orders the two layer-3 effects by timestamp.
  Spec.it s "CR 123.6c/613.7 a later Witness Protection hides the word, and a sticker placed after it shows" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    protection <- S.printingOf s registry "Witness Protection"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (laterAura, g2) = S.addPermanent protection S.alice (Sticker.put S.alice bearsId otter (Just 1) g1)
        hidden = S.attach laterAura bearsId g2
        (earlierAura, g3) = S.addPermanent protection S.alice g1
        shown = Sticker.put S.alice bearsId otter (Just 1) (S.attach earlierAura bearsId g3)
    Spec.assertEqWith s "CR 612.8/123.6c Witness Protection, later, leaves only Legitimate Businessperson" (nameTexts bearsId hidden) [Text.pack "Legitimate Businessperson"]
    Spec.assertEqWith s "CR 613.7 a sticker placed after it reads Legitimate Otter Businessperson" (nameTexts bearsId shown) [Text.pack "Legitimate Otter Businessperson"]
  -- Review Focus 1's copy half, CR 123.6c's second example: Mirrorweave makes
  -- every other creature a copy of bob's Seeker.
  Spec.it s "CR 123.6c It That Betrays Otter, as a copy of Seeker of the Way, is Seeker of the Otter Way" $ do
    sheets <- committedSheets
    betrays <- S.printingOf s registry "It That Betrays"
    seeker <- S.printingOf s registry "Seeker of the Way"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    plains <- S.printingOf s registry "Plains"
    let (betraysId, g1) = S.addPermanent betrays S.alice (mainPhaseForAlice (S.landsFor plains S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (seekerId, g2) = S.addPermanent seeker S.bob g1
        stickered = Sticker.put S.alice betraysId otter (Just 3) g2
        (weaveId, g3) = S.addHandCard mirrorweave S.alice stickered
        copied = S.runPure (namingTarget seekerId) g3 (S.cast S.alice weaveId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.6c as a copy of Seeker of the Way it is Seeker of the Otter Way" (nameTexts betraysId copied) [Text.pack "Seeker of the Otter Way"]
    Spec.assertEqWith s "before, It That Betrays Otter" (nameTexts betraysId stickered) [Text.pack "It That Betrays Otter"]
```

`ModificationSpec`:

```haskell
  Spec.it s "SetName carries the name" $
    Common.assertCodec
      s
      codec
      (Modification.SetName (CardName.MkCardName (Text.pack "Legitimate Businessperson")))
      " {\"type\":\"SetName\",\"value\":\"Legitimate Businessperson\"} "
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Modification.SetName'".

- [ ] **Step 4: Implement.** `Modification.hs`, before `InsertNameWords`:

```haskell
  | -- | layer 3, CR 612.8: this object has only this name (Witness Protection).
    SetName CardName.CardName
```

Codec `Arm.payload "SetName" CardName.codec Modification.SetName (\x -> case x of Modification.SetName y -> Just y; _ -> Nothing)` and its tag. `applyModification`:

```haskell
        -- CR 612.8: the object loses its names and has only this one.
        Modification.SetName named -> pc {PC.names = Set.singleton named}
```

Every other arm as `InsertNameWords`' in Task 2, the same list; `Mentions` → `Modification.SetName _cardName -> False`; `Rewrite` → `acc`.

`data/cards/witness-protection.json` (Kenrith's Transformation's shape):

```json
{"faces":[{"enchant":[{"pool":{"type":"Creatures"}}],
 "manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],
 "name":"Witness Protection",
 "oracleText":"Enchant creature\nEnchanted creature loses all abilities and is a green and white Citizen creature with base power and toughness 1/1 named Legitimate Businessperson. (It loses all other colors, card types, creature types, and names.)",
 "staticAbilities":[{"affected":{"type":"Attached"},"modifications":[
   {"type":"LoseAllAbilities"},
   {"type":"SetColor","value":[{"type":"Green"},{"type":"White"}]},
   {"type":"SetCardType","value":{"type":"Creature"}},
   {"type":"SetCreatureSubtype","value":{"type":"Citizen"}},
   {"type":"SetBasePowerToughness","value":{"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":1}}},
   {"type":"SetName","value":"Legitimate Businessperson"}]}],
 "typeLine":{"subtypes":[{"type":"Aura"}],"types":[{"type":"Enchantment"}]}}]}
```

`data/cards/it-that-betrays.json` (Vengeful Tracker's trigger, Prowling Geistcatcher's `became`; `underOwner` defaults to the ability's controller, CR 110.2a):

```json
{"faces":[{"keywords":[{"type":"Annihilator","value":2}],
 "manaCost":[{"type":"Generic","value":12}],
 "name":"It That Betrays",
 "oracleText":"Annihilator 2 (Whenever this creature attacks, defending player sacrifices two permanents of their choice.)\nWhenever an opponent sacrifices a nontoken permanent, put that card onto the battlefield under your control.",
 "power":{"type":"Literal","value":11},"toughness":{"type":"Literal","value":11},
 "triggeredAbilities":[{"condition":{"type":"PermanentSacrificed","value":{"filter":{"type":"Not","value":{"type":"IsToken"}},"player":{"type":"Opponent"}}},
   "modal":{"modes":[{"clauses":[{"effects":[{"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"became"},"zone":{"type":"Battlefield"}}}]}]}]}}],
 "typeLine":{"subtypes":[{"type":"Eldrazi"}],"types":[{"type":"Creature"}]}}]}
```

`data/cards/seeker-of-the-way.json` (Coruscation Mage's trigger, Glory-Bound Initiate's grant):

```json
{"faces":[{"keywords":[{"type":"Prowess"}],
 "manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],
 "name":"Seeker of the Way",
 "oracleText":"Prowess (Whenever you cast a noncreature spell, this creature gets +1/+1 until end of turn.)\nWhenever you cast a noncreature spell, this creature gains lifelink until end of turn.",
 "power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},
 "triggeredAbilities":[{"condition":{"type":"SpellCast","value":{"filter":{"type":"And","value":[{"type":"ControlledBy","value":{"type":"You"}},{"type":"Not","value":{"type":"HasCardType","value":{"type":"Creature"}}}]},"scope":{"type":"EachTurn"}}},
   "modal":{"modes":[{"clauses":[{"effects":[{"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"GainKeyword","value":{"type":"Lifelink"}},"ref":{"type":"InSlot","value":"self"}}}]}]}]}}],
 "typeLine":{"subtypes":[{"type":"Human"},{"type":"Warrior"}],"types":[{"type":"Creature"}]}}]}
```

`script/format-json.sh fix` each.

- [ ] **Step 5: Run** `-p Sticker`, `-p Modification`, `-p Card`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.**
  - `m3a.sed` on `Projection.hs`: `s/Modification.SetName named -> pc {PC.names = Set.singleton named}/Modification.SetName _ -> pc/`. Expected red: "CR 612.8/123.6c Witness Protection, later, leaves only Legitimate Businessperson".
  - `m3b.sed` on `Projection.hs`: `/^stickerGathered ::/,/^$/ s/gTimestamp = StickerPlacement.timestamp placement/gTimestamp = Object.timestamp obj/`. Expected red: "CR 613.7 a sticker placed after it reads Legitimate Otter Businessperson" (the Bears' own timestamp is older than the Aura's).
  - `m3c.sed` on `Projection.hs`: `s/else Set.map (NameWords.insertAfter k ws) (PC.names pc)/else Set.map (NameWords.insertAfter k ws) (Game.namesOf oid gs)/`. Expected red: "CR 123.6c as a copy of Seeker of the Way it is Seeker of the Otter Way" (the printed name, not the copiable one).

- [ ] **Step 7: Commit.** "Set a name in layer 3, and add Witness Protection, It That Betrays and Seeker of the Way (related to #872)".

---

## Task 4: placing a name sticker, Baaallerina and Sword-Swallowing Seraph

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, `source/libraries/types/Pawl/Types/Response.hs`, `source/libraries/engine/Pawl/Engine/Replay.hs`, `source/libraries/test/Pawl/ReplaySpec.hs`
- Modify: `source/libraries/scenario/Pawl/Scenario/Prompt.hs`, `source/libraries/scenario/Pawl/Scenario/Reply.hs`, `source/libraries/scenario/Pawl/Scenario.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`
- Create: `data/cards/baaallerina.json`, `data/cards/sword-swallowing-seraph.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Prompt.ChooseNamePosition :: Decider -> PlayerId -> ObjectId -> NonEmpty Natural -> Prompt Natural`; `Response.ChoseNamePosition Natural`; `Resolve.Effect.namePosition :: ObjectId -> Game Natural`; spec helpers `Asked`, `Naming`, `naming`, `entersNaming`, `placingAt`.
- Consumes: `Projection.namesOf`, `Projection.controllerOf`, `NameWords.wordCount`, `Sticker.put`.

- [ ] **Step 1: Verify Oracle.** Expected:
  - Baaallerina `{3}{U}` Creature — Sheep Performer 3/3: "Flying\nWhen this creature enters, you may put a name sticker on a nonland permanent you own.\n{2}{U}: Another target creature with a name sticker on it gains flying until end of turn."
  - Sword-Swallowing Seraph `{4}{W}{W}` Creature — Angel Performer 4/4: "Flying, vigilance\nWhen this creature enters, you may put a name sticker on a nonland permanent you own.\n{1}{W}, {T}: Put a +1/+1 counter on another target creature with a name sticker on it. Activate only as a sorcery."

- [ ] **Step 2: Write the failing tests.** `Pawl.StickerSpec` helpers after `pyrodancerEnters` (imports `Pawl.Types.FaceDownReason as FaceDownReason`):

```haskell
-- What name placements asked, oldest first: each position prompt's chooser and
-- positions, and each ChooseCardFromAmong's size.
data Asked
  = Positioned PlayerId.PlayerId [Natural]
  | LookedAt Int
  deriving (Eq, Show)

-- How a placement is answered: the permanent, the sticker, the position, and
-- which recipients a target slot takes first.
data Naming = MkNaming
  { namingOnto :: Maybe ObjectId.ObjectId,
    namingSticker :: StickerRef.StickerRef,
    namingPosition :: Natural,
    namingPrefers :: Recipient.Recipient -> Bool
  }

placingAt :: Maybe ObjectId.ObjectId -> StickerRef.StickerRef -> Natural -> Naming
placingAt onto ref k = MkNaming {namingOnto = onto, namingSticker = ref, namingPosition = k, namingPrefers = const False}

naming :: Naming -> Prompt.Prompt r -> State.State [Asked] r
naming how p = case p of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  Prompt.ChoosePermanent _ _ _ offered -> pure (case namingOnto how of
    Just oid | List.elem oid offered -> oid
    _ -> NonEmpty.head offered)
  Prompt.ChooseSticker _ _ _ offered -> pure (if List.elem (namingSticker how) offered then namingSticker how else NonEmpty.head offered)
  Prompt.ChooseNamePosition _ chooser _ offered -> do
    State.modify' (Positioned chooser (NonEmpty.toList offered) :)
    pure (namingPosition how)
  Prompt.ChooseCardFromAmong _ _ _ candidates -> do
    State.modify' (LookedAt (length candidates) :)
    pure (NonEmpty.head candidates)
  Prompt.ChooseTargets _ _ _ offers -> pure (S.preferring (namingPrefers how) offers)
  _ -> pure (S.identityAnswer p)

-- `card` enters under alice with its enters event; all it triggers resolves
-- under `how`.
entersNaming :: Printing.Printing -> Naming -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState, [Asked])
entersNaming card how gs0 =
  let (oid, entered) = S.entersWithTrigger card S.alice gs0
      ((_, after), asked) = State.runState (Engine.runGame (naming how) entered drain) []
   in (oid, after, reverse asked)
```

Cases:

```haskell
  -- Review Focus 3. alice owns the Bears and bob controls them.
  Spec.it s "CR 123.6b the controller, not the placer, chooses where the word goes" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, after, asked) = entersNaming baaallerina (placingAt (Just bearsId) otter 1) (S.giveControl bearsId S.bob g1)
    Spec.assertEqWith s "CR 123.6b bob, who controls the Bears, chooses among three positions" asked [Positioned S.bob [0, 1, 2]]
    Spec.assertEqWith s "and they are Grizzly Otter Bears" (nameTexts bearsId after) [Text.pack "Grizzly Otter Bears"]
  -- Review Focus 3's other half. CR 708.2's face-down state written straight
  -- on, FaceDownSpec's posture; bob turns it up with Break Open.
  Spec.it s "CR 708.2/123.6b a face-down permanent is named Night, and face up Night Ainok Tracker" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    tracker <- S.printingOf s registry "Ainok Tracker"
    breakOpen <- S.printingOf s registry "Break Open"
    mountain <- S.printingOf s registry "Mountain"
    let (trackerId, g1) = S.addPermanent tracker S.alice (S.landsFor mountain S.bob 2 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        hidden = g1 {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.TurnedFaceDown}) trackerId (GameState.objects g1)}
        (_, named, asked) = entersNaming baaallerina (placingAt (Just trackerId) night 0) hidden
        (breakId, g2) = S.addHandCard breakOpen S.bob named
        revealed = S.runPure (namingTarget trackerId) (g2 {GameState.priority = Just S.bob}) (S.cast S.bob breakId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.6b the face-down Tracker is named Night" (nameTexts trackerId named) [Text.pack "Night"]
    Spec.assertEqWith s "CR 123.6c face up it is Night Ainok Tracker" (nameTexts trackerId revealed) [Text.pack "Night Ainok Tracker"]
    Spec.assertEqWith s "CR 123.6b a nameless object has one position, so nobody was asked" asked []
  Spec.it s "CR 123.6 Baaallerina's ability targets only a creature with a name sticker" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    let (marked, g1) = S.addPermanent piker S.alice (S.landsFor island S.alice 3 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (plainPiker, g2) = S.addPermanent piker S.alice g1
        (baaId, g3, _) = entersNaming baaallerina (placingAt (Just marked) night 0) g2
        board = mainPhaseForAlice g3
        recording :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems sets))
            pure (namingTarget marked p)
          _ -> pure (S.identityAnswer p)
        ((_, after), offered) = case Activatable.abilitiesFor baaId board of
          [ability] -> State.runState (Engine.runGame recording board (Activate.activateAbility S.alice baaId ability >> Stack.resolveTop)) []
          _ -> (((), board), [])
    Spec.assertEqWith s "CR 115.1 only the Piker with a name sticker is offered" offered [marked]
    Spec.assertEqWith s "it gains flying, the other Piker does not" (Projection.hasKeyword Keyword.Flying marked after, Projection.hasKeyword Keyword.Flying plainPiker after) (True, False)
  Spec.it s "CR 123.6 whole card: Sword-Swallowing Seraph names a Piker and puts a +1/+1 counter on it" $ do
    sheets <- committedSheets
    seraph <- S.printingOf s registry "Sword-Swallowing Seraph"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    let (pikerId, g1) = S.addPermanent piker S.alice (S.landsFor plains S.alice 2 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (seraphId, g2, _) = entersNaming seraph (placingAt (Just pikerId) night 0) g1
        board = mainPhaseForAlice g2
        after = case Activatable.abilitiesFor seraphId board of
          [ability] -> S.runPure (namingTarget pikerId) board (Activate.activateAbility S.alice seraphId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 123.6 the Piker with a name sticker has a +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
    Spec.assertEqWith s "it is Night Goblin Piker" (nameTexts pikerId g2) [Text.pack "Night Goblin Piker"]
```

`ReplaySpec`, after the ChooseSticker case:

```haskell
        -- CR 123.6b: where the controller put the word is a decision.
        Spec.it s "ChooseNamePosition round-trips through the transcript" $ do
          let p = Prompt.ChooseNamePosition decider S.bob (ObjectId.MkObjectId 7) (0 NonEmpty.:| [1, 2])
          Spec.assertEqWith s "choosing the last round trips" (Replay.decode p (Replay.encode p 2)) (Just 2)
          Spec.assertEqWith s "a short transcript puts it first" (Replay.defaultAnswer p) 0
```

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Prompt.ChooseNamePosition'".

- [ ] **Step 4: Implement.** `Prompt.hs`, after `ChooseSticker`:

```haskell
  -- | CR 123.6b: after how many words of this object's name the controller
  -- puts a name sticker's word.
  ChooseNamePosition :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty Natural.Natural -> Prompt Natural.Natural
```

`Response.hs`, after `ChoseSticker`: `| -- | CR 123.6b: the position a controller chose.` / `ChoseNamePosition Natural.Natural`. `Replay`: `encode` → `Prompt.ChooseNamePosition {} -> Response.ChoseNamePosition answer`; `decode` → `Prompt.ChooseNamePosition {} -> case response of Response.ChoseNamePosition n -> Just n; _ -> Nothing` (laid out as its neighbours); `defaultAnswer` → `-- CR 123.6b: the start of the name is always a position.` / `Prompt.ChooseNamePosition _ _ _ positions -> NonEmpty.head positions`. Scenario: `Pawl.Scenario.Prompt` decider arm `Prompt.ChooseNamePosition decider _ _ _ -> Just (Decider.unwrap decider)` and name arm `"ChooseNamePosition"`; `Pawl.Scenario.Reply` → `natural`; `Pawl.Scenario`'s legality → `Prompt.Type.ChooseNamePosition _ _ _ positions -> elem chosen positions`.

`Resolve/Effect.hs`, the `PutSticker` arm's placement (imports `NameWords`, `StickerKind`, `StickerRef`):

```haskell
      let place chosen = do
            -- CR 123.6b: a name sticker's position is chosen as it is placed.
            position <- case StickerRef.kind chosen of
              StickerKind.Name -> fmap Just (namePosition oid)
              StickerKind.Ability -> pure Nothing
              StickerKind.PowerToughness -> pure Nothing
              StickerKind.Art -> pure Nothing
            State.modify' (Sticker.put placer oid chosen position)
      Monad.when owned $ case Sticker.available placer kinds gs of
        [] -> pure ()
        [only] -> place only
        first : rest -> do
          let offered = first NonEmpty.:| rest
          answer <- Game.choose (Prompt.ChooseSticker (Decide.deciderFor placer gs) placer oid offered)
          place (if List.elem answer offered then answer else first)
```

and, as a top-level function beside `bindSlot`:

```haskell
-- CR 123.6b: the object's CONTROLLER, or its owner for a card with none (CR
-- 108.4a), chooses where the word goes: the start, or after any number of the
-- words now in its name, the longest name's where it has several (#N2). An
-- object whose names hold no word has one position and is not asked.
namePosition :: ObjectId -> Game Natural
namePosition oid = do
  gs <- State.get
  let most = List.foldl' max 0 (fmap NameWords.wordCount (Set.toList (Projection.namesOf oid gs)))
      chooser = case Projection.controllerOf oid gs of
        Just pid -> Just pid
        Nothing -> fmap Object.owner (Game.lookupObject oid gs)
  case chooser of
    Just pid | most > 0 -> do
      answer <- Game.choose (Prompt.ChooseNamePosition (Decide.deciderFor pid gs) pid oid (0 NonEmpty.:| [1 .. most]))
      pure (if answer <= most then answer else 0)
    _ -> pure 0
```

The arm's comment gains: "CR 123.6b: the object's controller places a name sticker's word (namePosition)."

`data/cards/baaallerina.json` (Proficient Pyrodancer's shape, kind Name):

```json
{"faces":[{
 "activatedAbilities":[{"cost":{"mana":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}]},
   "modal":{"modes":[{"clauses":[{"effects":[{"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"GainKeyword","value":{"type":"Flying"}},"ref":{"type":"InSlot","value":"target"}}}]}],
     "targetSlots":{"target":{"filter":{"type":"And","value":[{"type":"Not","value":{"type":"IsSource"}},{"type":"HasSticker","value":{"type":"Name"}}]},"pool":{"type":"Creatures"}}}}]}}],
 "keywords":[{"type":"Flying"}],
 "manaCost":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],
 "name":"Baaallerina",
 "oracleText":"Flying\nWhen this creature enters, you may put a name sticker on a nonland permanent you own.\n{2}{U}: Another target creature with a name sticker on it gains flying until end of turn.",
 "power":{"type":"Literal","value":3},"toughness":{"type":"Literal","value":3},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"PutSticker","value":{"kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},
   "ref":{"type":"ChosenPermanent","value":{"filter":{"type":"And","value":[{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}},{"type":"OwnedBy","value":{"type":"You"}}]}}}}}],"optionality":{"type":"Optional"}}]}]}}],
 "typeLine":{"subtypes":[{"type":"Sheep"},{"type":"Performer"}],"types":[{"type":"Creature"}]}}]}
```

`data/cards/sword-swallowing-seraph.json` (the same trigger; Adaptive Gemguard's `SorcerySpeed`):

```json
{"faces":[{
 "activatedAbilities":[{"cost":{"components":[{"type":"TapThis"}],"mana":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}]},
   "modal":{"modes":[{"clauses":[{"effects":[{"type":"PutCounters","value":{"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"Literal","value":1},"ref":{"type":"InSlot","value":"target"}}}]}],
     "targetSlots":{"target":{"filter":{"type":"And","value":[{"type":"Not","value":{"type":"IsSource"}},{"type":"HasSticker","value":{"type":"Name"}}]},"pool":{"type":"Creatures"}}}}]},
   "restrictions":[{"type":"SorcerySpeed"}]}],
 "keywords":[{"type":"Flying"},{"type":"Vigilance"}],
 "manaCost":[{"type":"Generic","value":4},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],
 "name":"Sword-Swallowing Seraph",
 "oracleText":"Flying, vigilance\nWhen this creature enters, you may put a name sticker on a nonland permanent you own.\n{1}{W}, {T}: Put a +1/+1 counter on another target creature with a name sticker on it. Activate only as a sorcery.",
 "power":{"type":"Literal","value":4},"toughness":{"type":"Literal","value":4},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"PutSticker","value":{"kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},
   "ref":{"type":"ChosenPermanent","value":{"filter":{"type":"And","value":[{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}},{"type":"OwnedBy","value":{"type":"You"}}]}}}}}],"optionality":{"type":"Optional"}}]}]}}],
 "typeLine":{"subtypes":[{"type":"Angel"},{"type":"Performer"}],"types":[{"type":"Creature"}]}}]}
```

Prompt sites (CLAUDE.md item 4): `grep -rn 'ChooseSticker\|ChoseSticker' . --include='*.hs'` and give each hit its sibling; Replay's three functions are exhaustive, the scenario three are too.

- [ ] **Step 5: Run** `-p Sticker`, `-p Replay`, `-p Scenario`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.**
  - `m4a.sed` on `Resolve/Effect.hs`: `/^namePosition ::/,/^$/ s/Just pid -> Just pid/Just _ -> fmap Object.owner (Game.lookupObject oid gs)/`. Expected red: "CR 123.6b bob, who controls the Bears, chooses among three positions".
  - `m4b.sed` on `Projection.hs`: `s/if Set.null (PC.names pc) then Set.singleton (CardName.MkCardName ws)/if False then Set.singleton (CardName.MkCardName ws)/`. Expected red: "CR 123.6b the face-down Tracker is named Night".
  - `m4c.sed` on `data/cards/baaallerina.json`: `/"HasSticker"/,/"Name"/ s/"Name"/"Art"/`. Expected red: "CR 115.1 only the Piker with a name sticker is offered".
  - `m4d.sed` on `data/cards/sword-swallowing-seraph.json`: the same script. Expected red: "CR 123.6 the Piker with a name sticker has a +1/+1 counter".

- [ ] **Step 7: Commit.** "Ask the controller where a name sticker goes, and add Baaallerina and Sword-Swallowing Seraph (related to #872)".

---

## Task 5: "that sticker", Wizards of the _____, Wolf in _____ Clothing and _____-o-saurus

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Binding.hs`, `source/libraries/codec/Pawl/Codec/Binding.hs`, `source/libraries/codec/Pawl/Codec/BindingSpec.hs`, `source/libraries/codec/Pawl/Codec/ObjectSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Binding.hs`, `source/libraries/engine/Pawl/Engine/Filter.hs`, `source/libraries/engine/Pawl/Engine/Target.hs`, `source/libraries/engine/Pawl/Engine/Resolve/Slots.hs`, `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`
- Modify: `source/libraries/types/Pawl/Types/PutSticker.hs`, `source/libraries/codec/Pawl/Codec/PutSticker.hs`, `source/libraries/codec/Pawl/Codec/EffectSpec.hs`, `Projection/Rewrite.hs`, `Interchangeable/Mentions.hs`
- Modify: `source/libraries/types/Pawl/Types/Quantity.hs`, `source/libraries/codec/Pawl/Codec/Quantity.hs`, `source/libraries/codec/Pawl/Codec/QuantitySpec.hs`, `source/libraries/engine/Pawl/Engine/Quantity.hs`, `QuantitySlot.hs`, `Projection.hs`, `Star.hs`, `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/EffectLintSpec.hs`
- Create: `data/cards/wizards-of-the-_.json`, `data/cards/wolf-in-_-clothing.json`, `data/cards/_-o-saurus.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Binding.sticker :: Maybe StickerRef`; `Engine.Binding.toSticker :: StickerRef -> Binding`; `Filter.Context.slotStickers :: Map SlotName StickerRef`; `PutSticker.bound :: Maybe SlotName`; `Quantity.UniqueVowelsOnSticker SlotName`; `Resolve.Effect.bindStickerSlot :: ObjectId -> SlotName -> StickerRef -> GameState -> GameState`.
- Consumes: Task 4's `place`, `NameWords.uniqueVowels`, `Game.stickerWords`.

- [ ] **Step 1: Verify Oracle.** Expected:
  - Wizards of the _____ `{2}{U}{U}` Creature — Human Wizard Performer 3/1: "When this creature enters, you may put a name sticker on it, then look at the top X cards of your library, where X is the number of unique vowels on that sticker. Put one of those cards into your hand and the rest on the bottom of your library in any order. (The vowels are A, E, I, O, U, and Y.)"
  - Wolf in _____ Clothing `{3}{B}` Creature — Wolf Guest 2/3: "When this creature enters, you may put a name sticker on it. When you do, up to X target creatures each get -1/-1 until end of turn, where X is the number of unique vowels on that sticker. (The vowels are A, E, I, O, U, and Y.)"
  - _____-o-saurus `{4}{G}{G}` Creature — Alien Dinosaur 3/3: "Trample\nWhen this creature enters, you may put a name sticker on it. Put a +1/+1 counter on it for each unique vowel on that sticker. (The vowels are A, E, I, O, U, and Y.)"

- [ ] **Step 2: Write the failing tests.**

```haskell
  -- Review Focus 5: case. Three Islands to look at.
  Spec.it s "CR 123.6e Otter has two unique vowels, its O capital: Wizards of the _____ looks at two cards" $ do
    sheets <- committedSheets
    wizards <- S.printingOf s registry "Wizards of the _____"
    island <- S.printingOf s registry "Island"
    let (_, l1) = S.addLibraryCard island S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, l2) = S.addLibraryCard island S.alice l1
        (_, l3) = S.addLibraryCard island S.alice l2
        (wizardsId, after, asked) = entersNaming wizards (placingAt Nothing otter 3) l3
    Spec.assertEqWith s "CR 123.6e it looked at two cards, after three positions past three words" asked [Positioned S.alice [0, 1, 2, 3], LookedAt 2]
    Spec.assertEqWith s "CR 123.6a it is Wizards of the Otter _____" (nameTexts wizardsId after) [Text.pack "Wizards of the Otter _____"]
  -- Review Focus 5: Y, and the binding read by a "when you do" ability's
  -- target count (CR 603.12). Pikers are 2/1, so -1/-1 kills.
  Spec.it s "CR 123.6e Slimy has two unique vowels, Y one of them: Wolf in _____ Clothing kills two of bob's Pikers" $ do
    sheets <- committedSheets
    wolf <- S.printingOf s registry "Wolf in _____ Clothing"
    piker <- S.printingOf s registry "Goblin Piker"
    let (p1, g1) = S.addPermanent piker S.bob (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (p2, g2) = S.addPermanent piker S.bob g1
        (p3, g3) = S.addPermanent piker S.bob g2
        bobs r = case Recipient.objectOf r of
          Just oid -> List.elem oid [p1, p2, p3]
          Nothing -> False
        (_, after, _) = entersNaming wolf ((placingAt Nothing slimy 0) {namingPrefers = bobs}) g3
    Spec.assertEqWith s "CR 123.6e two of bob's Pikers died" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  -- Review Focus 4. Two boards differing in the position alone.
  Spec.it s "CR 123.6a Wolf in _____ Clothing offers four positions, and the word goes before a following blank" $ do
    sheets <- committedSheets
    wolf <- S.printingOf s registry "Wolf in _____ Clothing"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        (twoId, afterTwo, asked) = entersNaming wolf (placingAt Nothing otter 2) base
        (threeId, afterThree, _) = entersNaming wolf (placingAt Nothing otter 3) base
    Spec.assertEqWith s "CR 123.6a after two words it is Wolf in Otter _____ Clothing; after three, Wolf in _____ Clothing Otter" (nameTexts twoId afterTwo, nameTexts threeId afterThree) ([Text.pack "Wolf in Otter _____ Clothing"], [Text.pack "Wolf in _____ Clothing Otter"])
    Spec.assertEqWith s "CR 123.6a the blank is not a word: four positions" asked [Positioned S.alice [0, 1, 2, 3]]
  Spec.it s "CR 123.6a _____-o-saurus is one word: two positions, and Otter puts two counters on it" $ do
    sheets <- committedSheets
    saurus <- S.printingOf s registry "_____-o-saurus"
    let (saurusId, after, asked) = entersNaming saurus (placingAt Nothing otter 1) (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
    Spec.assertEqWith s "CR 123.6e two +1/+1 counters for Otter" (S.counterOf CounterKind.PlusOnePlusOne saurusId after) 2
    Spec.assertEqWith s "CR 123.6a it is _____-o-saurus Otter" (nameTexts saurusId after) [Text.pack "_____-o-saurus Otter"]
    Spec.assertEqWith s "CR 123.6a one word, two positions" asked [Positioned S.alice [0, 1]]
```

`QuantitySpec`: `Spec.it s "UniqueVowelsOnSticker" $ Common.assertCodec s Quantity.codec (Quantity.UniqueVowelsOnSticker (SlotName.MkSlotName (Text.pack "placed"))) " {\"type\":\"UniqueVowelsOnSticker\",\"value\":\"placed\"} "`. `EffectSpec`'s PutSticker case: add `(Just (SlotName.MkSlotName (Text.pack "placed")))` and `,\"bound\":\"placed\"` after `kinds`. `BindingSpec`'s every-field case: `Binding.sticker = Just (StickerRef.MkStickerRef (PlayerId.MkPlayerId 0) 2 StickerKind.Name 1)` and `,\"sticker\":{\"owner\":0,\"sheet\":2,\"kind\":{\"type\":\"Name\"},\"index\":1}` after `objects`, and its comment's "five" becomes "six".

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Quantity.UniqueVowelsOnSticker'".

- [ ] **Step 4: Implement.**

`Binding.hs`, after `objects`:

```haskell
    -- | CR 123.6e's "that sticker": the sticker an Effect.PutSticker placed,
    -- under its `bound` slot. Nothing for every other slot.
    sticker :: Maybe StickerRef.StickerRef
```

`Binding.empty` gains `sticker = Nothing`; the codec `sticker <- Fields.defaulted "sticker" Nothing (Common.maybe StickerRef.codec) Binding.sticker`; `Engine.Binding.mergeBinding` `Binding.sticker = Binding.sticker a <|> Binding.sticker b`; `renameObject` `Binding.sticker = Binding.sticker binding`; `ObjectSpec`'s `Binding.MkBinding` record `Binding.sticker = Nothing`; `Mentions`' positional `Binding.MkBinding targets _amount _modes copy objects` gains `_sticker` (a sticker names no object, slot or filter). In `Engine.Binding`, beside `toAmount`:

```haskell
-- | A binding that holds one placed sticker and nothing else (CR 123.6e).
toSticker :: StickerRef.StickerRef -> Binding
toSticker ref = Binding.empty {Binding.sticker = Just ref}
```

`Filter.Context`, after `boundAmounts`:

```haskell
    -- | CR 123.6e's "that sticker": the sticker an earlier instruction bound at
    -- each slot. Empty in contextFor; filled by Resolve.Slots.effectContext and
    -- Target.slotContext.
    slotStickers :: Map.Map SlotName.SlotName StickerRef.StickerRef,
```

`contextFor`: `slotStickers = Map.empty`. `Resolve.Slots.effectContext` and `Target.slotContext`, beside `Filter.boundAmounts = ...`: `Filter.slotStickers = Map.mapMaybe Binding.Type.sticker bindings,`.

`PutSticker.hs`, after `kinds`:

```haskell
    -- | CR 123.6e: the slot the placed sticker is bound under, for "that
    -- sticker".
    bound :: Maybe SlotName.SlotName
```

Codec `bound <- Fields.defaulted "bound" Nothing (Common.maybe SlotName.codec) PutSticker.bound`. Positional sites (compile-forced): `Resolve.Slots.effectObjectRefs`, `effectPlayerRefs`, the impossibility gate and the resolution arm in `Resolve/Effect.hs`, `Rewrite.rewriteEffect` (carry `bound` through), `Mentions.putStickerNames` (`|| any (slotNames asking) bound`: a slot name is read, CLAUDE.md item 4). `Resolve.Slots.boundSlots` (a `{}` pattern, not forced): `Effect.PutSticker putSticker -> foldMap Set.singleton (PutSticker.bound putSticker)` with the comment "CR 123.6e: the placed sticker". `slotsOf` stays `Map.empty` (the slot is a definition); `ownSlotsAreExhaustive`, `readsX`, `ManaAbility`'s three and `EffectZone`'s arm are right as they stand.

The resolution arm's `place`, after `State.modify' (Sticker.put ...)`:

```haskell
            Monad.forM_ bound (\slot -> State.modify' (bindStickerSlot resolving slot chosen))
```

and beside `bindSlot`:

```haskell
-- CR 123.6e's "that sticker": bind the placed sticker under `slot`, on
-- bindSlot's holder. Written only when a sticker was placed, so
-- happenedBetween needs no copy-across.
bindStickerSlot :: ObjectId -> SlotName -> StickerRef.StickerRef -> GameState -> GameState
bindStickerSlot holder slot ref = overHolderBindings holder (Map.insert slot (Binding.toSticker ref))
```

`Quantity.hs`, after `BoundCount`:

```haskell
  | -- | CR 123.6e: the unique vowels on the sticker that slot holds; 0 when it
    -- holds none.
    UniqueVowelsOnSticker SlotName.SlotName
```

`Engine.Quantity.evaluateAgainst`, after `BoundCount`:

```haskell
        -- CR 123.6e's "that sticker", off the binding, WasBound's posture: an
        -- unplaced sticker is an honest 0.
        Quantity.UniqueVowelsOnSticker slot -> case Map.lookup slot (Filter.slotStickers context) of
          Nothing -> Just 0
          Just ref -> case Game.stickerWords ref gs of
            Nothing -> Just 0
            Just ws -> Just (toInteger (NameWords.uniqueVowels ws))
```

Other arms, mirroring `WasBound` (grep `Quantity.WasBound` and `Quantity.Type.WasBound`, read every hit): `QuantitySlot.overSlots` → `fmap Quantity.UniqueVowelsOnSticker (f slot)` (the D4 dataflow lint's read side); `nestedRefs` → `Set.empty`; `nestedCounts` → `[]`; `mapPlayerRefs` → `quantity`; `Quantity.objectSlots` → `Set.empty`; `Quantity.readsX` → `False`; `Projection.quantityReads` → `Set.empty`; `Star.substituteStar` → `quantity`; `Rewrite.rewriteQuantity` → `quantity`; `Mentions.quantityNames` → `slotNames asking slotName`; `EffectLintSpec.printedBoxQuantity` → `False`; `CardSpec.quantityKindFilters` → `[]`; codec arm and tag.

`data/cards/wizards-of-the-_.json` (Necrosynthesis' look; the look is unconditional, and with no sticker X is 0, so either reading of the "then" is observably the same):

```json
{"faces":[{"manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],
 "name":"Wizards of the _____",
 "oracleText":"When this creature enters, you may put a name sticker on it, then look at the top X cards of your library, where X is the number of unique vowels on that sticker. Put one of those cards into your hand and the rest on the bottom of your library in any order. (The vowels are A, E, I, O, U, and Y.)",
 "power":{"type":"Literal","value":3},"toughness":{"type":"Literal","value":1},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[
   {"effects":[{"type":"PutSticker","value":{"bound":"placed","kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},"ref":{"type":"InSlot","value":"self"}}}],"optionality":{"type":"Optional"}},
   {"effects":[{"type":"LookAt","value":{"ref":{"type":"TopOfLibrary","value":{"count":{"type":"UniqueVowelsOnSticker","value":"placed"},"player":{"type":"Relative","value":{"type":"You"}}}},"slot":"looked"}}]},
   {"effects":[{"type":"MoveToZone","value":{"ref":{"type":"ChosenCardFromAmong","value":{"filter":{"type":"And","value":[]},"slot":"looked"}},"zone":{"type":"Hand"}}}]},
   {"effects":[{"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"looked"},"zone":{"type":"Library"}}}]}]}]}}],
 "typeLine":{"subtypes":[{"type":"Human"},{"type":"Wizard"},{"type":"Performer"}],"types":[{"type":"Creature"}]}}]}
```

`data/cards/wolf-in-_-clothing.json` (Miasma Demon's reflexive, armed in the placement's own clause so CR 603.12 reads the placement):

```json
{"faces":[{"delayedAbilities":{"shrink":{"condition":{"type":"Reflexive"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"ModifyPowerToughness","value":{"power":{"type":"Literal","value":-1},"toughness":{"type":"Literal","value":-1}}},"ref":{"type":"InSlot","value":"targets"}}}]}],
   "targetSlots":{"targets":{"count":{"type":"UpToComputed","value":{"type":"UniqueVowelsOnSticker","value":"placed"}},"pool":{"type":"Creatures"}}}}]}}},
 "manaCost":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],
 "name":"Wolf in _____ Clothing",
 "oracleText":"When this creature enters, you may put a name sticker on it. When you do, up to X target creatures each get -1/-1 until end of turn, where X is the number of unique vowels on that sticker. (The vowels are A, E, I, O, U, and Y.)",
 "power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":3},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[
   {"type":"PutSticker","value":{"bound":"placed","kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},"ref":{"type":"InSlot","value":"self"}}},
   {"type":"ArmDelayedTrigger","value":{"name":"shrink"}}],"optionality":{"type":"Optional"}}]}]}}],
 "typeLine":{"subtypes":[{"type":"Wolf"},{"type":"Guest"}],"types":[{"type":"Creature"}]}}]}
```

`data/cards/_-o-saurus.json`:

```json
{"faces":[{"keywords":[{"type":"Trample"}],
 "manaCost":[{"type":"Generic","value":4},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],
 "name":"_____-o-saurus",
 "oracleText":"Trample\nWhen this creature enters, you may put a name sticker on it. Put a +1/+1 counter on it for each unique vowel on that sticker. (The vowels are A, E, I, O, U, and Y.)",
 "power":{"type":"Literal","value":3},"toughness":{"type":"Literal","value":3},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[
   {"effects":[{"type":"PutSticker","value":{"bound":"placed","kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},"ref":{"type":"InSlot","value":"self"}}}],"optionality":{"type":"Optional"}},
   {"effects":[{"type":"PutCounters","value":{"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"UniqueVowelsOnSticker","value":"placed"},"ref":{"type":"InSlot","value":"self"}}}]}]}]}}],
 "typeLine":{"subtypes":[{"type":"Alien"},{"type":"Dinosaur"}],"types":[{"type":"Creature"}]}}]}
```

Binding sites (CLAUDE.md item 4): `grep -rn 'MkBinding\|{ Binding.amount = \|Binding.Type.amount = ' . --include='*.hs'`. Expected and right: the record constructions above (forced), `happenedBetween`'s `binding {Binding.Type.amount = Nothing}` (keeps `sticker`: a placement that happened stays a difference, which is right), and `Engine.placeOne`'s per-field join through `mergeBinding`.

- [ ] **Step 5: Run** `-p Sticker`, `-p Quantity`, `-p Binding`, `-p Effect`, `-p Card`, then the full suite. Expected: green. A red "looked at two cards" with `asked` holding no `LookedAt` means the LookAt count was evaluated in a context `effectContext` did not build: find it before changing anything else.

- [ ] **Step 6: Mutate.**
  - `m5a.sed` on `NameWords.hs`: `s/"aeiouy"/"aeiou"/`. Expected red: "CR 123.6e two of bob's Pikers died".
  - `m5b.sed` on `NameWords.hs`: `/^uniqueVowels/ s/(Text.toLower text)/text/`. Expected red: "CR 123.6e it looked at two cards, after three positions past three words".
  - `m5c.sed` on `Resolve/Effect.hs`: `/^bindStickerSlot holder slot ref =/c\bindStickerSlot _ _ _ = id`. Expected red: "CR 123.6e it looked at two cards, after three positions past three words".
  - `m5d.sed` on `Target.hs`: `/Filter.slotStickers = Map.mapMaybe Binding.Type.sticker bindings,/d`. Expected red: "CR 123.6e two of bob's Pikers died" (the reflexive's count is announced at CR 603.3d, off this context).
  - `m5e.sed` on `Resolve/Slots.hs`: the same `d` script. Expected red: "CR 123.6e it looked at two cards, after three positions past three words".
  - `m5f.sed` on `NameWords.hs`: `s/(_, token : rest) -> token : go (if isBlank token then n else n - 1) rest/(_, token : rest) -> token : go (n - 1) rest/`. Expected red: "CR 123.6a after two words it is Wolf in Otter _____ Clothing; after three, Wolf in _____ Clothing Otter".
  - `m5g.sed` on `NameWords.hs`: `s/Text.all (== '_') token/Text.any (== '_') token/`. Expected red: "CR 123.6a it is _____-o-saurus Otter".

- [ ] **Step 7: Commit.** "Bind the placed sticker and count its vowels, and add Wizards of the _____, Wolf in _____ Clothing and _____-o-saurus (related to #872)".

---

## Task 6: the stickers' letters and count, Trespasser and _____ Balls of Fire

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Filter.hs` (View), `source/libraries/engine/Pawl/Engine/Projection/View.hs`, `source/libraries/engine/Pawl/Engine/Count.hs`, `source/libraries/test/Pawl/FilterSpec.hs`, `source/libraries/test/Pawl/Support.hs`
- Modify: `source/libraries/types/Pawl/Types/Quantity.hs`, `source/libraries/codec/Pawl/Codec/Quantity.hs`, `source/libraries/codec/Pawl/Codec/QuantitySpec.hs`, and Task 5's Quantity sites
- Modify: `source/libraries/types/Pawl/Types/PlacesSticker.hs`, `source/libraries/codec/Pawl/Codec/PlacesSticker.hs`, `source/libraries/codec/Pawl/Codec/TriggerConditionSpec.hs`, `source/libraries/engine/Pawl/Engine/Event/Match.hs`, `Projection/Rewrite.hs`, `Interchangeable/Mentions.hs`, `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/ZoneTriggerSpec.hs`
- Create: `data/cards/_-_-_-trespasser.json`, `data/cards/_-balls-of-fire.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Filter.View.nameStickers :: Seq Text`; `Quantity.LettersOnNameStickers Text`; `Quantity.NameStickers`; `PlacesSticker.object :: Filter Keyword`.
- Consumes: `NameWords.letterCount`, `Game.stickerWords`.

- [ ] **Step 1: Verify Oracle.** Expected:
  - _____ _____ _____ Trespasser `{1}{U}` Creature — Human Rogue Guest 2/1: "When this creature enters, you may put a name sticker on it.\n{3}{U}: This creature gets +1/+0 until end of turn for each name sticker on it. It can't be blocked this turn."
  - _____ Balls of Fire `{3}{R}` Enchantment: "When this enchantment enters, you may put a name sticker on it.\nWhenever you put a sticker on this enchantment, it deals damage equal to the number of o's in name stickers on this enchantment to any target."

- [ ] **Step 2: Write the failing tests.**

```haskell
  Spec.it s "CR 123.6 Trespasser gets +1/+0 for its one name sticker" $ do
    sheets <- committedSheets
    trespasser <- S.printingOf s registry "_____ _____ _____ Trespasser"
    island <- S.printingOf s registry "Island"
    let (tId, g1, asked) = entersNaming trespasser (placingAt Nothing otter 1) (S.landsFor island S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        board = mainPhaseForAlice g1
        after = case Activatable.abilitiesFor tId board of
          [ability] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice tId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 123.6 one name sticker: power 3" (Projection.powerOf tId after) (Just 3)
    Spec.assertEqWith s "CR 123.6a three blanks and one word: two positions" asked [Positioned S.alice [0, 1]]
  -- Two boards: Balls of Fire's own sticker triggers it; a sticker on the
  -- Bears does not, though Balls of Fire carries an Otter of its own.
  Spec.it s "CR 123.6d Otter's capital O counts: _____ Balls of Fire deals bob 1, and only for its own sticker" $ do
    sheets <- committedSheets
    balls <- S.printingOf s registry "_____ Balls of Fire"
    baaallerina <- S.printingOf s registry "Baaallerina"
    bears <- S.printingOf s registry "Grizzly Bears"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        atBob how = how {namingPrefers = (== Recipient.ToPlayer S.bob)}
        (_, own, _) = entersNaming balls (atBob (placingAt Nothing otter 0)) base
        (ballsId, g1) = S.addPermanent balls S.alice base
        (bearsId, g2) = S.addPermanent bears S.alice (Sticker.put S.alice ballsId otter (Just 0) g1)
        (_, other, _) = entersNaming baaallerina (atBob (placingAt (Just bearsId) night 0)) g2
    Spec.assertEqWith s "CR 123.6d bob takes 1 for Otter's O" (S.lifeOf S.bob own) (Just 19)
    Spec.assertEqWith s "CR 123.3 a sticker on another permanent does not trigger it" (S.lifeOf S.bob other) (Just 20)
```

`QuantitySpec`: `LettersOnNameStickers` (`" {\"type\":\"LettersOnNameStickers\",\"value\":\"o\"} "`) and `NameStickers` (`" {\"type\":\"NameStickers\"} "`). `TriggerConditionSpec`: a second PlacesSticker case with `Filter.IsSource`, JSON `...\"kinds\":[{\"type\":\"Art\"}],\"object\":{\"type\":\"IsSource\"}}} `; the existing case gains `(Filter.And [])` and keeps its JSON.

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Quantity.NameStickers'".

- [ ] **Step 4: Implement.**

`Filter.View`, after `stickerKinds`:

```haskell
    -- | CR 123.6: the word on each name sticker on the candidate, in placement
    -- order. Off the object, stickerKinds' posture.
    nameStickers :: Seq.Seq Text.Text,
```

Every `MkView` builder (unit 1's Divergence 5): `Filter`'s player view `nameStickers = Seq.empty` (a player has no stickers); `Projection.View.viewOfCard` `Filter.nameStickers = Seq.empty` (a printed face); `viewOfCharacteristics`:

```haskell
      Filter.nameStickers = foldMap (\obj -> Seq.fromList (Maybe.mapMaybe (\p -> Game.stickerWords (StickerPlacement.sticker p) gs) (Foldable.toList (Object.stickers obj)))) (Game.lookupObject oid gs),
```

`Count.viewOfSnapshot` `Filter.nameStickers = Seq.empty` under the existing #4890 comment, reworded to "Not implemented: CR 608.2h's record of a departed object's stickers, kinds and words (#4890)." Test constructions (forced by `-Wmissing-fields`): `FilterSpec`'s two views and `Support`'s, `Seq.empty`.

`Quantity.hs`, after `UniqueVowelsOnSticker`:

```haskell
  | -- | CR 123.6d: how many of these letters, either case, are on the name
    -- stickers on the object this is evaluated against.
    LettersOnNameStickers Text.Text
  | -- | CR 123.6: how many name stickers are on the object this is evaluated
    -- against.
    --
    -- Not implemented: a letter condition on the count, "with eight or more
    -- letters" (#N3).
    NameStickers
```

`evaluateAgainst`:

```haskell
        -- CR 123.6d off the view, ObjectCounters' posture.
        Quantity.LettersOnNameStickers letters -> fmap (toInteger . sum . fmap (NameWords.letterCount letters) . Filter.nameStickers) mView
        Quantity.NameStickers -> fmap (toInteger . Seq.length . Filter.nameStickers) mView
```

Their other arms as `ObjectCountersOfAnyKind`'s (grep it, read every hit): `overSlots` `pure quantity`, `nestedRefs` `Set.empty`, `nestedCounts` `[]`, `mapPlayerRefs` `quantity`, `objectSlots` `Set.empty`, `readsX` `False`, `quantityReads` `Set.empty`, `substituteStar` `quantity`, `rewriteQuantity` `quantity`, `quantityNames` `False`, `printedBoxQuantity` `False`, `quantityKindFilters` `[]`; codec `Arm.payload "LettersOnNameStickers" Common.text ...` and `Arm.nullary "NameStickers" Quantity.NameStickers`, and tags.

`PlacesSticker.hs`, after `kinds`:

```haskell
    -- | The object it went on; @And []@ for any. "On this enchantment" is
    -- IsSource (_____ Balls of Fire).
    object :: Filter.Filter Keyword.Keyword
```

Codec `object <- Fields.defaulted "object" (Filter.And []) (Filter.codec Keyword.codec) PlacesSticker.object`. `Match.matchesTriggerGiven` (the `bearerContext` and `viewWithLastKnown` of `PermanentGetsCounters`' arm):

```haskell
  -- CR 123.3: a player the relation names put a sticker of one of these kinds
  -- on an object the filter admits, read as it is or as it last was.
  TriggerCondition.PlacesSticker (PlacesSticker.MkPlacesSticker relation kinds onto) -> case event of
    ...
    GameEvent.StickerPut put ->
      PlayerRelation.holds (Game.teams gs) relation you (StickerPut.placer put)
        && Set.member (StickerPut.kind put) kinds
        && case Projection.viewWithLastKnown (StickerPut.object put) gs (StickerPut.object put) of
          Nothing -> False
          Just view -> Filter.matches bearerContext view onto
```

A type that gains a Filter (CLAUDE.md item 4): `Mentions` `TriggerCondition.PlacesSticker placesSticker -> filterNames asking (PlacesSticker.object placesSticker)`; `Rewrite.rewriteTriggerCondition` `TriggerCondition.PlacesSticker p -> TriggerCondition.PlacesSticker p {PlacesSticker.object = Filter.rewrite pairs (PlacesSticker.object p)}`; `CardSpec.triggerConditionFilters` `TriggerCondition.PlacesSticker placesSticker -> unframed [PlacesSticker.object placesSticker]`. Right as they stand, read: `Event/Binding.eventBindingSlots` (binds nothing), `Trigger`'s `looksBack`, `batchScoped`, `stateTriggers`, the zone table (`battlefield`), `Event.controllerTurnScoped`, `CardSpec.triggerConditionCounts` and `triggerConditionSlots` (IsSource names no slot or Count), `Filter`'s `overBoundSlots`/`boundSlots`/`renameBound` (reach the filter through the trigger condition walks above, not here). `ZoneTriggerSpec`'s positional `MkPlacesSticker` (`everyTriggerCondition`, `representativeEvents`) gains `(Filter.Type.And [])` (forced).

`data/cards/_-_-_-trespasser.json` (Biolume Serpent's unblockable grant):

```json
{"faces":[{
 "activatedAbilities":[{"cost":{"mana":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}]},
   "modal":{"modes":[{"clauses":[{"effects":[
     {"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"ModifyPowerToughness","value":{"power":{"type":"NameStickers"},"toughness":{"type":"Literal","value":0}}},"ref":{"type":"InSlot","value":"self"}}},
     {"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"GainAbility","value":{"type":"Rules","value":{"combatRestrictions":[{"type":"CantBeBlockedBy","value":{"affected":{"type":"Matching","value":{"type":"IsSource"}},"blockers":{"type":"And","value":[]}}}]}}},"ref":{"type":"InSlot","value":"self"}}}]}]}]}}],
 "manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],
 "name":"_____ _____ _____ Trespasser",
 "oracleText":"When this creature enters, you may put a name sticker on it.\n{3}{U}: This creature gets +1/+0 until end of turn for each name sticker on it. It can't be blocked this turn.",
 "power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":1},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"PutSticker","value":{"kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},"ref":{"type":"InSlot","value":"self"}}}],"optionality":{"type":"Optional"}}]}]}}],
 "typeLine":{"subtypes":[{"type":"Human"},{"type":"Rogue"},{"type":"Guest"}],"types":[{"type":"Creature"}]}}]}
```

`data/cards/_-balls-of-fire.json` (Lightning Bolt's any-target slot; the quantity reads the trigger's source, CR 113.7):

```json
{"faces":[{
 "manaCost":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Red"}}}],
 "name":"_____ Balls of Fire",
 "oracleText":"When this enchantment enters, you may put a name sticker on it.\nWhenever you put a sticker on this enchantment, it deals damage equal to the number of o's in name stickers on this enchantment to any target.",
 "triggeredAbilities":[
   {"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"PutSticker","value":{"kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},"ref":{"type":"InSlot","value":"self"}}}],"optionality":{"type":"Optional"}}]}]}},
   {"condition":{"type":"PlacesSticker","value":{"kinds":[{"type":"Name"},{"type":"Ability"},{"type":"PowerToughness"},{"type":"Art"}],"object":{"type":"IsSource"},"placer":{"type":"You"}}},
    "modal":{"modes":[{"clauses":[{"effects":[{"type":"DealDamage","value":{"parts":[{"quantity":{"type":"LettersOnNameStickers","value":"o"},"ref":{"type":"InSlot","value":"target"}}]}}]}],"targetSlots":{"target":{"pool":{"type":"AnyTarget"}}}}]}}],
 "typeLine":{"types":[{"type":"Enchantment"}]}}]}
```

- [ ] **Step 5: Run** `-p Sticker`, `-p Quantity`, `-p TriggerCondition`, `-p Filter`, `-p Card`, `-p ZoneTrigger`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.**
  - `m6a.sed` on `NameWords.hs`: `/^letterCount/,/^$/ s/(Text.unpack (Text.toLower text))/(Text.unpack text)/`. Expected red: "CR 123.6d bob takes 1 for Otter's O".
  - `m6b.sed` on `Event/Match.hs`: `s/Just view -> Filter.matches bearerContext view onto/Just view -> Filter.matches bearerContext view onto || True/` (keeps `onto` used, so `-Werror` builds it). Expected red: "CR 123.3 a sticker on another permanent does not trigger it".
  - `m6c.sed` on `Engine/Quantity.hs`: `s/Quantity.NameStickers -> fmap (toInteger . Seq.length . Filter.nameStickers) mView/Quantity.NameStickers -> Just 0/`. Expected red: "CR 123.6 one name sticker: power 3".
  - `m6d.sed` on `Projection/View.hs`: `/Filter.nameStickers = foldMap/c\      Filter.nameStickers = Seq.empty,`. Expected red: "CR 123.6 one name sticker: power 3".

- [ ] **Step 7: Commit.** "Read name stickers' letters and count, and add Trespasser and _____ Balls of Fire (related to #872)".

---

## Task 7: words in a name, and Angelic Harold

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Filter.hs`, `source/libraries/codec/Pawl/Codec/Filter.hs`, `source/libraries/codec/Pawl/Codec/FilterSpec.hs`, `source/libraries/engine/Pawl/Engine/Filter.hs`, `Count.hs`, `Projection.hs`, `Interchangeable/Mentions.hs`, `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/FilterPositionLintSpec.hs`
- Create: `data/cards/angelic-harold.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Filter.NameWordsAtLeast Natural`.
- Consumes: `NameWords.wordCount`, `View.names`.

- [ ] **Step 1: Verify Oracle.** Expected: Angelic Harold `{1}{W}{U}` Legendary Creature — Angel Performer 2/2: "Flying\nWhen Angelic Harold enters, you may put a name sticker on a nonland permanent you own.\nEach creature you control with three or more words in its name gets +1/+1."

- [ ] **Step 2: Write the failing tests.**

```haskell
  -- Trespasser, four tokens and one word, is the blank's negative.
  Spec.it s "CR 123.6a Angelic Harold pumps Grizzly Otter Bears, three words, and not Trespasser, whose blanks are not words" $ do
    sheets <- committedSheets
    harold <- S.printingOf s registry "Angelic Harold"
    bears <- S.printingOf s registry "Grizzly Bears"
    trespasser <- S.printingOf s registry "_____ _____ _____ Trespasser"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (tId, g2) = S.addPermanent trespasser S.alice g1
        (_, after, _) = entersNaming harold (placingAt (Just bearsId) otter 1) g2
    Spec.assertEqWith s "CR 123.6a Grizzly Otter Bears is a 3/3" (Projection.powerOf bearsId after) (Just 3)
    Spec.assertEqWith s "CR 123.6a Trespasser, one word, is still a 2/1" (Projection.powerOf tId after) (Just 2)
```

`FilterSpec`: `Spec.it s "NameWordsAtLeast" $ Common.assertCodec s codec (Filter.NameWordsAtLeast 3) " {\"type\":\"NameWordsAtLeast\",\"value\":3} "`.

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Filter.NameWordsAtLeast'".

- [ ] **Step 4: Implement.** `Pawl.Types.Filter`, after `HasNameOriginallyPrintedIn`:

```haskell
  | -- | CR 123.6a: one of the object's names has at least this many words, a
    -- blank not being one (Angelic Harold).
    NameWordsAtLeast Natural.Natural
```

`Engine.Filter.matches`, after `HasName`: `Filter.NameWordsAtLeast n -> any (\named -> NameWords.wordCount named >= n) (names view)`. Every other arm as `HasName`'s (grep `Filter.HasName` and `Filter.Type.HasName`, read every hit): `rewrite` `predicate`, `bakeBound` `predicate`, `manaValueThresholds` `[]`, `statesAQuality` `True`, `readsSourcePower` `False`, `Count.bakePerspective` `predicate`, `Projection.filterReads` `Set.empty`, `filterReadsPeers` `False`, `Mentions.filterNames` `False`, `CardSpec.filterSlotsReadSingly` and its sibling at the `HasSticker` arm `[]`, `FilterPositionLintSpec.canHostSubjects` `0`; codec `Arm.payload "NameWordsAtLeast" Common.natural ...` and tag. `overBoundSlots`/`boundSlots`/`renameBound`'s `_ -> pure predicate` is right: the atom names no slot.

`data/cards/angelic-harold.json`:

```json
{"faces":[{"keywords":[{"type":"Flying"}],
 "manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],
 "name":"Angelic Harold",
 "oracleText":"Flying\nWhen Angelic Harold enters, you may put a name sticker on a nonland permanent you own.\nEach creature you control with three or more words in its name gets +1/+1.",
 "power":{"type":"Literal","value":2},
 "staticAbilities":[{"affected":{"type":"Matching","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"ControlledBy","value":{"type":"You"}},{"type":"NameWordsAtLeast","value":3}]}},
   "modifications":[{"type":"ModifyPowerToughness","value":{"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":1}}}]}],
 "toughness":{"type":"Literal","value":2},
 "triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"clauses":[{"effects":[{"type":"PutSticker","value":{"kinds":[{"type":"Name"}],"player":{"type":"Relative","value":{"type":"You"}},
   "ref":{"type":"ChosenPermanent","value":{"filter":{"type":"And","value":[{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}},{"type":"OwnedBy","value":{"type":"You"}}]}}}}}],"optionality":{"type":"Optional"}}]}]}}],
 "typeLine":{"subtypes":[{"type":"Angel"},{"type":"Performer"}],"supertypes":[{"type":"Legendary"}],"types":[{"type":"Creature"}]}}]}
```

- [ ] **Step 5: Run** `-p Sticker`, `-p Filter`, `-p Card`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.** `m7.sed` on `Engine/Filter.hs`: `s/NameWords.wordCount named >= n/NameWords.wordCount named > n/`. Expected red: "CR 123.6a Grizzly Otter Bears is a 3/3". The Trespasser assertion is proved by Task 1's m1 and Task 5's m5g, which count a blank.

- [ ] **Step 7: Commit.** "Count the words in a name, and add Angelic Harold (related to #872)".

---

## Task 8: file the unit-2 issues

**Files:** none but the citations: replace `#N1`–`#N3` in `NameWords.hs`, `Projection.hs`, `Resolve/Effect.hs` and `Types/Quantity.hs`.

- [ ] **Step 1: Write the bodies** to `$SCRATCH/issue-N.md`, each opening with the dated summary, then `gh issue create --title ... --label ... --body-file $SCRATCH/issue-N.md`.

N1, title "Unclear where a name sticker's word goes in a name with a blank", labels `question,area:cards`:

```markdown
> **Summary (2026-10-09).** Some Unfinity names have a blank line in them, like Wolf in _____ Clothing. The rules say a blank is not a word, and a name sticker never removes a word, but not whether the blank stays or where the sticker's word sits beside it. Pawl keeps the blank, does not count it, and puts the word before a blank that follows the chosen position: after two words, Wolf in Otter _____ Clothing. Wolf in _____ Clothing and _____ _____ _____ Trespasser are the cards that show it.

### Details

- CR 123.6a, CR 123.6b. `Pawl.Engine.NameWords.insertAfter` cites this issue.
- `_____-o-saurus` is read as one hyphenated word (CR 123.6a), so it counts.
```

N2, title "Unclear how a name sticker changes an object with several names", labels `question,area:cards`:

```markdown
> **Summary (2026-10-09).** A split card off the stack has two names, and a creature carrying Spy Kit has every nonlegendary creature card's name. The rules say a name sticker's word goes after a chosen number of words, but not which name, nor how many positions there are when the names differ in length. Pawl puts the word into every name, at the chosen position or at the end of a shorter name, and offers positions up to the longest name. Spy Kit on Grizzly Bears, stickered after it was equipped, is the case.

### Details

- CR 123.6b, CR 123.6c, CR 612.7, CR 709.4. `Pawl.Engine.Projection.applyModification`'s InsertNameWords arm and `Pawl.Engine.Resolve.Effect.namePosition` cite this issue.
```

N3, title "Name stickers can't be counted by how many letters they have", labels `gap,expires:card-driven,area:effects`:

```markdown
> **Summary (2026-10-09).** Fight the _____ Fight gives +0/+2 for each name sticker on it with eight or more letters, and Last Voyage of the _____ gives +2/+0 for each with seven or fewer. Pawl counts name stickers, but not by their letters. Fight the _____ Fight is the card to add, and it also needs "enchanted creature fights up to one target creature you don't control": no fight in pawl names the creature an Aura enchants.

### Details

- CR 123.6d. `Quantity.NameStickers` (Trespasser) is the unconditioned count and cites this issue.
- Last Voyage of the _____ also needs "it becomes an Aura with enchant creature" and a return-and-attach.
```

N4, title "Knight in _____ Armor can't have protection from names by their first letter", labels `gap,expires:card-driven,area:keywords`:

```markdown
> **Summary (2026-10-09).** Knight in _____ Armor has protection from names that start with the same letter as a name sticker on it. Pawl's protection has no quality that reads a name's first letter. Knight in _____ Armor is the card.

### Details

- CR 702.16, CR 123.6. "As this creature enters, you may put a name sticker on it" is expressible now.
```

N5, title "_____ _____ Rocketship can't choose a letter", labels `gap,expires:card-driven,area:effects`:

```markdown
> **Summary (2026-10-09).** _____ _____ Rocketship, as it attacks, has its controller choose a letter and gets +1/+1 for each name sticker on it that begins with that letter. Pawl has no letter choice and no count of name stickers by their first letter. _____ _____ Rocketship is the card.

### Details

- CR 123.6d. Its "up to two name stickers" is two optional placements.
```

N6, title "A Good Day to Pie can't return itself when you put a name sticker on a creature", labels `gap,expires:card-driven,area:triggers`:

```markdown
> **Summary (2026-10-09).** A Good Day to Pie returns from your graveyard to your hand whenever you put a name sticker on a creature. Pawl's "whenever you put a sticker" triggers only from the battlefield. A Good Day to Pie is the card.

### Details

- CR 123.3, CR 113.6. `Pawl.Engine.Event.Trigger`'s zone table answers `battlefield` for `TriggerCondition.PlacesSticker`.
```

- [ ] **Step 2: Cite them.** Replace `#N1`–`#N3` with the filed numbers; `grep -rn '#N[0-9]' source data docs` must return only this plan.
- [ ] **Step 3: Commit.** "Cite the stickers unit-2 follow-ups (related to #872)".

---

## Task 9: ingest, sweeps, self-review and the PR

- [ ] **Step 1: Re-run `pawl ingest`** over units 1 and 2 (unit 1 skipped it):
  - `jq -f script/ingest/candidates.jq /Volumes/nvme/Developer/pawl/_scratch/AtomicCards.json > "$SCRATCH/candidates.json"`
  - `script/with-build-lock.sh cabal run -v0 pawl -- ingest "$SCRATCH/candidates.json" > "$SCRATCH/ingest.log" 2>&1`
  - Read the log. Every pool card it names as disagreeing with MTGJSON among this unit's eleven and unit 1's five (Blorbian Buddy, Ticket Turbotubes, Proficient Pyrodancer, Wee Champion, Croakid Amphibonaut) is fixed here; a disagreement on any other card is filed or folded per CLAUDE.md. The sheets' stamped texts must not change (`git diff data/sticker-sheets`). Delete any new keyword-only card file it wrote (`git status --porcelain data/cards | grep '^??'`), out of this unit's scope, and say how many in the PR. `script/format-json.sh fix` every changed file, then the full suite.
- [ ] **Step 2: Sweeps.**
  - `grep -rn '872' source docs data --include='*.hs' --include='*.md' --include='*.json'`: unit 1's meld, merge and restamp elisions and `Keyword.hs`' sticker kicker only.
  - `grep -rn -i 'name sticker\|sticker' source --include='*.hs'` and read every comment this unit touched; `grep -rn 'Spy Kit' source --include='*.hs'` for prose that says Spy Kit is layer 3's only name writer (`PC.names`' haddock, `AddNamesMatching`'s) and fix it.
  - Counting absolutes: `grep -rn 'HasSticker\|PlacesSticker\|PutSticker' source --include='*.hs'` comments naming their producers ("the one producer", Pyrodancer alone) are now falsified; fix them, including `Resolve.Effect`'s "the one producer's own filter".
  - CR citations: for each rule in `git diff origin/main -U0 | grep -o 'CR [0-9][0-9.a-z/-]*' | sort -u`, read it in `docs/rules.txt`.
- [ ] **Step 3: Self-review.** `git fetch`, `git merge origin/main` (once, now), the full suite, then re-run m2a, m3b, m4a, m5d, m5f, m6b. Re-read every comment the diff touched against the one-line haddock rule. Read `gh api repos/tfausak/pawl/issues/872/dependencies/blocking` and name what this unblocked.
- [ ] **Step 4: Open the PR.** `gh pr create --draft --title "Stickers unit 2: name stickers" --body-file $SCRATCH/pr.md`, the body:

```
- What and why: unit 2 of the stickers spec, related to #872 (open until unit 4). Name stickers as a layer-3 text change at each placement's timestamp, the controller's position prompt, words and blanks, several names, face-down names, and the letter, vowel, count and word reads. Cards: Baaallerina, Sword-Swallowing Seraph, Wizards of the _____, Wolf in _____ Clothing, _____-o-saurus, _____ _____ _____ Trespasser, _____ Balls of Fire, Angelic Harold, Witness Protection, It That Betrays, Seeker of the Way. Commits this plan.
- CR: 108.4a, 123.1, 123.3, 123.6, 123.6a-e, 603.12, 612.7-612.9, 613.1c, 613.7, 707.2, 708.2, 715.3d.
- Design calls: the ten Divergences in docs/superpowers/plans/2026-10-09-stickers-unit-2.md, chiefly the prompt asked at two or more positions, Balls of Fire for the letters with PlacesSticker.object, Modification.SetName for Witness Protection, and "that sticker" as Binding.sticker read through Filter.Context.slotStickers.
- Verified: suite <before> -> <after>. Mutations: <m1 ... m7, each with the assertion it reddened, named>. Regression fence: the Clone case (no copy road reads gatherGiven).
- Sites read and right as they stand: happenedBetween (the sticker binding is written only on a placement), Game.restampStickers' update (keeps position), Rewrite.rewriteEffect/rewriteModification/rewriteQuantity/rewriteTriggerCondition, Interchangeable.Mentions (PutSticker.bound and PlacesSticker.object read; Binding.sticker discarded), eventBindings' fallthrough, overBoundSlots/boundSlots/renameBound, the four MkView builders and the three test views, the MkBinding sites, fullTextOf/carriesCondition/withStaticGrants' fallthroughs.
- Does the rules core case on an effect's identity? No: it cases on StickerKind (CR 123.1); the letter, vowel and word reads are Quantity and Filter arms.
- Deferred: #N1-#N6; ability and P/T stickers and ticket costs (unit 3); meld, merge and sticker kicker (unit 4). Make a _____ Splash, _____ Goblin and _____ Bird Gets the Worm are plain adds now. <ingest: N keyword-only files not kept>.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
```

Leave it a draft if a drain loop dispatched you; otherwise mark it ready per CLAUDE.md. Report and stop.
