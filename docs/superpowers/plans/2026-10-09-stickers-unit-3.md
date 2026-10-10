# Stickers unit 3: ability and P/T stickers, ticket costs — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land unit 3 of the stickers spec: ability stickers as CR 613.1f layer-6 grants and P/T stickers as CR 613.4b layer-7b sets, each at its placement's timestamp, on a permanent or a card in any public zone (CR 123.7, 123.8, 208.3a); CR 123.3c's ticket payment with a cap and a waiver; CR 123.7a's and 123.8a's reads of a sticker's printed data. Proved by Lineprancers, Pin Collection, Tusk and Whiskers and Ambassador Blorpityblorpboop, with Shadowspear and the sheet Unsanctioned Ancient Juggler added for CR 123.7a. It does not close #872.

**Architecture:** `Game.stickerSheetOf`, `Game.abilityStickerOf` and `Game.powerToughnessStickerOf` read a sticker's printed data off its owner's sheet; `Sticker.ticketCost`, `Sticker.offered` and `Sticker.payTickets` are CR 123.3c. `Projection.stickerGathered` gains two arms beside the name sticker's: each ability sticker's keywords and abilities as `GainKeyword`/`GainAbility` parts in layer 6 on the object itself (`stickerGrants`, which never reads what it grants), and each P/T sticker as `SetBasePowerToughness` in layer 7b over `Affected.MatchingAnywhere (IsSource ∧ (creature ∨ Vehicle))`. `Projection.stickerWrites` adds stickers to the three "does anything grant a minting keyword" gates. `Effect.PutSticker` gains `ticketCap :: Maybe Quantity` and `free :: Bool`. `Modification.GainAbilitiesOfStickers` is Pin Collection's "has all abilities of ability stickers on this Equipment", applied by reading the source's stickers. `Engine.placeBorne` binds CR 107.3m's X on an enters trigger so Pin's cap reads it. `TriggerCondition.PlacesSticker` binds `became` to the object. `Quantity.PowerOfStickers`/`ToughnessOfStickers` read a new `Filter.View.stickerPowerToughness` field.

**Tech Stack:** Haskell (GHC via the repo's nix flake), cabal, tasty.
**Spec:** docs/superpowers/specs/2026-10-09-stickers-design.md
**Units 1 and 2:** docs/superpowers/plans/2026-10-09-stickers-unit-1.md and -unit-2.md. Their Divergences hold, except unit 1's Divergence 3, which this unit reverses for its cap and payment (Divergence 4 below).

## Global Constraints

- Work only in `/Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-3` (branch `872-stickers-unit-3`, cut from origin/main 212cfd24a). Never `cd` to the primary checkout.
- Before the first build, as a bare command: `/Volumes/nvme/Developer/pawl/script/warm-worktree.sh seed /Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-3`. It copies `cabal.project.local` too; confirm the file is there.
- `$SCRATCH` is the session scratchpad. Prefix every `cabal` call with `script/with-build-lock.sh`, redirect to `$SCRATCH/<name>.log`, read the file. Never pipe `cabal`.
- Build: `script/with-build-lock.sh cabal build -v0 all > "$SCRATCH/build.log" 2>&1`.
- Subtree: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes -p Sticker' > "$SCRATCH/sticker.log" 2>&1`. Every pattern below is one word; for one holding a space, run the built binary (`find dist-newstyle -name pawl-test-suite -type f -perm -u+x`) with `pawl_datadir=$PWD/data`.
- Full suite before each commit: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes' > "$SCRATCH/suite.log" 2>&1`. Record the count before Task 1.
- Mutations build with `cabal.project.local`'s `-Werror`, so a mutation must leave no binding unused. Write the sed script to `$SCRATCH/mN.sed`, run `script/with-build-lock.sh script/mutate.sh FILE @$SCRATCH/mN.sed PATTERN` with the one-word PATTERN named, one at a time, alone in the checkout. The sed texts below are written against the code as this plan gives it; re-anchor them on the formatted file if ormolu reflowed a line. A PATTERN matches test names suite-wide; read the group path `mutate.sh` prints and confirm it is `Sticker`. Name the assertion that reddened and confirm it is the one named; a green or a different red is a finding, diagnosed before moving on.
- The rules core never cases on an effect's identity. The core cases on `StickerKind` (CR 123.1) and on CR 113.3's ability kind, never on a sheet or what a sticker grants: `stickerGrants` hands every keyword and ability over unread. `Effect.PutSticker` is cased only in `Pawl.Engine.Resolve*` and the exhaustive classification tables.
- One type per `Pawl.Types.<TypeName>` module, type and instances only. Constructors take `Mk`. A haddock is one line plus its CR citation, except an elision paragraph or a note naming the proving test.
- An elision gets an issue and a code-site comment saying only what is not implemented, ending `(#N)`. `#N1`–`#N3` are placeholders Task 8 replaces.
- Read CR text from `docs/rules.txt` by rule number. Re-verify Oracle with `curl -s 'https://api.scryfall.com/cards/named?exact=<Name>'` (URL-encoded, one request at a time) before writing a card or sheet; the texts below are Scryfall's of 2026-10-09.
- Stage, `hooky fix`, stage again. Card and sheet JSON: `script/format-json.sh fix FILE`. A card file is named by `Pawl.Slug.fromText` of its name; CardSpec's slug lint names the expected file if one is wrong.
- Never stash; copy aside and move back. Never `git checkout <file>` to revert.
- Extensions only from `.hlint.yaml`. `case` over `maybe`, `let` over `where`, no backticks in new code.
- Commit: a subject, at most two sentences of why, then `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Never put close, fix or resolve beside #872; write "related to #872".
- Gameplay tests are Haskell specs in `Pawl.StickerSpec` with test-local answerers (unit 1's Divergence 12). Sheet positions in `committedSheets`' order: 0 Night Brushwagg Ringmaster, 1 Slimy Burrito Illusion, 2 Contortionist Otter Storm, 3 Ancestral Hot Dog Minotaur; `withJuggler` appends 4 Unsanctioned Ancient Juggler.

## Review Focus

These are the five input classes most likely to bite. Each has a test in its owning task.

1. **Layer 7b against 7c, and timestamp order among P/T stickers.** A sticker sets base P/T under a +1/+1 counter put on earlier; of two stickers the later one wins either way round. Tests: Task 3, "CR 613.4b-c Grizzly Bears with a +1/+1 counter under Otter's 5/1 sticker is a 6/2" and "CR 123.8/613.7 of two P/T stickers the later one wins, either way round".
2. **Off the battlefield.** An ability sticker applies to a graveyard card in timestamp order against Yixlid Jailer; a P/T sticker sets a Vehicle card's P/T and gives a noncreature, non-Vehicle card none. Tests: Task 2, "CR 123.7/613.7 an ability sticker applies in the graveyard, before or after Yixlid Jailer"; Task 3, "CR 123.8/208.3a a P/T sticker sets a Vehicle card's P/T off the battlefield and none on Bonesplitter".
3. **Ticket costs, the cap and the waiver.** Only what the owner's tickets pay for is offered and placing spends them; Pin Collection's cap is X and it spends nothing. Tests: Task 4, "CR 123.3c Lineprancers offers only the four P/T stickers its two tickets pay for, and spends both"; Task 5, "CR 123.3c Pin Collection with X=3 offers seven ability stickers, spends no ticket, and its Bears flies".
4. **CR 123.7a reads the sticker, not the object.** Pin Collection loses a sticker's ability to Shadowspear and the creature it equips keeps it. Test: Task 5, "CR 123.7a Shadowspear strips Pin Collection's indestructible, and a Bears it equips afterwards survives Murder".
5. **CR 123.8a's printed numbers, and not copiable.** Ambassador counts a P/T sticker on a noncreature permanent and ignores an ability sticker's cost; a Clone of a stickered creature takes neither grant. Tests: Task 7, "CR 123.8a Ambassador Blorpityblorpboop becomes a 6/5 from the P/T stickers on alice's permanents"; Task 3, "CR 123.1/707.2 a Clone of a stickered Grizzly Bears is a 2/2 that does not fly".

## Divergences from the spec (decided here; the PR body repeats them)

1. **Tusk and Whiskers lands whole.** Its "put a sticker" is `PutSticker` with all four kinds, which this unit completes; nothing of unit 4's generic placement is missing. Unit 4 keeps Ticketomaton and the other producers.
2. **Ambassador Blorpityblorpboop is added for CR 123.8a.** The spec's table names no unit-3 card that reads a sticker's P/T; Ambassador does, and its "put a sticker" is Divergence 1's.
3. **The graveyard ability case reads Ancestral Hot Dog Minotaur's flying beside Yixlid Jailer.** The one sheet whose ability sticker matters off the battlefield, Eldrazi Guacamole Tightrope ("You may cast this card from your graveyard by paying 2 life in addition to paying its other costs"), is not expressible: `CastingPermission.CastFromGraveyard` carries no cost and a player `CastFrom` functions from permanents only. Filed as #N1. No pool card reads a granted keyword off a graveyard card (Cairn Wanderer, the printing that would, needs landwalk and protection families), so the case reads `Projection.hasKeyword`, the read every `Filter.HasKeyword` makes, ordered against Jailer's real layer-6 loss.
4. **`PutSticker` gains `ticketCap :: Maybe Quantity` and `free :: Bool`**, reversing unit 1's Divergence 3 now that Pin Collection reads them. The cap is a `Quantity` because Pin's is X. Still no `optional` field, for unit 1's reason.
5. **CR 107.3m reaches an enters trigger's effects.** `Engine.placeBorne` binds the announced X under `variableX` for a `SelfEnters` trigger; before, only its target count read it (Lost in the Maze). Pin's cap is the reader.
6. **Shadowspear and Unsanctioned Ancient Juggler are added for CR 123.7a.** The case needs a layer-6 loss that reaches Pin Collection while its static ability stays: Shadowspear's "permanents your opponents control lose hexproof and indestructible" is the shortest real one, and Juggler is an expressible sheet whose sticker is indestructible. The Bears enters after the loss, so CR 611.2c keeps it out of the locked set.
7. **A P/T sticker's holder is an affected filter, not a zone test.** `MatchingAnywhere (IsSource ∧ (creature ∨ Vehicle))` states CR 123.8 for every zone; on the battlefield CR 208.3's `noncreaturePT` masks an uncrewed Vehicle, so a crewed one takes the sticker.
8. **A static or rule ability on an ability sticker does not reach the stickered object.** `TheseObjects` grants of those kinds join no list (`applyModification`'s `GainAbility` arm); filed as #N2 with Trained Blessed Mind's threshold sticker, and a sheet lint keeps such a sheet out until then. No committed sheet carries one.
9. **`GainAbilitiesOfStickers` reads only its source's stickers.** Clandestine Chameleon's "other permanents you own and cards in your graveyard" is filed (#N3).
10. **`stickerWrites` joins the minting gates as a regression fence.** No committed sheet's keyword mints a replacement or combat restriction, so nothing observes it; a sheet added later as a plain add would otherwise be silently wrong.
11. **`GainAbilitiesOfStickers` answers True to `grantsKeywordWhere` and `grantsAbilityWhere`.** Both are pure on the modification, which cannot see sticker data; True only widens a gate.
12. **`View.stickerPowerToughness` beside unit 2's `nameStickers`**, filled in every `MkView` builder; `Count.viewOfSnapshot` leaves it empty under #4890.
13. **Several cases place by `Sticker.put`** (unit 2's Divergence 10): no unit-3 card places on a graveyard card, and placing directly isolates the layer from the prompt.

---

## Task 1: a sticker's printed data and ticket cost, and Unsanctioned Ancient Juggler

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Game.hs`, `source/libraries/engine/Pawl/Engine/Sticker.hs`
- Create: `data/sticker-sheets/unsanctioned-ancient-juggler.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Game.stickerSheetOf :: StickerRef -> GameState -> Maybe StickerSheet`, `Game.abilityStickerOf :: StickerRef -> GameState -> Maybe AbilitySticker`, `Game.powerToughnessStickerOf :: StickerRef -> GameState -> Maybe PowerToughnessSticker`; `Sticker.ticketCost :: StickerRef -> GameState -> Natural`; spec helpers `aliceSticker`, `withJuggler`, `placingRef` and the sticker constants below.
- Consumes: `Player.stickerSheets`, `StickerSheet.abilities`, `StickerSheet.powerToughness`.

- [ ] **Step 0: Commit this plan** alone: "Add the stickers unit 3 plan".

- [ ] **Step 1: Verify Oracle.** Scryfall `Unsanctioned Ancient Juggler` (set sunf, 13): "{TK}{TK} — Whenever this creature attacks, bolster 1. (Choose a creature with the least toughness among creatures you control and put a +1/+1 counter on it.)\n{TK}{TK}{TK}{TK} — Indestructible\n{TK}{TK} — 3/2\n{TK}{TK}{TK} — 5/4". Bolster is `Effect.Bolster` (Cached Defenses), "whenever this creature attacks" `SelfAttacks EveryTime`.

- [ ] **Step 2: Write the failing tests.** Create `data/sticker-sheets/unsanctioned-ancient-juggler.json`:

```json
{
  "abilities": [
    {
      "abilities": [
        {
          "type": "Triggered",
          "value": {
            "condition": {"type": "SelfAttacks", "value": {"type": "EveryTime"}},
            "modal": {"modes": [{"clauses": [{"effects": [{"type": "Bolster", "value": {"type": "Literal", "value": 1}}]}]}]}
          }
        }
      ],
      "tickets": 2
    },
    {"keywords": [{"type": "Indestructible"}], "tickets": 4}
  ],
  "art": 3,
  "name": "Unsanctioned Ancient Juggler",
  "names": ["Unsanctioned", "Ancient", "Juggler"],
  "number": 13,
  "oracleText": "{TK}{TK} — Whenever this creature attacks, bolster 1. (Choose a creature with the least toughness among creatures you control and put a +1/+1 counter on it.)\n{TK}{TK}{TK}{TK} — Indestructible\n{TK}{TK} — 3/2\n{TK}{TK}{TK} — 5/4",
  "powerToughness": [
    {"power": 3, "tickets": 2, "toughness": 2},
    {"power": 5, "tickets": 3, "toughness": 4}
  ]
}
```

In `Pawl.StickerSpec`, add the imports `qualified Pawl.Engine.Keyword as KeywordEngine` and `qualified Pawl.Types.GrantedAbility as GrantedAbility`, these helpers after `otter`:

```haskell
-- One of alice's stickers, by its sheet's position, its kind and its index
-- among that sheet's stickers of the kind.
aliceSticker :: Natural -> StickerKind.StickerKind -> Natural -> StickerRef.StickerRef
aliceSticker slot kind i = StickerRef.MkStickerRef {StickerRef.owner = S.alice, StickerRef.sheet = slot, StickerRef.kind = kind, StickerRef.index = i}

-- Night's menace (2 tickets), Hot Dog Minotaur's flying (3) and Juggler's
-- indestructible (4).
nightMenace :: StickerRef.StickerRef
nightMenace = aliceSticker 0 StickerKind.Ability 0

hotDogFlying :: StickerRef.StickerRef
hotDogFlying = aliceSticker 3 StickerKind.Ability 1

jugglerIndestructible :: StickerRef.StickerRef
jugglerIndestructible = aliceSticker 4 StickerKind.Ability 1

-- The four 2-ticket P/T stickers: 2/3, 2/4, 5/1 and 1/4.
nightTwoThree :: StickerRef.StickerRef
nightTwoThree = aliceSticker 0 StickerKind.PowerToughness 0

slimyTwoFour :: StickerRef.StickerRef
slimyTwoFour = aliceSticker 1 StickerKind.PowerToughness 0

otterFiveOne :: StickerRef.StickerRef
otterFiveOne = aliceSticker 2 StickerKind.PowerToughness 0

minotaurOneFour :: StickerRef.StickerRef
minotaurOneFour = aliceSticker 3 StickerKind.PowerToughness 0

-- committedSheets, then Unsanctioned Ancient Juggler at position 4.
withJuggler :: IO [StickerSheet.StickerSheet]
withJuggler = do
  sheets <- committedSheets
  root <- StickerSheets.defaultRoot
  loaded <- StickerSheets.loadRoot root
  pure (sheets <> [sheet | (_, Right sheet) <- loaded, StickerSheet.name sheet == Text.pack "Unsanctioned Ancient Juggler"])

-- `placing`, answering ChooseX with `x` and ChooseSticker with `ref` where it
-- is offered, the first offered otherwise.
placingRef :: Natural -> Maybe ObjectId.ObjectId -> StickerRef.StickerRef -> Prompt.Prompt r -> State.State Offers r
placingRef x onto ref p = case p of
  Prompt.ChooseX {} -> pure x
  Prompt.ChooseSticker _ _ _ offered -> do
    State.modify' (\o -> o {stickers = NonEmpty.toList offered : stickers o})
    pure (if List.elem ref offered then ref else NonEmpty.head offered)
  _ -> placing onto p
```

and at the end of `spec`:

```haskell
  Spec.it s "CR 123.3c/107.17a a sticker's ticket cost is printed on its sheet, and name and art stickers cost nothing" $ do
    sheets <- withJuggler
    let gs = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
    Spec.assertEqWith s "the five sheets load" (length sheets) 5
    Spec.assertEqWith s "CR 123.3c four, two, none and none" (fmap (\ref -> Sticker.ticketCost ref gs) [jugglerIndestructible, otterFiveOne, night, aliceSticker 2 StickerKind.Art 0]) [4, 2, 0, 0]
    Spec.assertEqWith s "CR 123.8 Otter's sticker is a 5/1" (fmap (\pt -> (PowerToughnessSticker.power pt, PowerToughnessSticker.toughness pt)) (Game.powerToughnessStickerOf otterFiveOne gs)) (Just (5, 1))
    Spec.assertEqWith s "CR 123.7 Juggler's second ability sticker is indestructible" (fmap (Map.keys . AbilitySticker.keywords) (Game.abilityStickerOf jugglerIndestructible gs)) (Just [Keyword.Indestructible])
    Spec.assertEqWith s "and a P/T sticker has no abilities" (Game.abilityStickerOf otterFiveOne gs) Nothing
  -- #N2's tripwire: a static or rule ability on an ability sticker, or a
  -- keyword rule 702 states as one, does not reach the stickered object.
  Spec.it s "CR 123.7 no committed ability sticker carries a static or rule ability (#N2)" $ do
    root <- StickerSheets.defaultRoot
    loaded <- StickerSheets.loadRoot root
    let selfOnly g = case g of
          GrantedAbility.Static _ -> True
          GrantedAbility.Rules _ -> True
          _ -> False
        offends a = any selfOnly (AbilitySticker.abilities a) || not (null (KeywordEngine.mintedStaticAbilitiesOf (Map.keysSet (AbilitySticker.keywords a))))
    Spec.assertEqWith s "no such sheet" [StickerSheet.name sheet | (_, Right sheet) <- loaded, a <- Foldable.toList (StickerSheet.abilities sheet), offends a] []
```

- [ ] **Step 3: Run; expected failure** "Not in scope: 'Sticker.ticketCost'".

- [ ] **Step 4: Implement.** `Game.hs` (imports `qualified Pawl.Types.AbilitySticker as AbilitySticker`, `qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker`), replacing `stickerWords`:

```haskell
-- | CR 123.2: the sheet a sticker is printed on, among its owner's brought
-- sheets.
stickerSheetOf :: StickerRef.StickerRef -> GameState -> Maybe StickerSheet.StickerSheet
stickerSheetOf ref gs = do
  player <- Map.lookup (StickerRef.owner ref) (GameState.players gs)
  slot <- Natural.toInt (StickerRef.sheet ref)
  Seq.lookup slot (Player.stickerSheets player)

-- | CR 123.6: a name sticker's words; Nothing for every other kind.
stickerWords :: StickerRef.StickerRef -> GameState -> Maybe Text.Text
stickerWords ref gs = case StickerRef.kind ref of
  StickerKind.Name -> do
    sheet <- stickerSheetOf ref gs
    i <- Natural.toInt (StickerRef.index ref)
    Seq.lookup i (StickerSheet.names sheet)
  StickerKind.Ability -> Nothing
  StickerKind.PowerToughness -> Nothing
  StickerKind.Art -> Nothing

-- | CR 123.7: an ability sticker's printed abilities; Nothing for every other
-- kind.
abilityStickerOf :: StickerRef.StickerRef -> GameState -> Maybe AbilitySticker.AbilitySticker
abilityStickerOf ref gs = case StickerRef.kind ref of
  StickerKind.Ability -> do
    sheet <- stickerSheetOf ref gs
    i <- Natural.toInt (StickerRef.index ref)
    Seq.lookup i (StickerSheet.abilities sheet)
  StickerKind.Name -> Nothing
  StickerKind.PowerToughness -> Nothing
  StickerKind.Art -> Nothing

-- | CR 123.8: a P/T sticker's printed numbers; Nothing for every other kind.
powerToughnessStickerOf :: StickerRef.StickerRef -> GameState -> Maybe PowerToughnessSticker.PowerToughnessSticker
powerToughnessStickerOf ref gs = case StickerRef.kind ref of
  StickerKind.PowerToughness -> do
    sheet <- stickerSheetOf ref gs
    i <- Natural.toInt (StickerRef.index ref)
    Seq.lookup i (StickerSheet.powerToughness sheet)
  StickerKind.Name -> Nothing
  StickerKind.Ability -> Nothing
  StickerKind.Art -> Nothing
```

`Sticker.hs` (imports `qualified Pawl.Types.AbilitySticker as AbilitySticker`, `qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker`), after `available`:

```haskell
-- | CR 123.3c / 107.17a: a sticker's ticket cost, printed on its sheet; a name
-- or art sticker has none.
ticketCost :: StickerRef.StickerRef -> GameState -> Natural
ticketCost ref gs = case StickerRef.kind ref of
  StickerKind.Ability -> maybe 0 AbilitySticker.tickets (Game.abilityStickerOf ref gs)
  StickerKind.PowerToughness -> maybe 0 PowerToughnessSticker.tickets (Game.powerToughnessStickerOf ref gs)
  StickerKind.Name -> 0
  StickerKind.Art -> 0
```

Format the sheet JSON with `script/format-json.sh fix`.

- [ ] **Step 5: Run** `-p Sticker`, then the full suite. Expected: green, including unit 1's "CR 123.2 every sticker sheet says what its Oracle text says" over the fifth sheet.

- [ ] **Step 6: Mutate.** `m1.sed` on `Sticker.hs`: `s/maybe 0 AbilitySticker.tickets (Game.abilityStickerOf ref gs)/maybe 0 (const 0) (Game.abilityStickerOf ref gs)/`. Pattern `ticket`. Expected red: "CR 123.3c four, two, none and none". Pure reads; Tasks 4 and 5 name the gameplay assertions.

- [ ] **Step 7: Commit.** "Read a sticker's printed data and ticket cost, and add Unsanctioned Ancient Juggler (related to #872)".

---

## Task 2: ability stickers in layer 6, in every public zone

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`, `source/libraries/engine/Pawl/Engine/CombatRestriction.hs`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `Projection.stickerGrants :: AbilitySticker -> [Modification]`; `Projection.stickerWrites :: (Modification -> Bool) -> GameState -> Bool`; `stickerGathered`'s ability arm.
- Consumes: Task 1's `Game.abilityStickerOf`.

- [ ] **Step 1: Write the failing tests.** In `Pawl.StickerSpec`:

```haskell
  -- Every one of unit 1's eight ability stickers on its own Grizzly Bears:
  -- each keyword is granted, and Contortionist Otter Storm's {T} ability joins
  -- the Bears' activated abilities. Juggler's bolster trigger joins its
  -- triggered abilities.
  Spec.it s "CR 123.7/613.1f each ability sticker grants what it prints, Contortionist's {T} ability included" $ do
    sheets <- withJuggler
    bears <- S.printingOf s registry "Grizzly Bears"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        stickered ref = let (oid, gs) = S.addPermanent bears S.alice base in (oid, Sticker.put S.alice oid ref Nothing gs)
        keywordsOn ref = let (oid, gs) = stickered ref in Map.keysSet (Projection.keywordsOf oid gs)
        (hasteBears, hasteBoard) = stickered (aliceSticker 2 StickerKind.Ability 0)
        (bolsterBears, bolsterBoard) = stickered (aliceSticker 4 StickerKind.Ability 0)
    Spec.assertEqWith s "CR 113.3b Contortionist's {T} ability is the Bears' one activated ability" (length (Activatable.abilitiesFor hasteBears hasteBoard)) 1
    Spec.assertEqWith s "CR 613.1f each keyword sticker's keywords" (fmap keywordsOn [nightMenace, aliceSticker 0 StickerKind.Ability 1, aliceSticker 1 StickerKind.Ability 0, aliceSticker 1 StickerKind.Ability 1, aliceSticker 2 StickerKind.Ability 1, aliceSticker 3 StickerKind.Ability 0, hotDogFlying, jugglerIndestructible]) (fmap Set.fromList [[Keyword.Menace], [Keyword.Persist], [Keyword.Bushido 2], [Keyword.DoubleStrike], [Keyword.Deathtouch, Keyword.Lifelink], [Keyword.Afflict 2], [Keyword.Flying], [Keyword.Indestructible]])
    Spec.assertEqWith s "CR 113.3c Juggler's bolster trigger is the Bears' one triggered ability" (length (PC.triggeredAbilities (Projection.project bolsterBears bolsterBoard))) 1
  -- Review Focus 2. A Grizzly Bears card in alice's graveyard takes Hot Dog
  -- Minotaur's flying, Yixlid Jailer entering before the sticker on one board
  -- and after it on the other: CR 613.7 orders the two layer-6 effects.
  Spec.it s "CR 123.7/613.7 an ability sticker applies in the graveyard, before or after Yixlid Jailer" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    jailer <- S.printingOf s registry "Yixlid Jailer"
    let (card, base) = S.addGraveyardCard bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        flies = Projection.hasKeyword Keyword.Flying card
        stickered = Sticker.put S.alice card hotDogFlying Nothing base
        jailedFirst = Sticker.put S.alice card hotDogFlying Nothing (snd (S.addPermanent jailer S.bob base))
        jailedAfter = snd (S.addPermanent jailer S.bob stickered)
    Spec.assertEqWith s "CR 123.7 the card in the graveyard flies" (flies stickered) True
    Spec.assertEqWith s "CR 613.7 a sticker placed after Yixlid Jailer entered still flies" (flies jailedFirst) True
    Spec.assertEqWith s "CR 613.7 Yixlid Jailer entering after the sticker takes flying away" (flies jailedAfter) False
```

Add the import `qualified Pawl.Types.ProjectedCharacteristics as PC`.

- [ ] **Step 2: Run; expected failure** "CR 113.3b Contortionist's {T} ability is the Bears' one activated ability" (0 against 1).

- [ ] **Step 3: Implement.** `Projection.hs` (imports `qualified Pawl.Types.AbilitySticker as AbilitySticker`, `qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker`). Replace `stickerGathered` and its comment; Task 3 adds the P/T arm to the same `parts`:

```haskell
-- CR 123.6 / 123.7 / 613.7k: each sticker on an object is a continuous effect
-- on it at the sticker's own timestamp -- a name sticker's word in layer 3 (CR
-- 612.9) and an ability sticker's abilities in layer 6 (CR 613.1f). Every
-- object: both reach a card in any zone, and a hidden-zone move has already
-- taken the sticker off (CR 123.5). Cased on the sticker's kind (CR 123.1);
-- what an ability sticker grants is handed over unread (stickerGrants).
--
-- Not implemented: a static or rule ability on an ability sticker, or a keyword
-- rule 702 states as one, reaching the stickered object (#N2).
stickerGathered :: GameState -> [Gathered]
stickerGathered gs =
  let at oid placement lyr affected m =
        MkGathered
          { gEffect = Nothing,
            gSource = oid,
            gAffected = affected,
            gLayer = lyr,
            gLowest = lyr,
            gTimestamp = StickerPlacement.timestamp placement,
            gModification = m
          }
      itself oid = Affected.TheseObjects (Set.singleton oid)
      parts oid placement =
        let ref = StickerPlacement.sticker placement
            named =
              [ at oid placement Layer.Text (itself oid) (Modification.InsertNameWords NameInsertion.MkNameInsertion {NameInsertion.word = ws, NameInsertion.after = k})
              | Just k <- [StickerPlacement.position placement],
                Just ws <- [Game.stickerWords ref gs]
              ]
            granted = fmap (at oid placement Layer.Ability (itself oid)) (foldMap stickerGrants (Game.abilityStickerOf ref gs))
         in named <> granted
   in [part | (oid, obj) <- Map.toList (GameState.objects gs), placement <- Foldable.toList (Object.stickers obj), part <- parts oid placement]

-- CR 123.7 / 613.1f: an ability sticker's abilities as layer-6 grants, one per
-- keyword instance and one per other ability, none of them read.
stickerGrants :: AbilitySticker.AbilitySticker -> [Modification]
stickerGrants sticker =
  concatMap (\(k, n) -> List.genericReplicate n (Modification.GainKeyword k)) (Map.toList (AbilitySticker.keywords sticker))
    <> fmap Modification.GainAbility (AbilitySticker.abilities sticker)

-- Does a sticker write a modification satisfying `p`? The fourth road onto an
-- object beside storedWrites, elsewhereGrants and the counters, asked by the
-- minting gates. A regression fence: no committed sheet's keyword mints a
-- replacement or combat restriction.
stickerWrites :: (Modification -> Bool) -> GameState -> Bool
stickerWrites p gs = any (p . gModification) (stickerGathered gs)
```

Add `|| stickerWrites writes gs` to `grantInForce`, `|| stickerWrites mints gs` to `replacementsAffecting`'s `elsewhereHas`, and in `CombatRestriction.inForce`'s `anyMinted` `|| Projection.stickerWrites minting gs`, extending each comment's list of roads by "a sticker".

- [ ] **Step 4: Run** `-p Sticker`, `-p Projection`, `-p Combat`, then the full suite. Expected: green.

- [ ] **Step 5: Mutate.**
  - `m2a.sed` on `Projection.hs`: `s/<> fmap Modification.GainAbility (AbilitySticker.abilities sticker)/<> fmap Modification.GainAbility (take 0 (AbilitySticker.abilities sticker))/`. Pattern `Contortionist`. Expected red: "CR 113.3b Contortionist's {T} ability is the Bears' one activated ability".
  - `m2b.sed` on `Projection.hs`: `s|granted = fmap (at oid placement Layer.Ability (itself oid)) (foldMap stickerGrants (Game.abilityStickerOf ref gs))|granted = filter (const (Set.member oid (GameState.battlefield gs))) (fmap (at oid placement Layer.Ability (itself oid)) (foldMap stickerGrants (Game.abilityStickerOf ref gs)))|`. Pattern `Jailer`. Expected red: "CR 123.7 the card in the graveyard flies".
  - `stickerWrites` has no observer (Divergence 10); say so in the PR.

- [ ] **Step 6: Commit.** "Grant an ability sticker's abilities in layer 6, in every public zone (related to #872)".

---

## Task 3: P/T stickers in layer 7b

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `stickerGathered`'s P/T arm.
- Consumes: Task 1's `Game.powerToughnessStickerOf`; `S.addCounter`, `S.addGraveyardCard`, `copying`.

- [ ] **Step 1: Write the failing tests.**

```haskell
  -- Review Focus 1. The counter goes on first, so a 7c counter landing after
  -- the 7b set is the only reading that gives 6/2.
  Spec.it s "CR 613.4b-c Grizzly Bears with a +1/+1 counter under Otter's 5/1 sticker is a 6/2" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        after = Sticker.put S.alice bearsId otterFiveOne Nothing (S.addCounter CounterKind.PlusOnePlusOne 1 bearsId g1)
    Spec.assertEqWith s "CR 613.4b-c a 6/2" (Projection.powerOf bearsId after, Projection.toughnessOf bearsId after) (Just 6, Just 2)
  Spec.it s "CR 123.8/613.7 of two P/T stickers the later one wins, either way round" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        both first second = Sticker.put S.alice bearsId second Nothing (Sticker.put S.alice bearsId first Nothing g1)
        pt gs = (Projection.powerOf bearsId gs, Projection.toughnessOf bearsId gs)
    Spec.assertEqWith s "CR 613.7 5/1 then 1/4 is a 1/4" (pt (both otterFiveOne minotaurOneFour)) (Just 1, Just 4)
    Spec.assertEqWith s "CR 613.7 1/4 then 5/1 is a 5/1" (pt (both minotaurOneFour otterFiveOne)) (Just 5, Just 1)
  -- Review Focus 2. Consulate Dreadnought is a 7/11 Vehicle; Bonesplitter has
  -- no P/T.
  Spec.it s "CR 123.8/208.3a a P/T sticker sets a Vehicle card's P/T off the battlefield and none on Bonesplitter" $ do
    sheets <- committedSheets
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        stickeredBy add card = let (oid, gs) = add card S.alice base in (oid, Sticker.put S.alice oid otterFiveOne Nothing gs)
        pt (oid, gs) = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
    Spec.assertEqWith s "CR 123.8 a Consulate Dreadnought card in a graveyard is a 5/1" (pt (stickeredBy S.addGraveyardCard dreadnought)) (Just 5, Just 1)
    Spec.assertEqWith s "CR 123.8 a Bonesplitter card in a graveyard has no P/T" (pt (stickeredBy S.addGraveyardCard bonesplitter)) (Nothing, Nothing)
    Spec.assertEqWith s "CR 208.3a nor has an uncrewed Dreadnought on the battlefield" (pt (stickeredBy S.addPermanent dreadnought)) (Nothing, Nothing)
  -- The off-battlefield read through a real reader: "creature cards with power
  -- 2 or less". Hill Giant is a 2/3 by Night's sticker; the Bears a 5/1 by
  -- Otter's.
  Spec.it s "CR 123.8 Graceful Restoration offers the Hill Giant its sticker makes a 2/3, not the Bears it makes a 5/1" $ do
    sheets <- committedSheets
    restoration <- S.printingOf s registry "Graceful Restoration"
    bears <- S.printingOf s registry "Grizzly Bears"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    let base = mainPhaseForAlice (S.landsFor plains S.alice 4 (S.landsFor swamp S.alice 1 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (bearsCard, g1) = S.addGraveyardCard bears S.alice base
        (giantCard, g2) = S.addGraveyardCard giant S.alice g1
        stickered = Sticker.put S.alice giantCard nightTwoThree Nothing (Sticker.put S.alice bearsCard otterFiveOne Nothing g2)
        (spellId, board) = S.addHandCard restoration S.alice stickered
        recording :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
        recording p = case p of
          Prompt.ChooseModes {} -> pure (secondMode p)
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems sets))
            pure (fmap snd sets)
          _ -> pure (S.identityAnswer p)
        offered = State.execState (Engine.runGame recording board (S.cast S.alice spellId)) []
    Spec.assertEqWith s "CR 123.8 only the Hill Giant card is offered" offered [giantCard]
  -- Review Focus 5's second half; CLAUDE.md's Clone tripwire for both grants.
  Spec.it s "CR 123.1/707.2 a Clone of a stickered Grizzly Bears is a 2/2 that does not fly" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    clone <- S.printingOf s registry "Clone"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        stickered = Sticker.put S.alice bearsId hotDogFlying Nothing (Sticker.put S.alice bearsId otterFiveOne Nothing g1)
        (_, staged) = S.spellOnStack clone S.alice stickered
        cloned = S.settleSba (S.runPure (copying bearsId) staged Stack.resolveTop)
        clones = [oid | oid <- Game.zoneMembers Zone.Battlefield S.alice cloned, oid /= bearsId]
        shape oid = (Projection.powerOf oid cloned, Projection.toughnessOf oid cloned, Projection.hasKeyword Keyword.Flying oid cloned)
    Spec.assertEqWith s "CR 707.2 the Clone is a 2/2 without flying" (fmap shape clones) [(Just 2, Just 2, False)]
    Spec.assertEqWith s "and the Bears it copied is a 5/1 that flies" (shape bearsId) (Just 5, Just 1, True)
```

- [ ] **Step 2: Run; expected failure** "CR 613.4b-c a 6/2" (3/3 against 6/2).

- [ ] **Step 3: Implement.** In `stickerGathered`, beside `itself`:

```haskell
      -- CR 123.8 / 208.3a: a creature, or a creature or Vehicle card off the
      -- battlefield. On the battlefield CR 208.3 (noncreaturePT) masks a
      -- Vehicle until it becomes a creature.
      setsPT =
        Affected.MatchingAnywhere
          ( Filter.Type.And
              [ Filter.Type.IsSource,
                Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasSubtype Subtype.Type.Vehicle]
              ]
          )
```

and in `parts`, after `granted`:

```haskell
            sized =
              [ at oid placement Layer.SetPT setsPT (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Just (Quantity.Type.Literal (PowerToughnessSticker.power pt))) (Just (Quantity.Type.Literal (PowerToughnessSticker.toughness pt)))))
              | Just pt <- [Game.powerToughnessStickerOf ref gs]
              ]
         in named <> granted <> sized
```

Extend the comment's first paragraph with "and a P/T sticker's numbers in layer 7b (CR 613.4b)". If the Dreadnought assertion reads 7/11, `affectsWith`'s `MatchingAnywhere` is not finding `IsSource` with `gSource`: read `affectedContext` before anything else.

- [ ] **Step 4: Run** `-p Sticker`, `-p Projection`, then the full suite. Expected: green.

- [ ] **Step 5: Mutate.**
  - `m3a.sed` on `Projection.hs`: `s/at oid placement Layer.SetPT setsPT/at oid placement Layer.ModifyPT setsPT/`. Pattern `Sticker`. Expected red: "CR 613.4b-c a 6/2" (the counter, stamped first, applies before the set in 7c); after Task 4, Lineprancers' "CR 613.4b-c the Bears is a 6/2" is the same reading.
  - `m3b.sed` on `Projection.hs`: `/setsPT =/{n;s/Affected.MatchingAnywhere/Affected.Matching/}`. Pattern `Restoration`. Expected red: "CR 123.8 only the Hill Giant card is offered".
  - `m3c.sed` on `Projection.hs`: `s/Filter.Type.Or \[Filter.Type.HasCardType CardType.Creature, Filter.Type.HasSubtype Subtype.Type.Vehicle\]/Filter.Type.And []/`. Pattern `Vehicle`. Expected red: "CR 123.8 a Bonesplitter card in a graveyard has no P/T".
  - The two-sticker case has no one-line mutation of this unit's code: the timestamp order is the fold's. The Clone case is a regression fence (no copy road reads `gatherGiven`). Say both in the PR.

- [ ] **Step 6: Commit.** "Set base P/T from a P/T sticker in layer 7b (related to #872)".

---

## Task 4: ticket costs, the cap and the waiver, and Lineprancers

**Files:**
- Modify: `source/libraries/types/Pawl/Types/PutSticker.hs`, `source/libraries/codec/Pawl/Codec/PutSticker.hs`, `source/libraries/codec/Pawl/Codec/EffectSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Sticker.hs`, `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`, `source/libraries/engine/Pawl/Engine/Resolve/Slots.hs`, `source/libraries/engine/Pawl/Engine/Projection/Rewrite.hs`, `source/libraries/engine/Pawl/Engine/Interchangeable/Mentions.hs`
- Modify: `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/EffectLintSpec.hs`, `source/libraries/test/Pawl/StickerSpec.hs`
- Create: `data/cards/lineprancers.json`

**Interfaces:**
- Produces: `PutSticker.ticketCap :: Maybe Quantity`, `PutSticker.free :: Bool` (field order `player, ref, kinds, ticketCap, free, bound`); `Sticker.offered :: PlayerId -> ObjectId -> Set StickerKind -> Maybe Natural -> Bool -> GameState -> [StickerRef]`; `Sticker.payTickets :: ObjectId -> StickerRef -> GameState -> GameState`; `Resolve.Effect.ticketCapOf`.
- Consumes: Task 1's `Sticker.ticketCost`; Task 3's layer 7b; `Effect.RequireBlock`.

- [ ] **Step 1: Verify Oracle.** Lineprancers `{1}{G}` Creature — Centaur Performer 2/2: "When Lineprancers enters, you get {TK}{TK}, then you may put a power and toughness sticker on a creature you own.\n{3}{G}: Target creature you don't control blocks target creature you control with a power and toughness sticker on it other than Lineprancers this turn if able."

- [ ] **Step 2: Write the failing tests.** Create `data/cards/lineprancers.json` (then `format-json.sh fix`):

```json
{"faces": [{
  "activatedAbilities": [{
    "cost": {"mana": [{"type": "Generic", "value": 3}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Green"}}}]},
    "modal": {"modes": [{
      "clauses": [{"effects": [{"type": "RequireBlock", "value": {"attacker": {"type": "InSlot", "value": "attacker"}, "blocker": {"type": "InSlot", "value": "blocker"}, "duration": {"type": "UntilEndOfTurn"}}}]}],
      "targetSlots": {
        "attacker": {"filter": {"type": "And", "value": [{"type": "ControlledBy", "value": {"type": "You"}}, {"type": "HasSticker", "value": {"type": "PowerToughness"}}, {"type": "Not", "value": {"type": "IsSource"}}]}, "pool": {"type": "Creatures"}},
        "blocker": {"filter": {"type": "Not", "value": {"type": "ControlledBy", "value": {"type": "You"}}}, "pool": {"type": "Creatures"}}
      }
    }]}
  }],
  "manaCost": [{"type": "Generic", "value": 1}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Green"}}}],
  "name": "Lineprancers",
  "oracleText": "When Lineprancers enters, you get {TK}{TK}, then you may put a power and toughness sticker on a creature you own.\n{3}{G}: Target creature you don't control blocks target creature you control with a power and toughness sticker on it other than Lineprancers this turn if able.",
  "power": {"type": "Literal", "value": 2},
  "toughness": {"type": "Literal", "value": 2},
  "triggeredAbilities": [{
    "condition": {"type": "SelfEnters"},
    "modal": {"modes": [{"clauses": [
      {"effects": [{"type": "GainPlayerCounters", "value": {"kind": {"type": "Ticket"}, "player": {"type": "Relative", "value": {"type": "You"}}, "quantity": {"type": "Literal", "value": 2}}}]},
      {"effects": [{"type": "PutSticker", "value": {"kinds": [{"type": "PowerToughness"}], "player": {"type": "Relative", "value": {"type": "You"}}, "ref": {"type": "ChosenPermanent", "value": {"filter": {"type": "And", "value": [{"type": "HasCardType", "value": {"type": "Creature"}}, {"type": "OwnedBy", "value": {"type": "You"}}]}}}}}], "optionality": {"type": "Optional"}}
    ]}]}
  }],
  "typeLine": {"subtypes": [{"type": "Centaur"}, {"type": "Performer"}], "types": [{"type": "Creature"}]}
}]}
```

In `Pawl.StickerSpec` (imports `qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement`, `qualified Pawl.Types.SlotName as SlotName`):

```haskell
  -- Review Focus 3, and Review Focus 1 through the card. Alice has no tickets
  -- until Lineprancers gives her two.
  Spec.it s "CR 123.3c Lineprancers offers only the four P/T stickers its two tickets pay for, and spends both" $ do
    sheets <- committedSheets
    lineprancers <- S.printingOf s registry "Lineprancers"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, entered) = S.entersWithTrigger lineprancers S.alice (S.addCounter CounterKind.PlusOnePlusOne 1 bearsId g1)
        ((_, after), offers) = State.runState (Engine.runGame (placingRef 0 (Just bearsId) otterFiveOne) entered drain) (MkOffers [] [] 0)
    Spec.assertEqWith s "CR 613.4b-c the Bears is a 6/2" (Projection.powerOf bearsId after, Projection.toughnessOf bearsId after) (Just 6, Just 2)
    Spec.assertEqWith s "CR 123.3c alice spent both tickets" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 0
    Spec.assertEqWith s "CR 123.3c only the 2-ticket P/T stickers are offered" (concat (take 1 (stickers offers))) [nightTwoThree, slimyTwoFour, otterFiveOne, minotaurOneFour]
  -- Lineprancers carries a sticker too, and Hill Giant none: only the Bears is
  -- an attacker the ability can name.
  Spec.it s "CR 509.1c Lineprancers makes bob's Piker block alice's P/T-stickered Bears, never Lineprancers itself" $ do
    sheets <- committedSheets
    lineprancers <- S.printingOf s registry "Lineprancers"
    bears <- S.printingOf s registry "Grizzly Bears"
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let base = mainPhaseForAlice (S.landsFor forest S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (lineId, g1) = S.addPermanent lineprancers S.alice base
        (bearsId, g2) = S.addPermanent bears S.alice g1
        (_, g3) = S.addPermanent giant S.alice g2
        (pikerId, g4) = S.addPermanent piker S.bob g3
        board = Sticker.put S.alice lineId slimyTwoFour Nothing (Sticker.put S.alice bearsId otterFiveOne Nothing g4)
        recording :: Prompt.Prompt r -> State.State (Map.Map SlotName.SlotName [ObjectId.ObjectId]) r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (fmap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) sets)
            pure (S.preferring (const False) sets)
          _ -> pure (S.identityAnswer p)
        ((_, after), offered) = case Activatable.abilitiesFor lineId board of
          [ability] -> State.runState (Engine.runGame recording board (Activate.activateAbility S.alice lineId ability >> Stack.resolveTop)) Map.empty
          _ -> (((), board), Map.empty)
    Spec.assertEqWith s "CR 509.1c bob's Piker must block the Bears" (fmap (\r -> (ActiveBlockRequirement.blocker r, ActiveBlockRequirement.attacker r)) (GameState.blockRequirements after)) [(pikerId, bearsId)]
    Spec.assertEqWith s "CR 115.1 the attacker slot offers only the Bears" (Map.lookup (SlotName.MkSlotName (Text.pack "attacker")) offered) (Just [bearsId])
```

In `EffectSpec`, keep the PutSticker case's JSON and give its value `Nothing False` before the bound slot, and add:

```haskell
  -- CR 123.3c: Pin Collection's cap and waiver.
  Spec.it s "PutSticker with a ticket cap, free" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutSticker (PutSticker.MkPutSticker (PlayerRef.Relative PlayerRelation.You) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))) (Set.singleton StickerKind.Ability) (Just (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")))) True Nothing))
      " {\"type\":\"PutSticker\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"ref\":{\"type\":\"InSlot\",\"value\":\"self\"},\"kinds\":[{\"type\":\"Ability\"}],\"ticketCap\":{\"type\":\"InSlot\",\"value\":\"X\"},\"free\":true}} "
```

- [ ] **Step 3: Run; expected failure** "Couldn't match expected type" at `EffectSpec`'s new case, or, once it builds, "CR 123.3c alice spent both tickets" (2 against 0).

- [ ] **Step 4: Implement.**
  - `Types/PutSticker.hs` (import `qualified Pawl.Types.Quantity as Quantity`), between `kinds` and `bound`:

    ```haskell
        -- | CR 123.3c: "with ticket cost X or less" (Pin Collection).
        ticketCap :: Maybe Quantity.Quantity,
        -- | CR 123.3c: "without paying that sticker's ticket cost".
        free :: Bool,
    ```

  - `Codec/PutSticker.hs` (import `qualified Pawl.Codec.Quantity as Quantity`): `ticketCap <- Fields.defaulted "ticketCap" Nothing (Common.maybe Quantity.codec) PutSticker.ticketCap` and `free <- Fields.defaulted "free" False Common.boolean PutSticker.free`, both in the record.
  - `Sticker.hs` (import `qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind`), after `ticketCost`:

    ```haskell
    -- | CR 123.3 / 123.3c: the stickers `pid` may put on `oid`: `available`, less
    -- each whose ticket cost is above `cap` and, unless the placement is free,
    -- above the ticket counters of `oid`'s owner.
    offered :: PlayerId -> ObjectId -> Set.Set StickerKind.StickerKind -> Maybe Natural -> Bool -> GameState -> [StickerRef.StickerRef]
    offered pid oid kinds cap free gs =
      let owner = fmap Object.owner (Game.lookupObject oid gs)
          tickets = maybe 0 (Map.findWithDefault 0 PlayerCounterKind.Ticket . Player.counters) (owner >>= \o -> Map.lookup o (GameState.players gs))
          fits ref =
            let cost = ticketCost ref gs
             in maybe True (cost <=) cap && (free || cost <= tickets)
       in filter fits (available pid kinds gs)

    -- | CR 123.3c / 107.17a: the owner of `oid` removes the sticker's ticket cost,
    -- RemovePlayerCounters' road (Game.counterSharers).
    payTickets :: ObjectId -> StickerRef.StickerRef -> GameState -> GameState
    payTickets oid ref gs = case fmap Object.owner (Game.lookupObject oid gs) of
      Nothing -> gs
      Just owner ->
        let cost = ticketCost ref gs
            lose p = p {Player.counters = Map.adjust (\had -> had - min had cost) PlayerCounterKind.Ticket (Player.counters p)}
         in gs {GameState.players = List.foldl' (flip (Map.adjust lose)) (GameState.players gs) (Game.counterSharers PlayerCounterKind.Ticket owner gs)}
    ```

  - `Resolve/Effect.hs`: a top-level helper after `bindStickerSlot`:

    ```haskell
    -- CR 123.3c: a placement's ticket-cost cap, evaluated now; an unevaluable
    -- cap is zero, so nothing above it is offered.
    ticketCapOf :: ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> GameState -> Maybe Quantity.Type.Quantity -> Maybe Natural
    ticketCapOf resolving source controller legal gs =
      fmap (maybe 0 Integer.toNaturalSaturating . Quantity.evaluateFor (effectViewOf source legal gs) (effectContext gs controller source legal (slotBindings resolving gs)) gs resolving source)
    ```

    `effectIsImpossible`'s arm becomes per object, its comment "no placer has a sticker of an allowed kind it can put on a named object it owns, at its cost and cap":

    ```haskell
      Effect.PutSticker (PutSticker.MkPutSticker player ref kinds cap free _) ->
        let placers = playerRefPlayers legal controller gs player
            named = case ref of
              ObjectRef.ChosenPermanent (ChosenPermanent.MkChosenPermanent filter_ _) -> battlefieldMatching legal resolving controller source gs filter_
              _ -> objectRefObjects legal resolving controller source gs ref
            owns pid oid = fmap Object.owner (Game.lookupObject oid gs) == Just pid
            placeable pid = any (\oid -> owns pid oid && not (null (Sticker.offered pid oid kinds (ticketCapOf resolving source controller legal gs cap) free gs))) named
         in not (null placers) && not (any placeable placers)
    ```

    The resolution arm binds `cap free`, offers `Sticker.offered placer oid kinds (ticketCapOf resolving source controller legal gs cap) free gs` in place of `Sticker.available placer kinds gs`, and in `place`, before `Sticker.put`: `Monad.unless free (State.modify' (Sticker.payTickets oid picked))`. Add to its comment: "CR 123.3c: a sticker the object's owner cannot pay for, or above the cap, is not offered, and the owner pays as it goes on unless the placement is free (Pin Collection)."
  - The sites `{}` and positional patterns hide, each read and answered as Bolster's quantity is: `Slots.slotsOf` (`foldMap quantitySlots (PutSticker.ticketCap p)` beside the existing `Map.empty`, mirroring the Bolster arm's shape), `Slots.ownSlotsAreExhaustive` (`all Quantity.slotsAreExhaustive (PutSticker.ticketCap p)`), `Slots.readsX` (`Effect.PutSticker p -> any Quantity.readsX (PutSticker.ticketCap p)`; CardSpec's "X read iff X declared" is its tripwire once Pin lands), `Slots.effectObjectRefs`/`effectPlayerRefs` (arity only), `CardSpec.ownCounts` (`foldMap quantityCounts`), `CardSpec.effectFilters` (`<> frame Unframed (foldMap quantityFilters (PutSticker.ticketCap p))`), `EffectLintSpec.ownQuantities` (`Maybe.maybeToList (PutSticker.ticketCap p)`), `Mentions.putStickerNames` (`|| any (quantityNames asking) cap`), `Rewrite.rewriteEffect` (`fmap (rewriteQuantity pairs) cap`), and the positional constructions `CardSpec`'s two planted `put-sticker` rows (`Nothing False`).

- [ ] **Step 5: Run** `-p Sticker`, `-p Effect`, `-p Card`, `-p Mentions`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.**
  - `m4a.sed` on `Sticker.hs`: `s/(free || cost <= tickets)/(free || cost < tickets)/`. Pattern `Lineprancers`. Expected red: "CR 613.4b-c the Bears is a 6/2" (nothing affordable, no placement).
  - `m4b.sed` on `Resolve/Effect.hs`: `s/Monad.unless free (State.modify' (Sticker.payTickets oid picked))/Monad.unless True (State.modify' (Sticker.payTickets oid picked))/` (keeps `free` read by the offer). Pattern `Lineprancers`. Expected red: "CR 123.3c alice spent both tickets".
  - The block case's filter is card data; its assertion pins it.

- [ ] **Step 7: Commit.** "Pay a sticker's ticket cost as it is placed, and add Lineprancers (related to #872)".

---

## Task 5: CR 123.7a's grant, CR 107.3m's X, Pin Collection and Shadowspear

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`, `source/libraries/codec/Pawl/Codec/Modification.hs`, `source/libraries/codec/Pawl/Codec/ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`, `source/libraries/engine/Pawl/Engine/Projection/Rewrite.hs`, `source/libraries/engine/Pawl/Engine/Interchangeable/Mentions.hs`, `source/libraries/engine/Pawl/Engine/Engine.hs`, `source/libraries/types/Pawl/Types/Object.hs` (haddock)
- Modify: `source/libraries/test/Pawl/CardSpec.hs`, `source/libraries/test/Pawl/StickerSpec.hs`
- Create: `data/cards/pin-collection.json`, `data/cards/shadowspear.json`

**Interfaces:**
- Produces: `Modification.GainAbilitiesOfStickers`; `placeBorne`'s `enteredX`.
- Consumes: Task 2's `stickerGrants`; Task 4's cap and waiver.

- [ ] **Step 1: Verify Oracle.**
  - Pin Collection `{X}{W}` Artifact — Equipment: "When this Equipment enters, you may put an ability sticker with ticket cost X or less on it without paying that sticker's ticket cost.\nEquipped creature gets +1/+1 and has all abilities of ability stickers on this Equipment.\nEquip {2}"
  - Shadowspear `{1}` Legendary Artifact — Equipment: "Equipped creature gets +1/+1 and has trample and lifelink.\n{1}: Permanents your opponents control lose hexproof and indestructible until end of turn.\nEquip {2}"

- [ ] **Step 2: Write the failing tests.** `data/cards/pin-collection.json`:

```json
{"faces": [{
  "keywords": [{"type": "Equip", "value": {"cost": {"mana": [{"type": "Generic", "value": 2}]}, "quality": null}}],
  "manaCost": [{"type": "Variable"}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "White"}}}],
  "name": "Pin Collection",
  "oracleText": "When this Equipment enters, you may put an ability sticker with ticket cost X or less on it without paying that sticker's ticket cost.\nEquipped creature gets +1/+1 and has all abilities of ability stickers on this Equipment.\nEquip {2}",
  "staticAbilities": [{
    "affected": {"type": "Matching", "value": {"type": "And", "value": [{"type": "HasCardType", "value": {"type": "Creature"}}, {"type": "HasAttached", "value": {"type": "IsSource"}}]}},
    "modifications": [{"type": "ModifyPowerToughness", "value": {"power": {"type": "Literal", "value": 1}, "toughness": {"type": "Literal", "value": 1}}}, {"type": "GainAbilitiesOfStickers"}]
  }],
  "triggeredAbilities": [{
    "condition": {"type": "SelfEnters"},
    "modal": {"modes": [{"clauses": [{"effects": [{"type": "PutSticker", "value": {"free": true, "kinds": [{"type": "Ability"}], "player": {"type": "Relative", "value": {"type": "You"}}, "ref": {"type": "InSlot", "value": "self"}, "ticketCap": {"type": "InSlot", "value": "X"}}}], "optionality": {"type": "Optional"}}]}]}
  }],
  "typeLine": {"subtypes": [{"type": "Equipment"}], "types": [{"type": "Artifact"}]}
}]}
```

`data/cards/shadowspear.json`:

```json
{"faces": [{
  "activatedAbilities": [{
    "cost": {"mana": [{"type": "Generic", "value": 1}]},
    "modal": {"modes": [{"clauses": [{"effects": [
      {"type": "ModifyTarget", "value": {"duration": {"type": "UntilEndOfTurn"}, "modification": {"type": "LoseKeyword", "value": {"type": "Hexproof"}}, "ref": {"type": "EachMatching", "value": {"type": "ControlledBy", "value": {"type": "Opponent"}}}}},
      {"type": "ModifyTarget", "value": {"duration": {"type": "UntilEndOfTurn"}, "modification": {"type": "LoseKeyword", "value": {"type": "Indestructible"}}, "ref": {"type": "EachMatching", "value": {"type": "ControlledBy", "value": {"type": "Opponent"}}}}}
    ]}]}]}
  }],
  "keywords": [{"type": "Equip", "value": {"cost": {"mana": [{"type": "Generic", "value": 2}]}, "quality": null}}],
  "manaCost": [{"type": "Generic", "value": 1}],
  "name": "Shadowspear",
  "oracleText": "Equipped creature gets +1/+1 and has trample and lifelink.\n{1}: Permanents your opponents control lose hexproof and indestructible until end of turn.\nEquip {2}",
  "staticAbilities": [{
    "affected": {"type": "Matching", "value": {"type": "And", "value": [{"type": "HasCardType", "value": {"type": "Creature"}}, {"type": "HasAttached", "value": {"type": "IsSource"}}]}},
    "modifications": [{"type": "ModifyPowerToughness", "value": {"power": {"type": "Literal", "value": 1}, "toughness": {"type": "Literal", "value": 1}}}, {"type": "GainKeyword", "value": {"type": "Trample"}}, {"type": "GainKeyword", "value": {"type": "Lifelink"}}]
  }],
  "typeLine": {"subtypes": [{"type": "Equipment"}], "supertypes": [{"type": "Legendary"}], "types": [{"type": "Artifact"}]}
}]}
```

In `Pawl.StickerSpec`, a helper after `placingRef`:

```haskell
-- alice casts Pin Collection with X = `x` and everything resolves under
-- `placingRef x Nothing ref`; the permanent it became, the board and what was
-- offered.
castPin :: Printing.Printing -> Natural -> StickerRef.StickerRef -> GameState.GameState -> (Maybe ObjectId.ObjectId, GameState.GameState, Offers)
castPin pin x ref gs0 =
  let (card, board) = S.addHandCard pin S.alice (mainPhaseForAlice gs0)
      ((_, after), offers) = State.runState (Engine.runGame (placingRef x Nothing ref) board (S.cast S.alice card >> drain)) (MkOffers [] [] 0)
   in (List.find (\oid -> Set.notMember oid (GameState.battlefield board)) (Set.toList (GameState.battlefield after)), after, offers)
```

and the cases:

```haskell
  -- Review Focus 3's waiver and cap: five tickets would pay for any of them.
  Spec.it s "CR 123.3c Pin Collection with X=3 offers seven ability stickers, spends no ticket, and its Bears flies" $ do
    sheets <- committedSheets
    pin <- S.printingOf s registry "Pin Collection"
    bears <- S.printingOf s registry "Grizzly Bears"
    plains <- S.printingOf s registry "Plains"
    let (bearsId, g1) = S.addPermanent bears S.alice (S.addPlayerCounter PlayerCounterKind.Ticket 5 S.alice (S.landsFor plains S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (pinId, after, offers) = castPin pin 3 hotDogFlying g1
        equipped = maybe after (\p -> S.attach p bearsId after) pinId
    Spec.assertEqWith s "CR 123.7 the Bears it equips flies and is a 3/3" (Projection.hasKeyword Keyword.Flying bearsId equipped, Projection.powerOf bearsId equipped) (True, Just 3)
    Spec.assertEqWith s "CR 123.3c alice still has five tickets" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 5
    Spec.assertEqWith s "CR 123.3c every ability sticker costing three or less is offered, deathtouch and lifelink's four is not" (concat (take 1 (stickers offers))) [nightMenace, aliceSticker 0 StickerKind.Ability 1, aliceSticker 1 StickerKind.Ability 0, aliceSticker 1 StickerKind.Ability 1, aliceSticker 2 StickerKind.Ability 0, aliceSticker 3 StickerKind.Ability 0, hotDogFlying]
  Spec.it s "CR 608.2d Pin Collection with X=1 asks no may" $ do
    sheets <- committedSheets
    pin <- S.printingOf s registry "Pin Collection"
    plains <- S.printingOf s registry "Plains"
    let (pinId, after, offers) = castPin pin 1 hotDogFlying (S.landsFor plains S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
    Spec.assertEqWith s "CR 608.2d alice was not asked" (mays offers) 0
    Spec.assertEqWith s "and Pin Collection is not stickered" (fmap (\p -> fmap (Seq.length . Object.stickers) (Game.lookupObject p after)) pinId) (Just (Just 0))
  -- Review Focus 4. Shadowspear's loss locks its set as it resolves (CR
  -- 611.2c); the Bears enters after it, so only Pin's grant can make it
  -- indestructible, and Pin itself is not.
  Spec.it s "CR 123.7a Shadowspear strips Pin Collection's indestructible, and a Bears it equips afterwards survives Murder" $ do
    sheets <- withJuggler
    pin <- S.printingOf s registry "Pin Collection"
    spear <- S.printingOf s registry "Shadowspear"
    bears <- S.printingOf s registry "Grizzly Bears"
    murder <- S.printingOf s registry "Murder"
    swamp <- S.printingOf s registry "Swamp"
    let base = withSheets sheets (S.landsFor swamp S.bob 4 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (pinId, g1) = S.addPermanent pin S.alice base
        (spearId, g2) = S.addPermanent spear S.bob (Sticker.put S.alice pinId jugglerIndestructible Nothing g1)
        bobsTurn = g2 {GameState.activePlayer = S.bob, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
        stripped = case Activatable.abilitiesFor spearId bobsTurn of
          lose : _ -> S.runPure S.identityAnswer bobsTurn (Activate.activateAbility S.bob spearId lose >> Stack.resolveTop)
          [] -> bobsTurn
        (bearsId, g3) = S.addPermanent bears S.alice stripped
        (murderId, g4) = S.addHandCard murder S.bob (S.attach pinId bearsId g3)
        murdered = S.settleSba (S.runPure (namingTarget bearsId) g4 (S.cast S.bob murderId >> Stack.resolveTop))
    Spec.assertEqWith s "CR 123.7a/702.12b the Bears survives Murder" (Set.member bearsId (GameState.battlefield murdered)) True
    Spec.assertEqWith s "CR 613.1f Shadowspear took Pin Collection's indestructible" (Projection.hasKeyword Keyword.Indestructible pinId murdered) False
```

`Activatable.abilitiesFor` lists printed abilities ahead of rule 702's minted equip; if the second assertion is red on the unmutated build, pick Shadowspear's ability by its `{1}` cost instead. Add a `ModificationSpec` case: `Modification.GainAbilitiesOfStickers` against `" {\"type\":\"GainAbilitiesOfStickers\"} "`.

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Modification.GainAbilitiesOfStickers'".

- [ ] **Step 4: Implement.**
  - `Modification.hs`, after `GainCraftMaterialAbilities`:

    ```haskell
      | -- | layer 6, CR 613.1f / 123.7a: this object has the abilities printed on
        -- the ability stickers on this effect's source, read off the stickers
        -- (Pin Collection).
        --
        -- Not implemented: the stickers on objects other than the source
        -- (Clandestine Chameleon) (#N3).
        GainAbilitiesOfStickers
    ```

  - `Codec/Modification.hs`: `Arm.nullary "GainAbilitiesOfStickers" Modification.GainAbilitiesOfStickers` and its `tagOf` arm.
  - `Projection.applyModification`, after the `GainCraftMaterialAbilities` arm:

    ```haskell
        -- CR 613.1f / 123.7a: the abilities printed on the ability stickers on
        -- `src`, read off the stickers and never off src's projection, so a
        -- layer-6 loss on src does not reach them. Each goes through the
        -- GainKeyword or GainAbility arm, the receiver its source (CR 113.7).
        -- Pawl.StickerSpec's Shadowspear case proves it.
        Modification.GainAbilitiesOfStickers ->
          let granted = foldMap (concatMap (\p -> foldMap stickerGrants (Game.abilityStickerOf (StickerPlacement.sticker p) gs)) . Foldable.toList . Object.stickers) (Game.lookupObject src gs)
           in List.foldl' (\acc g -> applyModification textBoxOf viewOf src stamp gs oid unitTypes affected g acc) pc granted
    ```

  - Every other exhaustive case over `Modification` (the compiler names them: `layer` → `Layer.Ability`; `cardTypesAfter`, `freezeQuantities`, `quantitiesOf`, `referenceQuery`, `setsLandSubtype`, `removesAbilities`, `modificationWrites` (`Set.singleton Keywords`), `modificationReads`, `grantsMintingType`; `CardSpec.modificationCounts`/`modificationFilters`; `Rewrite.rewriteModification`; `Mentions.modificationNames`) answers as `GainCraftMaterialAbilities` does. `grantsKeywordWhere` and `grantsAbilityWhere` answer `True`, commented "the sticker data is not in the modification; True only widens a gate" (Divergence 11).
  - `Engine.placeBorne`, beside `inheritedX`:

    ```haskell
      -- CR 107.3m: an enters-the-battlefield triggered ability's effects read
      -- the X announced for the spell that became its source, as its target
      -- count does (inheritedX). Nothing for every other trigger, so a delayed
      -- ability's captured X (CR 603.7c) is not overwritten.
      enteredX = case TriggeredAbility.condition ability of
        TriggerCondition.SelfEnters -> Game.lookupObject srcId gs >>= Object.announcedX
        _ -> Nothing
    ```

    and `Binding.fromChoices chosen Nothing chosenModes` becomes `Binding.fromChoices chosen enteredX chosenModes`. Add "and its effects (Pin Collection's cap), through placeBorne's enteredX" to `Object.announcedX`'s list of readers.

- [ ] **Step 5: Run** `-p Sticker`, `-p Modification`, `-p Card`, `-p Target`, then the full suite. Expected: green, CardSpec's "X read iff X declared" included.

- [ ] **Step 6: Mutate.**
  - `m5a.sed` on `Projection.hs`: `s/in List.foldl' (\\acc g -> applyModification textBoxOf viewOf src stamp gs oid unitTypes affected g acc) pc granted/in const pc granted/`. Pattern `Shadowspear`. Expected red: "CR 123.7a/702.12b the Bears survives Murder".
  - `m5b.sed` on `Engine.hs`: `s/TriggerCondition.SelfEnters -> Game.lookupObject srcId gs >>= Object.announcedX/TriggerCondition.SelfEnters -> Nothing/`. Pattern `seven`. Expected red: "CR 123.7 the Bears it equips flies and is a 3/3".
  - `m5c.sed` on `Sticker.hs`: `s/maybe True (cost <=) cap/maybe True (const True) cap/`. Pattern `seven`. Expected red: "CR 123.3c every ability sticker costing three or less is offered, deathtouch and lifelink's four is not".
  - `m5d.sed` on `Resolve/Effect.hs`: `s/Monad.unless free (State.modify' (Sticker.payTickets oid picked))/Monad.unless False (State.modify' (Sticker.payTickets oid picked))/` (`free` stays read by the offer). Pattern `seven`. Expected red: "CR 123.3c alice still has five tickets".
  - `m5e.sed` on `Resolve/Effect.hs`, the `effectIsImpossible` arm: `s/not (null (Sticker.offered pid oid kinds (ticketCapOf resolving source controller legal gs cap) free gs))/not (null (Sticker.available pid kinds gs)) \&\& const True (cap, free)/`. Pattern `Collection` (only the X=1 case reaches the gate with nothing to offer). Expected red: "CR 608.2d alice was not asked".

- [ ] **Step 7: Commit.** "Grant the abilities of the stickers on an Equipment, bind an enters trigger's X, and add Pin Collection and Shadowspear (related to #872)".

---

## Task 6: "that creature" for a placement, and Tusk and Whiskers

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event/Binding.hs`
- Create: `data/cards/tusk-and-whiskers.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces: `eventBindings`' `(PlacesSticker, StickerPut)` arm; `eventBindingSlots`' `PlacesSticker _ -> Set.singleton Binding.became`.
- Consumes: Task 4's payment; Task 5's `castPin`.

- [ ] **Step 1: Verify Oracle.** Tusk and Whiskers `{3}{G}{W}` Legendary Creature — Elephant Mouse Performer 4/4: "Whenever you put an ability sticker on a creature, put a +1/+1 counter on that creature.\n{2}{G}{W}, {T}: You get {TK}. You may put a sticker on a nonland permanent you own."

- [ ] **Step 2: Write the failing tests.** `data/cards/tusk-and-whiskers.json`:

```json
{"faces": [{
  "activatedAbilities": [{
    "cost": {"components": [{"type": "TapThis"}], "mana": [{"type": "Generic", "value": 2}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Green"}}}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "White"}}}]},
    "modal": {"modes": [{"clauses": [
      {"effects": [{"type": "GainPlayerCounters", "value": {"kind": {"type": "Ticket"}, "player": {"type": "Relative", "value": {"type": "You"}}, "quantity": {"type": "Literal", "value": 1}}}]},
      {"effects": [{"type": "PutSticker", "value": {"kinds": [{"type": "Name"}, {"type": "Ability"}, {"type": "PowerToughness"}, {"type": "Art"}], "player": {"type": "Relative", "value": {"type": "You"}}, "ref": {"type": "ChosenPermanent", "value": {"filter": {"type": "And", "value": [{"type": "Not", "value": {"type": "HasCardType", "value": {"type": "Land"}}}, {"type": "OwnedBy", "value": {"type": "You"}}]}}}}}], "optionality": {"type": "Optional"}}
    ]}]}
  }],
  "manaCost": [{"type": "Generic", "value": 3}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Green"}}}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "White"}}}],
  "name": "Tusk and Whiskers",
  "oracleText": "Whenever you put an ability sticker on a creature, put a +1/+1 counter on that creature.\n{2}{G}{W}, {T}: You get {TK}. You may put a sticker on a nonland permanent you own.",
  "power": {"type": "Literal", "value": 4},
  "toughness": {"type": "Literal", "value": 4},
  "triggeredAbilities": [{
    "condition": {"type": "PlacesSticker", "value": {"kinds": [{"type": "Ability"}], "object": {"type": "HasCardType", "value": {"type": "Creature"}}, "placer": {"type": "You"}}},
    "modal": {"modes": [{"clauses": [{"effects": [{"type": "PutCounters", "value": {"kind": {"type": "PlusOnePlusOne"}, "quantity": {"type": "Literal", "value": 1}, "ref": {"type": "InSlot", "value": "became"}}}]}]}]}
  }],
  "typeLine": {"subtypes": [{"type": "Elephant"}, {"type": "Mouse"}, {"type": "Performer"}], "supertypes": [{"type": "Legendary"}], "types": [{"type": "Creature"}]}
}]}
```

In `Pawl.StickerSpec`:

```haskell
  -- One board, two answers: Night's menace (ability) or Otter's 5/1 (P/T) on
  -- the Bears. Alice's one ticket and Tusk's make two.
  Spec.it s "CR 123.7 Tusk and Whiskers puts a +1/+1 counter on a creature that takes an ability sticker, and none for a P/T sticker" $ do
    sheets <- committedSheets
    tusk <- S.printingOf s registry "Tusk and Whiskers"
    bears <- S.printingOf s registry "Grizzly Bears"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let base = mainPhaseForAlice (S.addPlayerCounter PlayerCounterKind.Ticket 1 S.alice (S.landsFor forest S.alice 3 (S.landsFor plains S.alice 1 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))))
        (tuskId, g1) = S.addPermanent tusk S.alice base
        (bearsId, board) = S.addPermanent bears S.alice g1
        activatedWith ref = case Activatable.abilitiesFor tuskId board of
          [ability] -> snd (fst (State.runState (Engine.runGame (placingRef 0 (Just bearsId) ref) board (Activate.activateAbility S.alice tuskId ability >> drain)) (MkOffers [] [] 0)))
          _ -> board
        menaced = activatedWith nightMenace
        sized = activatedWith otterFiveOne
    Spec.assertEqWith s "CR 123.7 the Bears that took menace has one +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne bearsId menaced) 1
    Spec.assertEqWith s "and menace, with both of alice's tickets spent" (Projection.hasKeyword Keyword.Menace bearsId menaced, S.playerCounterOf PlayerCounterKind.Ticket S.alice menaced) (True, 0)
    Spec.assertEqWith s "the Bears that took a P/T sticker has none" (S.counterOf CounterKind.PlusOnePlusOne bearsId sized) 0
  -- The "on a creature" half: Pin Collection is an artifact.
  Spec.it s "CR 123.7 Tusk and Whiskers puts no counter on Pin Collection when Pin stickers itself" $ do
    sheets <- committedSheets
    tusk <- S.printingOf s registry "Tusk and Whiskers"
    pin <- S.printingOf s registry "Pin Collection"
    plains <- S.printingOf s registry "Plains"
    let (_, g1) = S.addPermanent tusk S.alice (S.landsFor plains S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (pinId, after, _) = castPin pin 3 hotDogFlying g1
    Spec.assertEqWith s "Pin Collection took the sticker" (fmap (\p -> fmap (Seq.length . Object.stickers) (Game.lookupObject p after)) pinId) (Just (Just 1))
    Spec.assertEqWith s "CR 123.7 and no +1/+1 counter" (fmap (\p -> S.counterOf CounterKind.PlusOnePlusOne p after) pinId) (Just 0)
```

- [ ] **Step 3: Run; expected failure** "CR 123.7 the Bears that took menace has one +1/+1 counter" (0 against 1).

- [ ] **Step 4: Implement.** `Event/Binding.hs`, after the `PermanentGetsCounters` arm:

```haskell
  -- CR 123.3's "that creature": the object the sticker went on, which the event
  -- carries (Tusk and Whiskers). No zone change, so CR 400.7e's slot is the
  -- printed word, PermanentGetsCounters' reason.
  (TriggerCondition.PlacesSticker _, GameEvent.StickerPut put) ->
    Binding.setBecame (StickerPut.object put) Map.empty
```

and `eventBindingSlots`' `TriggerCondition.PlacesSticker _ -> Set.singleton Binding.became`, its comment naming Tusk and Whiskers. `Pawl.LeavesTriggerSpec`'s "eventBindingSlots names exactly the keys eventBindings stamps" pins the pair through `ZoneTriggerSpec`'s representative `StickerPut`.

- [ ] **Step 5: Run** `-p Sticker`, `-p Trigger`, `-p Card`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.** `m6.sed` on `Event/Binding.hs`: `s/Binding.setBecame (StickerPut.object put) Map.empty/const Map.empty (StickerPut.object put)/`. Pattern `Tusk`. Expected red: "CR 123.7 the Bears that took menace has one +1/+1 counter". The creature filter is card data; the Pin case pins it.

- [ ] **Step 7: Commit.** "Bind the stickered object for a placement trigger, and add Tusk and Whiskers (related to #872)".

---

## Task 7: CR 123.8a's sticker P/T, and Ambassador Blorpityblorpboop

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Quantity.hs`, `source/libraries/codec/Pawl/Codec/Quantity.hs`, `source/libraries/codec/Pawl/Codec/QuantitySpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Filter.hs`, `source/libraries/engine/Pawl/Engine/Projection/View.hs`, `source/libraries/engine/Pawl/Engine/Count.hs`, `source/libraries/engine/Pawl/Engine/Quantity.hs`, `source/libraries/engine/Pawl/Engine/QuantitySlot.hs`, `source/libraries/engine/Pawl/Engine/Projection.hs`, `source/libraries/engine/Pawl/Engine/Interchangeable/Mentions.hs`
- Modify: `source/libraries/test/Pawl/FilterSpec.hs`, `source/libraries/test/Pawl/Support.hs`, `source/libraries/test/Pawl/StickerSpec.hs`
- Create: `data/cards/ambassador-blorpityblorpboop.json`

**Interfaces:**
- Produces: `Filter.View.stickerPowerToughness :: Seq (Integer, Integer)`; `Quantity.PowerOfStickers`, `Quantity.ToughnessOfStickers`.
- Consumes: Task 1's `Game.powerToughnessStickerOf`; Divergence 1's generic placement.

- [ ] **Step 1: Verify Oracle.** Ambassador Blorpityblorpboop `{3}{G}{U}` Legendary Creature — Alien Advisor Guest 3/3: "When Ambassador Blorpityblorpboop enters, you get {TK}{TK}{TK}, then you may put a sticker on a nonland permanent you own.\nAt the beginning of each combat, you may have Ambassador Blorpityblorpboop's base power become equal to the total power of all stickers on permanents you control and its base toughness become equal to those stickers' total toughness."

- [ ] **Step 2: Write the failing tests.** `data/cards/ambassador-blorpityblorpboop.json`:

```json
{"faces": [{
  "manaCost": [{"type": "Generic", "value": 3}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Green"}}}, {"type": "OfType", "value": {"type": "Colored", "value": {"type": "Blue"}}}],
  "name": "Ambassador Blorpityblorpboop",
  "oracleText": "When Ambassador Blorpityblorpboop enters, you get {TK}{TK}{TK}, then you may put a sticker on a nonland permanent you own.\nAt the beginning of each combat, you may have Ambassador Blorpityblorpboop's base power become equal to the total power of all stickers on permanents you control and its base toughness become equal to those stickers' total toughness.",
  "power": {"type": "Literal", "value": 3},
  "toughness": {"type": "Literal", "value": 3},
  "triggeredAbilities": [
    {
      "condition": {"type": "SelfEnters"},
      "modal": {"modes": [{"clauses": [
        {"effects": [{"type": "GainPlayerCounters", "value": {"kind": {"type": "Ticket"}, "player": {"type": "Relative", "value": {"type": "You"}}, "quantity": {"type": "Literal", "value": 3}}}]},
        {"effects": [{"type": "PutSticker", "value": {"kinds": [{"type": "Name"}, {"type": "Ability"}, {"type": "PowerToughness"}, {"type": "Art"}], "player": {"type": "Relative", "value": {"type": "You"}}, "ref": {"type": "ChosenPermanent", "value": {"filter": {"type": "And", "value": [{"type": "Not", "value": {"type": "HasCardType", "value": {"type": "Land"}}}, {"type": "OwnedBy", "value": {"type": "You"}}]}}}}}], "optionality": {"type": "Optional"}}
      ]}]}
    },
    {
      "condition": {"type": "StepBegins", "value": {"phase": {"type": "Combat", "value": {"type": "BeginningOfCombat"}}, "scope": {"type": "EachTurn"}}},
      "modal": {"modes": [{"clauses": [{"effects": [{"type": "ModifyTarget", "value": {
        "duration": {"type": "Indefinite"},
        "modification": {"type": "SetBasePowerToughness", "value": {
          "power": {"type": "Count", "value": {"aggregation": {"type": "Total", "value": {"type": "PowerOfStickers"}}, "filter": {"type": "ControlledBy", "value": {"type": "You"}}, "scope": {"type": "InZone", "value": {"player": {"type": "Relative", "value": {"type": "AnyPlayer"}}, "zone": {"type": "Battlefield"}}}}},
          "toughness": {"type": "Count", "value": {"aggregation": {"type": "Total", "value": {"type": "ToughnessOfStickers"}}, "filter": {"type": "ControlledBy", "value": {"type": "You"}}, "scope": {"type": "InZone", "value": {"player": {"type": "Relative", "value": {"type": "AnyPlayer"}}, "zone": {"type": "Battlefield"}}}}}
        }},
        "ref": {"type": "InSlot", "value": "self"}
      }}], "optionality": {"type": "Optional"}}]}]}
    }
  ],
  "typeLine": {"subtypes": [{"type": "Alien"}, {"type": "Advisor"}, {"type": "Guest"}], "supertypes": [{"type": "Legendary"}], "types": [{"type": "Creature"}]}
}]}
```

In `Pawl.StickerSpec` (imports `qualified Pawl.Types.CombatStep as CombatStep`, `qualified Pawl.Types.GameEvent as GameEvent`, `qualified Pawl.Types.StepBegan as StepBegan`):

```haskell
  -- Review Focus 5. Ambassador's own trigger puts Otter's 5/1 on the Bears;
  -- Hot Dog Minotaur's 1/4 goes on Bonesplitter, a noncreature whose P/T CR
  -- 208.3 blanks, and Night's 2-ticket menace on the Bears. 5+1 and 1+4: the
  -- menace sticker's cost is no part of it, and Bonesplitter's sticker is.
  Spec.it s "CR 123.8a Ambassador Blorpityblorpboop becomes a 6/5 from the P/T stickers on alice's permanents" $ do
    sheets <- committedSheets
    ambassador <- S.printingOf s registry "Ambassador Blorpityblorpboop"
    bears <- S.printingOf s registry "Grizzly Bears"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (splitterId, g2) = S.addPermanent bonesplitter S.alice g1
        (ambassadorId, entered) = S.entersWithTrigger ambassador S.alice g2
        placed = snd (fst (State.runState (Engine.runGame (placingRef 0 (Just bearsId) otterFiveOne) entered drain) (MkOffers [] [] 0)))
        stickered = Sticker.put S.alice bearsId nightMenace Nothing (Sticker.put S.alice splitterId minotaurOneFour Nothing placed)
        atCombat = (S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.BeginningOfCombat) S.alice)] stickered) {GameState.phase = Phase.Combat CombatStep.BeginningOfCombat}
        combat = snd (fst (State.runState (Engine.runGame (placing Nothing) atCombat drain) (MkOffers [] [] 0)))
    Spec.assertEqWith s "CR 123.8a Ambassador is a 6/5" (Projection.powerOf ambassadorId combat, Projection.toughnessOf ambassadorId combat) (Just 6, Just 5)
    Spec.assertEqWith s "CR 123.3c alice paid two of its three tickets" (S.playerCounterOf PlayerCounterKind.Ticket S.alice combat) 1
```

`QuantitySpec`: `Quantity.PowerOfStickers` against `" {\"type\":\"PowerOfStickers\"} "` and `Quantity.ToughnessOfStickers` against `" {\"type\":\"ToughnessOfStickers\"} "`.

- [ ] **Step 3: Run; expected failure** "Not in scope: data constructor 'Quantity.PowerOfStickers'".

- [ ] **Step 4: Implement.**
  - `Types/Quantity.hs`, after `NameStickers`:

    ```haskell
      | -- | CR 123.8a: the printed power on the P/T stickers on the object this is
        -- evaluated against, summed (Ambassador Blorpityblorpboop).
        PowerOfStickers
      | -- | CR 123.8a: the printed toughness on them, summed.
        ToughnessOfStickers
    ```

    Codec: two `Arm.nullary` arms and their `tagOf` arms.
  - `Engine/Filter.hs`'s `View`, after `nameStickers`:

    ```haskell
        -- | CR 123.8a: the printed power and toughness on each P/T sticker on the
        -- candidate, in placement order. Off the object, stickerKinds' posture.
        stickerPowerToughness :: Seq.Seq (Integer, Integer),
    ```

    Filled in every `MkView` builder: `Filter`'s player view and `View.viewOfCard` with `Seq.empty`; `Count.viewOfSnapshot` with `Seq.empty` under its existing "(#4890)" comment, extended to "kinds, words and P/T"; `View.viewOfCharacteristics` with

    ```haskell
          Filter.stickerPowerToughness = foldMap (Seq.fromList . Maybe.mapMaybe (\p -> fmap (\pt -> (PowerToughnessSticker.power pt, PowerToughnessSticker.toughness pt)) (Game.powerToughnessStickerOf (StickerPlacement.sticker p) gs)) . Foldable.toList . Object.stickers) (Game.lookupObject oid gs),
    ```

    and the test views in `FilterSpec` (both) and `Support` with `Seq.empty`.
  - `Engine/Quantity.hs`, beside `NameStickers`:

    ```haskell
            Quantity.PowerOfStickers -> fmap (sum . fmap fst . Filter.stickerPowerToughness) mView
            Quantity.ToughnessOfStickers -> fmap (sum . fmap snd . Filter.stickerPowerToughness) mView
    ```

    Every other exhaustive case over `Quantity` (`Engine.Quantity`'s slot and `readsX` tables, `QuantitySlot`'s four, `Projection.quantityReads`, `Mentions`) answers as `NameStickers` does.

- [ ] **Step 5: Run** `-p Sticker`, `-p Quantity`, `-p Filter`, `-p Count`, `-p Card`, then the full suite. Expected: green.

- [ ] **Step 6: Mutate.**
  - `m7a.sed` on `Engine/Quantity.hs`: `s/Quantity.PowerOfStickers -> fmap (sum . fmap fst . Filter.stickerPowerToughness) mView/Quantity.PowerOfStickers -> fmap (const 0 . Filter.stickerPowerToughness) mView/`. Pattern `Ambassador`. Expected red: "CR 123.8a Ambassador is a 6/5".
  - `m7b.sed` on `Projection/View.hs`: `s/Filter.stickerPowerToughness = \(.*\),$/Filter.stickerPowerToughness = Seq.take 0 (\1),/`. Pattern `Ambassador`. Expected red: "CR 123.8a Ambassador is a 6/5".

- [ ] **Step 7: Commit.** "Read a P/T sticker's printed numbers, and add Ambassador Blorpityblorpboop (related to #872)".

---

## Task 8: file the unit-3 issues

**Files:** none but the citations: replace `#N1`–`#N3` in `Projection.hs`, `Types/Modification.hs` and `StickerSpec.hs`.

- [ ] **Step 1: Write the bodies** to `$SCRATCH/issue-N.md`, each opening with the dated summary, then `gh issue create --title ... --label ... --body-file $SCRATCH/issue-N.md`.

N1, title "Eldrazi Guacamole Tightrope's sheet can't be added: no self-cast-from-graveyard with an extra cost", labels `gap,expires:card-driven,area:cards`:

```markdown
> **Summary (2026-10-09).** The sticker sheet Eldrazi Guacamole Tightrope has an ability sticker reading "You may cast this card from your graveyard by paying 2 life in addition to paying its other costs". Pawl's permission for a card to cast itself from a graveyard carries no extra cost, and a player permission that does only works from a permanent. Eldrazi Guacamole Tightrope is the sheet to add; it is also the sheet whose ability sticker matters off the battlefield.

### Details

- CR 113.6f, CR 123.7, CR 601.2f. `Pawl.Types.CastingPermission.CastFromGraveyard` has no payload; `Pawl.Types.CastFromZone.additionalCosts` rides `PlayerEffect.CastFrom`, gathered from permanents.
- Its other stickers (haste, 1/4, 5/3) are expressible now.
```

N2, title "A static ability on an ability sticker doesn't apply to the stickered object", labels `gap,expires:card-driven,area:cards`:

```markdown
> **Summary (2026-10-09).** Some ability stickers print a static ability: Trained Blessed Mind's "Threshold — As long as seven or more cards are in your graveyard, this creature gets +4/+0 and has trample", Vampire Champion Fury's hellbent, Mystic Doom Sandwich's "must be blocked if able". Pawl grants a sticker's keywords, activated and triggered abilities, but a static or rule ability granted to the object itself joins no list, and nor does a keyword rule 702 states as a static ability. Trained Blessed Mind is the sheet to add.

### Details

- CR 113.3d, CR 123.7, CR 613.1f. `Pawl.Engine.Projection.stickerGathered` cites this issue; `Pawl.Engine.Projection.View.grantedStaticAbilitiesOf` reads stored grants only.
- `Pawl.StickerSpec`'s "no committed ability sticker carries a static or rule ability" keeps such a sheet out until this lands; delete it then.
```

N3, title "Clandestine Chameleon can't have the abilities of stickers on other permanents and graveyard cards", labels `gap,expires:card-driven,area:effects`:

```markdown
> **Summary (2026-10-09).** Clandestine Chameleon has all abilities of ability stickers on other permanents you own and cards in your graveyard. Pawl's grant of a sticker's abilities reads only the stickers on the granting object itself (Pin Collection). Clandestine Chameleon is the card.

### Details

- CR 123.7a. `Pawl.Types.Modification.GainAbilitiesOfStickers` cites this issue.
- Its "you get {TK}{TK}, then you may put a sticker on a nonland permanent you own" is expressible now.
```

- [ ] **Step 2: Cite them.** Replace `#N1`–`#N3` with the filed numbers; `grep -rn '#N[0-9]' source data docs` must return only this plan.
- [ ] **Step 3: Commit.** "Cite the stickers unit-3 follow-ups (related to #872)".

---

## Task 9: ingest, sweeps, self-review and the PR

- [ ] **Step 1: Re-run `pawl ingest`:**
  - `jq -f script/ingest/candidates.jq /Volumes/nvme/Developer/pawl/_scratch/AtomicCards.json > "$SCRATCH/candidates.json"`
  - `script/with-build-lock.sh cabal run -v0 pawl -- ingest "$SCRATCH/candidates.json" > "$SCRATCH/ingest.log" 2>&1`
  - Read the log. Every disagreement it names among this unit's five cards and the Juggler sheet is fixed here; one on any other card is filed or folded per CLAUDE.md. The four unit-1 sheets' stamped texts must not change (`git diff data/sticker-sheets`). Delete any new keyword-only card file it wrote (`git status --porcelain data/cards | grep '^??'`), out of scope, and say how many in the PR. `script/format-json.sh fix` every changed file, then the full suite.
- [ ] **Step 2: Sweeps.**
  - `grep -rn '872' source docs data --include='*.hs' --include='*.md' --include='*.json'`: unit 1's meld, merge and restamp elisions and `Keyword.hs`' sticker kicker only.
  - `grep -rn -i 'sticker\|ticket' source --include='*.hs'` and read every comment this unit touched; counting absolutes ("the one producer", "art stickers carry no data", "Game.stickerWords answers Nothing for every other kind") are now false: fix them.
  - `grep -rn '{ *PutSticker\.\|PutSticker\.[a-z]* = ' source --include='*.hs'` for a record update that keeps an old `ticketCap` or `free`; `Rewrite.rewriteEffect` is the one rebuilding it.
  - CR citations: for each rule in `git diff origin/main -U0 | grep -o 'CR [0-9][0-9.a-z/-]*' | sort -u`, read it in `docs/rules.txt`.
- [ ] **Step 3: Self-review.** `git fetch`, `git merge origin/main` (once, now), the full suite, then re-run m2b, m3a, m4a, m5a, m6, m7a. Re-read every comment the diff touched against the one-line haddock rule. Read `gh api repos/tfausak/pawl/issues/872/dependencies/blocking` and name what this unblocked.
- [ ] **Step 4: Open the PR.** `gh pr create --draft --title "Stickers unit 3: ability and P/T stickers, ticket costs" --body-file $SCRATCH/pr.md`, the body:

```
- What and why: unit 3 of the stickers spec, related to #872 (open until unit 4). Ability stickers in layer 6 and P/T stickers in layer 7b, in every public zone, at each placement's timestamp; CR 123.3c's ticket payment with a cap and waiver; CR 123.7a's and 123.8a's reads of a sticker's printed data. Cards: Lineprancers, Pin Collection, Tusk and Whiskers, Ambassador Blorpityblorpboop, Shadowspear; sheet: Unsanctioned Ancient Juggler. Commits this plan.
- CR: 107.3m, 107.17a, 113.3, 123.1, 123.3, 123.3b-c, 123.5, 123.7, 123.7a, 123.8, 123.8a, 208.3, 208.3a, 509.1c, 603.2, 608.2d, 611.2c, 613.1f, 613.4b-c, 613.7, 613.7k, 702.12b, 707.2.
- Design calls: the thirteen Divergences in docs/superpowers/plans/2026-10-09-stickers-unit-3.md, chiefly Tusk and Whiskers and Ambassador landing now, PutSticker's ticketCap and free, CR 107.3m's X reaching an enters trigger's effects, GainAbilitiesOfStickers reading the source's stickers, and a P/T sticker's holder as an affected filter.
- Verified: suite <before> -> <after>. Mutations: <m1 ... m7b, each with the assertion it reddened, named>. Regression fences: the Clone case, stickerWrites, the two-sticker order (the fold's), the waiver beyond m5c.
- Sites read and right as they stand: happenedBetween (the cap's evaluation writes nothing; payment and placement are both inside place, so neither happens without the other), Rewrite.rewriteEffect (rewrites ticketCap), Interchangeable.Mentions (putStickerNames reads ticketCap; GainAbilitiesOfStickers, PowerOfStickers, ToughnessOfStickers name nothing), eventBindings' fallthrough (PlacesSticker now binds became), overBoundSlots/boundSlots/renameBound (no new slot field), the four MkView builders and three test views, the positional MkPutSticker sites (CardSpec x2, EffectSpec) and patterns (Slots x2, Resolve x2, CardSpec, EffectLintSpec, Mentions, Rewrite), the Arm.tagged round trips (GainAbilitiesOfStickers, PowerOfStickers, ToughnessOfStickers, PutSticker's new fields), copyStampOf and copiableCharacteristics (untouched; stickers are read off Object.stickers, never the card).
- Does the rules core case on an effect's identity? No: it cases on StickerKind (CR 123.1) and CR 113.3's ability kind; stickerGrants hands a sticker's abilities over unread.
- Deferred: #N1-#N3; meld, merge, sticker kicker and the other generic producers (unit 4). <ingest: N keyword-only files not kept>.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
```

Leave it a draft if a drain loop dispatched you; otherwise mark it ready per CLAUDE.md. Report and stop.
