# Stickers unit 4: generic producers, sticker kicker, meld and merge — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land unit 4 of the stickers spec and close #872. It covers:
- "put a sticker" of any kind, on a permanent or a graveyard card (Ticketomaton, Scampire);
- CR 702.33h's sticker kicker (Wicker Picker), which counts as kicked for a generic reader (Hallar, the Firefletcher) and not for a printed kicker's linked "if kicked" (Faerie Squadron);
- CR 123.5a–c and 613.7k for melded and merged permanents.

**Architecture:**
- **Generic placement.** It already works for all four kinds; Ticketomaton is data. Scampire needs `effectIsImpossible`'s `PutSticker` arm to see a `ChosenCardInGraveyard` candidate set.
- **One placement step.** `Pawl.Engine.Sticker.place` becomes the one per-object placement step, with `namePosition` moved in beside it. Both the resolution arm and the cast hook call it.
- **Sticker kicker.**
  - `Keyword.StickerKicker (Cost Keyword)` is an optional additional cost (`optionalCost`, limit 1) whose family is `KeywordFamily.Kicker`, per CR 702.33h's "means Kicker".
  - `Quantity.WasKicked` narrows to the printed `Kicker`/`Multikicker` constructors (CR 702.33e's link).
  - A new `Filter.Kicked` is CR 702.33d's "kicked" for any kicker-family cost.
  - `Cast` pays it off just before `GameEvent.SpellCast`: the caster gets {TK}, then may put a sticker on the spell if they own it.
- **Meld.** The melded permanent takes every melded card's stickers in timestamp order.
- **Merge.** The merged permanent takes the spell's stickers beside the host's and restamps them all.
- **Split.** `changeZoneAttaching`'s split asks the departing permanent's owner which component keeps its stickers, before placement, so `Game.restampStickers` runs right after that object's stamp.

**Tech Stack:** Haskell (GHC via the repo's nix flake), cabal, tasty.
**Spec:** docs/superpowers/specs/2026-10-09-stickers-design.md
**Units 1–3:** docs/superpowers/plans/2026-10-09-stickers-unit-{1,2,3}.md. Their Divergences hold.

## Global Constraints

- **Worktree.**
  - Work only in `/Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-4`: branch `872-stickers-unit-4`, cut from origin/main 942e9f674. Never `cd` to the primary checkout.
  - Before the first build, run this as a bare command: `/Volumes/nvme/Developer/pawl/script/warm-worktree.sh seed /Volumes/nvme/Developer/pawl/.claude/worktrees/872-stickers-4`. Then confirm `cabal.project.local` is there.
- **Builds and tests.**
  - `$SCRATCH` is the session scratchpad. Prefix every `cabal` call with `script/with-build-lock.sh`, redirect its output to `$SCRATCH/<name>.log`, and read the file. Never pipe `cabal`.
  - Build: `script/with-build-lock.sh cabal build -v0 all > "$SCRATCH/build.log" 2>&1`.
  - Subtree: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes -p PATTERN' > "$SCRATCH/t.log" 2>&1`, with a one-word PATTERN.
  - Full suite: `script/with-build-lock.sh cabal test --test-options '--timeout 120s --hide-successes' > "$SCRATCH/suite.log" 2>&1`. Run it before each commit, and record the count before Task 1.
- **Mutations.**
  - Write each one as `$SCRATCH/mN.sed`, then run `script/with-build-lock.sh script/mutate.sh FILE @$SCRATCH/mN.sed PATTERN`. Run them one at a time, alone in the checkout. A mutation must leave no binding unused (`-Werror`).
  - Name the assertion that reddened, and confirm it is the one this plan names. A green, or a different red, is a finding: diagnose it before moving on.
  - Re-anchor the sed text if ormolu reflowed the line.
- **The rules core never cases on an effect's identity.**
  - Allowed: casing on `StickerKind` (CR 123.1), on keyword constructors (rule 702), and on `KeywordFamily`.
  - `Effect.PutSticker` is cased only in `Pawl.Engine.Resolve*` and the exhaustive classification tables.
- **Types and haddocks.** One type per `Pawl.Types.<TypeName>` module, holding the type and its instances only. Constructors take `Mk`. A haddock is one line plus its CR citation, except an elision paragraph or a note naming the proving test.
- **Elisions.** Each gets an issue, and a code-site comment saying only what is not implemented, ending `(#N)`. `#N1`–`#N2` are placeholders that Task 7 replaces.
- **Sources.**
  - Read CR text from `docs/rules.txt` by rule number.
  - Before writing a card, re-verify its Oracle text with `curl -s 'https://api.scryfall.com/cards/named?exact=<Name>'`, URL-encoded, one request at a time. The texts below are Scryfall's of 2026-10-10.
- **Committing.**
  - Stage, run `hooky fix`, then stage again.
  - Card JSON: run `script/format-json.sh fix FILE`. A card file is named by `Pawl.Slug.fromText` of its name.
  - Never stash; copy a file aside and move it back instead. Never `git checkout <file>` to revert.
  - Use extensions only from `.hlint.yaml`. Prefer `case` over `maybe` and `let` over `where`; no backticks in new code.
  - A commit message is a subject, at most two sentences of why, then `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
  - No commit message may say close, fix or resolve beside #872. Only the PR body says `Closes #872`.
- **Tests and answerers.**
  - Gameplay tests are Haskell specs in `Pawl.StickerSpec`, with test-local answerers (unit 1's Divergence 12) and its existing helpers: `committedSheets`, `withSheets`, `withJuggler`, `placingRef`, `aliceSticker`, `mainPhaseForAlice`, `drain`, and the named sticker constants.
  - The test code below is written against those names. Fix name or type slips; stop and report a rules or design error.
  - A case may place a sticker directly with `Sticker.put` when the placing card is not what it tests (unit 2's Divergence 10).

## Review Focus

These are the five input classes most likely to bite. Each has a test in its owning task.

1. **Sticker kicker turns on generic "kicked" but not a printed kicker's link.**
   - Faerie Squadron sticker-kicked alone enters as a 1/1 without flying.
   - Faerie Squadron kicked with its own kicker enters with counters and flying.
   - Hallar triggers on a sticker-kicked Grizzly Bears.
   - Tests (Task 3): "CR 702.33e sticker kicker does not satisfy Faerie Squadron's if kicked", "CR 702.33h/603.2 Hallar triggers on a sticker-kicked creature spell".
2. **A spell the caster does not own.** The caster gets {TK} and is never offered a sticker (CR 123.3b). Test (Task 3): "CR 123.3b a sticker-kicked Hostage Taker card alice doesn't own gives her a ticket and takes no sticker".
3. **The sticker is on the spell before CR 601.2i.** `GameEvent.SpellCast`'s snapshot shows the P/T sticker's power, and the permanent keeps the sticker (CR 123.5). Test (Task 3): "CR 702.33h the sticker is on the spell when it becomes cast, and on the Bears it becomes".
4. **Timestamp order through meld and merge.**
   - Two P/T stickers on two melded cards resolve by their old order.
   - A host's sticker restamps after an earlier layer-7b effect when a spell merges onto it.
   - Tests: Task 4, "CR 123.5a Chittering Host takes both cards' P/T stickers, the later one winning, either way round"; Task 5, "CR 613.7k a host's P/T sticker restamps at the merge and beats Turn to Frog".
5. **The split asks, and only when there is a choice.**
   - The owner chooses which card keeps the stickers; the answer flips the outcome.
   - A move to a hidden zone, or a split with one public object, asks nothing.
   - Tests (Task 6): "CR 123.5c Chittering Host's owner chooses which card keeps its stickers in the graveyard, either way", "CR 123.5 a melded permanent bounced to hand asks nothing and keeps no sticker".

## Divergences from the spec (decided here; the PR body repeats them)

1. **Sticker kicker's family is `KeywordFamily.Kicker`, not a constructor of its own.**
   - CR 702.33h says sticker kicker "means Kicker [cost]". This follows `Multikicker`'s precedent (CR 702.33c's "a multikicker cost is a kicker cost"): a "spell with kicker" reader then matches it.
   - The linked-ability split moves to `Quantity.WasKicked`, which now reads only the printed `Kicker`/`Multikicker` constructors (CR 702.33e, 607; Scryfall's Wicker Picker ruling). The spec's "matching KeywordFamily constructor" would have left no family for a "spell with kicker" reader to match.
2. **`Filter.Kicked` and Hallar, the Firefletcher are added.** No pool card reads "kicked" generically, so without them CR 702.33h's "means Kicker" would go unproven.
3. **The 123.5c owner chooses before placement, among the components.** The destination is settled first, so the answer is equivalent to choosing among the objects. `Game.restampStickers` then runs right after the chosen object's own stamp, as CR 613.7k requires; choosing after placement would stamp the stickers after every later arrival.
4. **Across a host and a merging spell, stickers keep their old timestamp order.** CR 613.7k fixes the order within one object only. The cross-object order is filed as a rules question (#N1).
5. **Two Wicker Pickers offer one sticker kicker.** This is #3635 ("two identical optional costs on one spell are paid as one"). Wicker Picker's ruling is added there as a comment; no new issue.
6. **Solaflora, Intergalactic Icon is filed, not folded** (#N2). Its Auras-and-Equipment half is a capability of its own.

---

## Task 1: Ticketomaton, "a sticker" of any kind

**Files:**
- Create: `data/cards/ticketomaton.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:** consumes `Effect.PutSticker` with all four kinds, as Tusk and Whiskers' activated ability already is.

- [ ] **Step 1: Verify Oracle.** Ticketomaton `{3}` Artifact Creature — Robot 2/2: "When Ticketomaton enters, you get {TK}, then you may put a sticker on a nonland permanent you own."
- [ ] **Step 2: Write the card.** Copy Lineprancers' trigger shape, with these changes:
  - `quantity` 1;
  - `kinds` = all four: `Name`, `Ability`, `PowerToughness`, `Art`;
  - the filter is Proficient Pyrodancer's: `Not (HasCardType Land) ∧ OwnedBy You`.
- [ ] **Step 3: Write the failing test.**

```haskell
  -- Ticketomaton's one ticket: every name and art sticker, and each ability or
  -- P/T sticker costing at most 1, across the four kinds.
  Spec.it s "CR 123.3 Ticketomaton offers stickers of every kind its ticket pays for" $ do
    sheets <- committedSheets
    robot <- S.printingOf s registry "Ticketomaton"
    -- cast it (or S.addPermanent + its enters trigger) with an answerer that
    -- records the ChooseSticker candidates and picks the first.
    ...
    Spec.assertEqWith s "CR 123.3 the offer spans all four kinds" (Set.fromList (fmap StickerRef.kind offered)) (Set.fromList [StickerKind.Name, StickerKind.Ability, StickerKind.PowerToughness, StickerKind.Art])
    Spec.assertEqWith s "CR 123.3c and nothing costing more than one ticket" (all (\r -> Sticker.ticketCost r board <= 1) offered) True
```

  If no committed sheet has an ability or P/T sticker costing 1 or less, give alice the tickets beforehand and adjust the second assertion's bound. It must stay a bound the offer can violate.
- [ ] **Step 4: Run** `-p Ticketomaton`. Expect it to fail only because the card is missing, then pass once the file exists.
- [ ] **Step 5: Mutate.** In `m1.sed` on `ticketomaton.json`, drop `{"type": "Name"},` from `kinds`. Expected red: "CR 123.3 the offer spans all four kinds". The mutation proves the test, not new engine code; say so in the PR.
- [ ] **Step 6: Commit.** "Add Ticketomaton: a sticker of any kind (related to #872)".

---

## Task 2: Scampire, a sticker on a graveyard card

**Files:**
- Create: `data/cards/scampire.json`
- Modify: `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`, the `effectIsImpossible` `PutSticker` arm (around line 3510) and, if Step 4 shows it, the resolution arm (around line 5700)
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

- [ ] **Step 1: Verify Oracle.** Scampire `{2}{B}` Creature — Vampire Employee 1/3:
  - "When Scampire enters, you get {TK}, then you may put a sticker on a creature card in your graveyard."
  - "{3}{B}: Return target creature card with a sticker on it from your graveyard to the battlefield. That creature gains haste. Exile it at the beginning of the next end step. Activate only as a sorcery."
- [ ] **Step 2: Write the card.**
  - **Trigger:** Ticketomaton's, with the `ref` changed to `ChosenCardInGraveyard {chooser: the controller, players: your graveyard, filter: HasCardType Creature, count: 1}`. Read `Pawl.Types.ChosenCardInGraveyard` and an existing card that uses it for the exact wire shape.
  - **Activated ability:** grep `data/cards` for `Exile it at the beginning of the next end step` beside `graveyard to the battlefield`, read the hits, and copy the shape of one that returns a target card that way (`dregscape-zombie.json` matched the grep but may be unearth). Use target filter `HasCardType Creature ∧ Stickered`, sorcery timing, and cost `{3}{B}`.
- [ ] **Step 3: Write the failing tests.**

```haskell
  Spec.it s "CR 123.3/123.5 Scampire stickers a creature card in alice's graveyard, and returns only that card" $ do
    -- alice: Scampire in hand, Grizzly Bears and Hill Giant cards in her graveyard,
    -- swamps. Cast Scampire; answer ChooseCardInGraveyard with the Bears and
    -- ChooseSticker with an art sticker. Then activate {3}{B}.
    ...
    Spec.assertEqWith s "CR 123.3 the Bears card in the graveyard has the art sticker" (stickerKindsOn bearsCard board) [StickerKind.Art]
    Spec.assertEqWith s "CR 123.4 only the stickered Bears is a legal target" targets [bearsCard]
    Spec.assertEqWith s "CR 123.5 the returned Bears keeps its sticker and has haste" (stickerKindsOn bearsPerm returned, Projection.hasKeyword Keyword.Haste bearsPerm returned) ([StickerKind.Art], True)
  Spec.it s "CR 608.2d Scampire with no creature card in alice's graveyard asks nothing" $ do
    -- same board, empty graveyard: the trigger's optional clause offers no may.
    Spec.assertEqWith s "CR 608.2d alice was not asked" askedPrompts []
```

  Here `stickerKindsOn oid gs = maybe [] (Foldable.toList . fmap (StickerRef.kind . StickerPlacement.sticker) . Object.stickers) (Game.lookupObject oid gs)`; add it if no such helper exists.
- [ ] **Step 4: Run** `-p Scampire`. Expected first failure: "CR 123.3 the Bears card in the graveyard has the art sticker".
  - The likely cause is `effectIsImpossible`'s arm: its `_ -> objectRefObjects` answers nothing for a chosen ref, so the optional clause is skipped as impossible.
  - Read which red you actually get before changing anything.
- [ ] **Step 5: Implement.** In `effectIsImpossible`'s `PutSticker` arm, give `ChosenCardInGraveyard` the candidate set its resolution offers, as `ChosenPermanent` already has: the cards in the named graveyards matching the filter. If the resolution arm's `_ -> objectRefObjects` does not prompt for a `ChosenCardInGraveyard`, route it through the function the other `ChosenCardInGraveyard` opcodes resolve through. Find it by grepping `ChosenCardInGraveyard` in `Resolve/Effect.hs`.
- [ ] **Step 6: Run** `-p Sticker` and `-p Scampire`, then the full suite.
- [ ] **Step 7: Mutate.** In `m2.sed`, revert the new `ChosenCardInGraveyard` candidate arm to `[]`. Expected red: "CR 123.3 the Bears card in the graveyard has the art sticker".
- [ ] **Step 8: Commit.** "Add Scampire: a sticker on a graveyard card (related to #872)".

---

## Task 3: sticker kicker, Wicker Picker and Hallar

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Keyword.hs`, which gets `StickerKicker (Cost.Cost Keyword)` after `Multikicker`. Delete the Kicker haddock's "Not implemented: CR 702.33h's sticker kicker (#872)" paragraph.
- Modify: `source/libraries/codec/Pawl/Codec/Keyword.hs` (arm and tag) and `KeywordSpec.hs` (a round trip).
- Modify: `source/libraries/engine/Pawl/Engine/Keyword.hs`:
  - `optionalCost`: `Keyword.StickerKicker cost -> Just (cost, Just 1)`;
  - `familyOf`: `-> Just KeywordFamily.Kicker`, with a comment citing CR 702.33h's "means Kicker";
  - every exhaustive `Keyword.Kicker _ -> []` sibling (lines around 442, 672, 1227, 1841, 2619, 4442, 4828, 5178) gets the same arm.
- Modify: `Pawl.Engine.Filter`'s `rewriteKeyword` (`rewriteCost`), `Interchangeable/Mentions.hs` (`costNames`) and `registry/Pawl/Oracle.hs`.
- Modify: `source/libraries/types/Pawl/Types/Filter.hs`, which gets `Kicked`. Plumb it through every `Filter.TagWasSpent` sibling site in `Engine/Filter.hs` (eval, and the traversals at around 2480, 3240, 3422, 3627, 3761 and 3941), plus `Codec/Filter.hs`, `FilterSpec.hs`, Mentions and Rewrite.
- Modify: `source/libraries/engine/Pawl/Engine/Quantity.hs`, where `WasKicked` reads only the printed constructors.
- Modify: `source/libraries/engine/Pawl/Engine/Sticker.hs`, which gets `place` and the moved `namePosition`. `Resolve/Effect.hs`'s `PutSticker` arm calls `Sticker.place`.
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, which gets `ChooseStickerOrNone`. Also its `Response`, `Replay.hs`, `scenario/Pawl/Scenario/Prompt.hs`, `Scenario.hs` and the prompt codec (grep `ChooseSticker` and mirror every hit).
- Modify: `source/libraries/engine/Pawl/Engine/Cast.hs`, the hook before `GameEvent.SpellCast`.
- Create: `data/cards/wicker-picker.json`, `data/cards/hallar-the-firefletcher.json`
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

**Interfaces:**
- Produces:
  - `Keyword.StickerKicker`;
  - `Filter.Kicked`;
  - `Sticker.place :: Bool -> PlayerId -> ObjectId -> Set StickerKind -> Maybe Natural -> Bool -> Game (Maybe StickerRef)`. The first argument is `optional`: when it is set the placer may decline (`ChooseStickerOrNone`), otherwise `chooseAmong`/`ChooseSticker` as now. It returns the placed sticker.
- Consumes: `Sticker.offered`, `Sticker.payTickets`, `Sticker.put`, and the `GainPlayerCounters` resolution's player-counter writer.

- [ ] **Step 1: Verify Oracle.**
  - Wicker Picker `{3}` Artifact Creature — Scarecrow Guest 2/3: "Creature spells you cast have sticker kicker {1}. (You may pay an additional {1} as you cast a creature spell. If you do, you get {TK}, then you may put a sticker on it.)"
  - Hallar, the Firefletcher `{1}{R}{G}` Legendary Creature — Elf Archer 3/3: "Trample\nWhenever you cast a spell, if that spell was kicked, put a +1/+1 counter on Hallar, then Hallar deals damage equal to the number of +1/+1 counters on it to each opponent."
  - Read Wicker Picker's rulings (`rulings_uri`).
- [ ] **Step 2: Write the cards.**
  - **Wicker Picker:** copy Chief Engineer's static ability (`MatchingOffBattlefield` over `WasCastFrom` each zone ∧ `ControlledBy You`), with `HasCardType Creature` and `GainKeyword {"type": "StickerKicker", "value": {"mana": [{"type": "Generic", "value": 1}]}}`.
  - **Hallar:** `keywords: [Trample]`, plus a `SpellCast` trigger whose spell filter is `Kicked` and whose caster is you. Its effects: `PutCounters` +1/+1 on self, then damage equal to `ObjectCounters` +1/+1 on self to each opponent. Grep a pool card for each effect's shape.
- [ ] **Step 3: Write the failing tests.**

```haskell
  Spec.it s "CR 702.33h sticker kicker gives alice a ticket and puts a sticker on the spell" $ do
    -- alice: Wicker Picker, Forests; Grizzly Bears in hand. Cast it, answer
    -- ChooseKicker StickerKicker with 1 and ChooseStickerOrNone with an art sticker.
    Spec.assertEqWith s "CR 702.33h alice got one ticket and the Bears spell has the art sticker" (S.playerCounterOf PlayerCounterKind.Ticket S.alice cast, stickerKindsOn spell cast) (1, [StickerKind.Art])
  Spec.it s "CR 702.33h the sticker is on the spell when it becomes cast, and on the Bears it becomes" $ do
    -- same, choosing a P/T sticker (alice's ticket pays it; give her more if the
    -- cheapest costs more). Read the GameEvent.SpellCast snapshot's power off the log.
    Spec.assertEqWith s "CR 601.2i the cast snapshot shows the sticker's power" snapshotPower stickerPower
    Spec.assertEqWith s "CR 123.5 the Bears on the battlefield keeps it" (Projection.power bears resolved) stickerPower
  Spec.it s "CR 702.33d declining sticker kicker gives no ticket and asks no sticker" $ do
    Spec.assertEqWith s "no ticket, no ChooseStickerOrNone" (tickets, askedSticker) (0, False)
  Spec.it s "CR 702.33e sticker kicker does not satisfy Faerie Squadron's if kicked" $ do
    -- sticker-kick Faerie Squadron, decline its own kicker.
    Spec.assertEqWith s "CR 702.33e the Squadron enters with no counters and no flying" (S.counterOf CounterKind.PlusOnePlusOne sq g, Projection.hasKeyword Keyword.Flying sq g) (0, False)
    -- and with its own kicker paid (sticker kicker declined): two counters, flying.
    Spec.assertEqWith s "CR 702.33d its own kicker still does" (S.counterOf CounterKind.PlusOnePlusOne sq2 g2, Projection.hasKeyword Keyword.Flying sq2 g2) (2, True)
  Spec.it s "CR 702.33h/603.2 Hallar triggers on a sticker-kicked creature spell" $ do
    -- Wicker Picker + Hallar; sticker-kick Grizzly Bears; drain.
    Spec.assertEqWith s "CR 702.33h Hallar has a +1/+1 counter and bob took 1" (S.counterOf CounterKind.PlusOnePlusOne hallar g, S.lifeOf S.bob g) (1, 19)
    -- unkicked: no trigger.
    Spec.assertEqWith s "an unkicked Bears doesn't trigger Hallar" (S.counterOf CounterKind.PlusOnePlusOne hallar g0) 0
  Spec.it s "CR 123.3b a sticker-kicked Hostage Taker card alice doesn't own gives her a ticket and takes no sticker" $ do
    -- alice's Hostage Taker exiles bob's Grizzly Bears; alice casts it with
    -- Wicker Picker's sticker kicker.
    Spec.assertEqWith s "CR 123.3b alice got the ticket, was not asked, and the Bears has no sticker" (tickets, askedSticker, stickerKindsOn bears g) (1, False, [])
```

  Start the bob figure in the Hallar test from the life total the harness starts at; 20 − 1 is assumed.
- [ ] **Step 4: Run** `-p Sticker`. Expected first failure: the Wicker Picker card fails to parse until `StickerKicker`'s codec exists. Then "CR 702.33h alice got one ticket and the Bears spell has the art sticker".
- [ ] **Step 5: Implement.**
  - **Keyword and family.** Add the constructor and every arm the compiler forces. Then grep `Keyword.Multikicker` and `Keyword.Type.Multikicker` (CLAUDE.md's alias rule) and add a `StickerKicker` arm beside each non-exhaustive hit that should treat it alike. Record every hit read in the PR.
  - **`WasKicked`:**

    ```haskell
    -- CR 702.33e: "if kicked" is linked to the kicker printed on the object, which
    -- a sticker kicker (CR 702.33h) is not, though it is in the family.
    Quantity.WasKicked -> fmap (\view -> if any (> 0) (Map.filterWithKey (\keyword _ -> Keyword.isPrintedKicker keyword) (Filter.paidCosts view)) then 1 else 0) mView
    ```

    Add `Keyword.isPrintedKicker` in `Pawl.Engine.Keyword` as a two-arm case (`Kicker`, `Multikicker`) with a wildcard, `optionalCost`'s posture.
  - **`Filter.Kicked`:** `Filter.Kicked -> any (\(k, n) -> n > 0 && Keyword.familyOf k == Just KeywordFamily.Kicker) (Map.toList (paidCosts view))`. Its haddock: "CR 702.33d: the spell was kicked, by any kicker cost, a sticker kicker's among them (CR 702.33h)."
  - **`Sticker.place`:** move the per-object body of `Resolve/Effect.hs`'s `PutSticker` arm (the `place picked` block and the `chooseAmong`) and `namePosition` into `Pawl.Engine.Sticker`. Its imports (`Projection`, `NameWords`, `Prompt`) must not cycle; if one does, say so and keep `namePosition` in Resolve, passing it in as an argument. `bindStickerSlot` stays in Resolve, applied to `place`'s result.
  - **`Cast` hook,** after the `ridersUsed` store and before the `SpellCast` event:

    ```haskell
    -- CR 702.33h: for each sticker kicker paid, the caster gets {TK}, then may
    -- put a sticker on the spell, which CR 123.3b allows only on a spell they own.
    -- Before CR 601.2i's event, so a cast trigger sees it (Wicker Picker's ruling).
    Monad.forM_ [k | (k@Keyword.StickerKicker {}, n) <- Map.toList paid, _ <- [1 .. n]] $ \_ -> do
      State.modify' (gainTickets pid 1)
      Monad.void (Sticker.place True pid sid allKinds Nothing False)
    ```

    `paid` is the announced map `stampPaidCosts` wrote. `gainTickets` is whatever the `GainPlayerCounters` resolution arm (`Resolve/Effect.hs` around line 9949) writes through. Factor it out if it is inline, so ticket replacements and `counterSharers` apply alike. `allKinds` is the four `StickerKind`s. `Sticker.place` already refuses a placer who does not own the object. The comprehension cases on a keyword constructor, which is rule 702, not an effect.
  - **`ChooseStickerOrNone`:** `Decider -> PlayerId -> ObjectId -> NonEmpty StickerRef -> Prompt (Maybe StickerRef)`. Mirror `ChooseMovedCounterOrNone` across `Replay`'s three arms (its default answer is `Nothing`), Scenario's two, and `Scenario.hs`'s validity check, which accepts `Nothing` or a member. An empty offer asks nothing; that is CR 608.2d's impossible option, not an elision.
- [ ] **Step 6: Run** `-p Sticker`, `-p Kicker`, `-p Keyword`, `-p Card`, then the full suite. A red outside these groups means a widened site: read it, and list it in the PR.
- [ ] **Step 7: Mutate.** Run each alone:
  - m3a, in `Cast.hs`: replace the `Sticker.place` line with `pure ()`. Expected red: "CR 702.33h alice got one ticket and the Bears spell has the art sticker".
  - m3b, in `Quantity.hs`: `Keyword.isPrintedKicker keyword` → `Keyword.familyOf keyword == Just KeywordFamily.Kicker`. Expected red: "CR 702.33e the Squadron enters with no counters and no flying".
  - m3c, in `Filter.hs`'s `Kicked` eval: the familyOf test → `Keyword.isPrintedKicker k`. Expected red: "CR 702.33h Hallar has a +1/+1 counter and bob took 1".
  - m3d, in `Sticker.place`: drop its owner gate. Expected red: "CR 123.3b alice got the ticket, was not asked, and the Bears has no sticker".
  - m3e: move the hook after the `SpellCast` event. Expected red: "CR 601.2i the cast snapshot shows the sticker's power".
- [ ] **Step 8: Comment on #3635.** One line: Wicker Picker's ruling, "If you control multiple Wicker Pickers, then you can pay {1} for each of them", is a producer for this issue. Cite #3635 in the `Cast` hook's comment as `(gap #3635)`.
- [ ] **Step 9: Commit.** "Add sticker kicker, Wicker Picker and Hallar, the Firefletcher (related to #872)".

---

## Task 4: stickers on a melded permanent (CR 123.5a)

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`, the meld placement (`Object.stickers = Seq.empty` with the `(#872)` comment, around line 8018)
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`. Reuse `Pawl.MeldSpec`'s Graf Rats board helpers; import them, or copy the minimum.

- [ ] **Step 1: Write the failing test.**

```haskell
  -- Graf Rats takes P/T sticker A, then Midnight Scavengers takes B (Sticker.put,
  -- so B is later); combat begins and they meld. Then the same with B on the Rats
  -- first. Use two P/T stickers with different numbers.
  Spec.it s "CR 123.5a Chittering Host takes both cards' P/T stickers, the later one winning, either way round" $ do
    Spec.assertEqWith s "CR 123.5a Chittering Host has both stickers" (length (stickerKindsOn host melded)) 2
    Spec.assertEqWith s "CR 613.7k the later sticker sets its P/T" (Projection.power host melded, Projection.toughness host melded) bNumbers
    Spec.assertEqWith s "and the other way round" (Projection.power host' melded', Projection.toughness host' melded') aNumbers
```

- [ ] **Step 2: Run** `-p Chittering`. Expected red: "CR 123.5a Chittering Host has both stickers" (0 against 2).
- [ ] **Step 3: Implement.** Before `forgetObject`, read each melding object's stickers off the board. Concatenate them and order them with `Seq.sortOn StickerPlacement.timestamp`. Set `Object.stickers` to that. Then run `State.modify' (Game.restampStickers newId)` right after the `placeObject` that mints `newId`. Replace the elision comment with: `-- CR 123.5a: every melded card's stickers, in their timestamp order; CR 613.7k restamps them below.`
- [ ] **Step 4: Run** `-p Sticker`, `-p Meld`, then the full suite.
- [ ] **Step 5: Mutate.** m4a: the gathered stickers → `Seq.empty`. Expected red: "CR 123.5a Chittering Host has both stickers". m4b: drop the `sortOn` (keep concatenation order Rats-then-Scavengers). Expected red: "and the other way round".
- [ ] **Step 6: Commit.** "Keep melded cards' stickers on the melded permanent (related to #872)".

---

## Task 5: stickers on a merged permanent (CR 123.5b, 613.7k)

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`, in `merge` (the haddock's `(#872)` paragraph, around line 8205)
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

- [ ] **Step 1: Write the failing tests.**

```haskell
  -- Wicker Picker; alice casts Cubwarden for its mutate cost onto her Grizzly
  -- Bears, sticker-kicking it with a P/T sticker.
  Spec.it s "CR 123.5b the mutating Cubwarden spell's P/T sticker is on the merged permanent" $ do
    Spec.assertEqWith s "CR 123.5b the merged Bears has the spell's sticker and its P/T" (stickerKindsOn bears merged, (Projection.power bears merged, Projection.toughness bears merged)) ([StickerKind.PowerToughness], stickerNumbers)
  -- The Bears takes a P/T sticker (Sticker.put); Turn to Frog makes it 1/1
  -- (later timestamp); then Cubwarden mutates onto it.
  Spec.it s "CR 613.7k a host's P/T sticker restamps at the merge and beats Turn to Frog" $ do
    Spec.assertEqWith s "before the merge, Turn to Frog's 1/1 wins" beforeMerge (1, 1)
    Spec.assertEqWith s "CR 613.7k after it, the restamped sticker's P/T wins" afterMerge stickerNumbers
```

- [ ] **Step 2: Run** `-p Sticker`. Expected red: "CR 123.5b the merged Bears has the spell's sticker and its P/T".
- [ ] **Step 3: Implement.** In `merge`, read the spell's stickers before the spell object goes. Write `Object.stickers target = Seq.sortOn StickerPlacement.timestamp (hostStickers <> spellStickers)`, then `Game.restampStickers target`. Replace the haddock's elision paragraph with:
  - "CR 123.5b / 613.7k: the spell's stickers join the host's, all restamped at the merge, in their old timestamp order across the two objects, which the rule does not fix (#N1)."
- [ ] **Step 4: Run** `-p Sticker`, `-p Mutate`, then the full suite.
- [ ] **Step 5: Mutate.** m5a: drop `spellStickers`. Expected red: "CR 123.5b the merged Bears has the spell's sticker and its P/T". m5b: drop the `restampStickers target` call. Expected red: "CR 613.7k after it, the restamped sticker's P/T wins".
- [ ] **Step 6: Commit.** "Merge a mutating spell's stickers into the merged permanent and restamp them (related to #872)".

---

## Task 6: the split — the owner chooses who keeps the stickers (CR 123.5c)

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`, in `changeZoneAttaching`'s split (`asComponent` and the `(#872)` comment, around lines 6435–6465)
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, which gets `ChooseStickerKeeper`. Give its candidates the same type `arrangeComponents`' prompt uses, and mirror its plumbing in Response, Replay, Scenario and codec.
- Modify: `source/libraries/test/Pawl/StickerSpec.hs`

- [ ] **Step 1: Write the failing tests.**

```haskell
  -- Chittering Host with an art sticker (Sticker.put on the host); it dies
  -- (Murder). The owner's answerer picks Graf Rats, then Midnight Scavengers.
  Spec.it s "CR 123.5c Chittering Host's owner chooses which card keeps its stickers in the graveyard, either way" $ do
    Spec.assertEqWith s "CR 123.5c alice was asked, and Graf Rats keeps the sticker" (askedKeeper, stickerKindsOn rats g, stickerKindsOn scav g) (True, [StickerKind.Art], [])
    Spec.assertEqWith s "and Midnight Scavengers when she picks it" (stickerKindsOn rats' g', stickerKindsOn scav' g') ([], [StickerKind.Art])
  Spec.it s "CR 123.5 a melded permanent bounced to hand asks nothing and keeps no sticker" $ do
    Spec.assertEqWith s "CR 123.5 not asked, and no card in hand has a sticker" (askedKeeper, concatMap (\c -> stickerKindsOn c g) hand) (False, [])
  -- The Task 5 board's merged Bears dies: alice picks the Cubwarden card.
  Spec.it s "CR 123.5c a mutated permanent's owner chooses which card keeps its stickers" $ do
    Spec.assertEqWith s "CR 123.5c the Cubwarden card keeps the P/T sticker, the Bears card has none" (stickerKindsOn cub g, stickerKindsOn bearsCard g) ([StickerKind.PowerToughness], [])
```

- [ ] **Step 2: Run** `-p Sticker`. Expected red: "CR 123.5c alice was asked, and Graf Rats keeps the sticker".
- [ ] **Step 3: Implement.**
  - After `arrangeComponents` and before the first `placeObject`, collect the public candidates: the `destComponents` if `dest` is not a hidden zone (`Game.isHiddenZone`), plus the `commandComponents`.
  - If the departing object has stickers and there are two or more candidates, ask `Object.owner obj` with `ChooseStickerKeeper`. With exactly one, that one keeps them; that is not an elision, since there is nothing to choose. With none, nothing does.
  - `asComponent` keeps `Object.stickers obj` on the chosen component and empties it on every other. Each `placeObject` whose object kept the stickers is followed by `Game.restampStickers` on its id, as `newId`'s already is.
  - Replace the elision comment with: "CR 123.5c: the owner chooses, among the components reaching a public zone, the one that keeps the stickers; asked before placement so CR 613.7k's restamp follows that object's own stamp."
- [ ] **Step 4: Run** `-p Sticker`, `-p Meld`, `-p Mutate`, `-p Commander`, then the full suite.
- [ ] **Step 5: Mutate.** m6a: ignore the answer and always keep the first candidate. Expected red: "and Midnight Scavengers when she picks it". m6b: drop the `isHiddenZone` test, so a hand-bound split counts its cards as candidates. Expected red: "CR 123.5 not asked, and no card in hand has a sticker".
- [ ] **Step 6: Commit.** "Ask the owner which split object keeps a melded or merged permanent's stickers (related to #872)".

---

## Task 7: file the unit-4 issues

- [ ] **Step 1: File.** Write each body to `$SCRATCH/issue-N.md`, opening with the dated summary, then `gh issue create --title ... --label ... --body-file ...`.

  N1, title "Which order do a host's and a merging spell's stickers take after the merge?". Label it `question`, as #4887 is, with no area label:

  ```markdown
  > **Summary (2026-10-10).** When a spell with stickers merges onto a permanent with stickers, every sticker gets a new timestamp. The rules keep each object's own stickers in order but don't say how the two objects' stickers interleave. Pawl keeps their old timestamp order across both. Two P/T stickers, one on Cubwarden cast with Wicker Picker's sticker kicker and one on the creature it mutates onto, would show the difference.

  ### Details

  - CR 123.5b, CR 613.7k. `Pawl.Engine.Event.merge` cites this issue.
  ```

  N2, title "Solaflora, Intergalactic Icon: its counters, stickers and attachments affect other creatures", labels `gap,expires:card-driven,area:cards`:

  ```markdown
  > **Summary (2026-10-10).** Solaflora's Auras and Equipment, and its counters and stickers, affect other creatures you control as though they were on them. Pawl has no way for an attachment, counter or sticker to apply to an object it is not on. Solaflora is the card.

  ### Details

  - CR 123.7, CR 123.8, CR 301.5, CR 303.4. Out of the stickers spec's unit 4, as the spec says.
  ```

- [ ] **Step 2: Cite them.** Replace `#N1`. `grep -rn '#N[0-9]' source data docs` must return only this plan.
- [ ] **Step 3: Commit.** "Cite the stickers unit-4 follow-ups (related to #872)".

---

## Task 8: ingest, sweeps, self-review and the PR

- [ ] **Step 1: `pawl ingest`.**
  - Run `jq -f script/ingest/candidates.jq /Volumes/nvme/Developer/pawl/_scratch/AtomicCards.json > "$SCRATCH/candidates.json"`, then `script/with-build-lock.sh cabal run -v0 pawl -- ingest "$SCRATCH/candidates.json" > "$SCRATCH/ingest.log" 2>&1`.
  - Fix any disagreement on this unit's four cards.
  - Delete any new keyword-only file it writes, and say how many in the PR. Then run the full suite.
- [ ] **Step 2: Sweeps.**
  - Run `grep -rn '872' source docs data --include='*.hs' --include='*.md' --include='*.json'`. Every elision citing #872 must be gone; only the spec, plans and historical references remain. #872 closes with this PR (CLAUDE.md: grep the bare number).
  - Run `grep -rn -i 'sticker\|kicker\|kicked' source --include='*.hs'` and read every comment this unit touched. The rule-statements now false include "the one producer", "Kicker is the only…", and `WasKicked`'s "family" wording.
  - Check the record-update sites for `Object.stickers` (`{ Object.stickers =`, `o {Object.stickers`). For each, confirm it is intended. `happenedBetween`: a placement inside the cast hook is not an instruction, so there is no copy-across. Say so.
  - CR citations: read each `CR` the diff cites in `docs/rules.txt`.
- [ ] **Step 3: Self-review.** Run `git fetch` and `git merge origin/main` (once, now), then the full suite, and re-run m3a, m3b, m3c, m4a, m5a and m6a. Re-read every touched comment against the one-line haddock rule. Read `gh api repos/tfausak/pawl/issues/872/dependencies/blocking` and name in the PR body what this unblocks.
- [ ] **Step 4: Open the PR.** Run `gh pr create --draft --title "Stickers unit 4: generic producers, sticker kicker, meld and merge" --body-file $SCRATCH/pr.md`:

```
- What and why: the last unit of the stickers spec. Closes #872
  - "a sticker" of any kind, on a permanent or a graveyard card;
  - CR 702.33h's sticker kicker;
  - CR 123.5a-c's stickers through meld, merge and the split.
  - Cards: Ticketomaton, Scampire, Wicker Picker, Hallar, the Firefletcher.
  - Commits this plan.
- CR: 123.3, 123.3b-c, 123.4, 123.5, 123.5a-c, 601.2b, 601.2i, 603.2, 607, 608.2d, 613.7k, 702.33a, 702.33d-e, 702.33h, 712.4a, 730.
- Design calls: the six Divergences in docs/superpowers/plans/2026-10-10-stickers-unit-4.md, chiefly:
  - sticker kicker in the Kicker family, with WasKicked narrowed to the printed kicker;
  - Filter.Kicked and Hallar;
  - the 123.5c choice made among components before placement.
- Verified: suite <before> -> <after>. Mutations: <m1 ... m6b, each with the assertion it reddened, named>.
- Sites read and right as they stand: <every Multikicker/Kicker sibling hit, every TagWasSpent sibling, every ChooseSticker plumbing hit, the Object.stickers updates, happenedBetween>.
- Does the rules core case on an effect's identity? No. It cases on StickerKind (CR 123.1), keyword constructors (rule 702) and KeywordFamily.
- Deferred: #N1, #N2; #3635 gains Wicker Picker. <ingest: N keyword-only files not kept>.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
```

Leave the PR a draft. Report its URL and head SHA, suite counts, each mutation with the assertion it reddened, the issues filed, your departures from this plan, and anything unresolved. Then stop.
