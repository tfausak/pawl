# Copying flip cards: implementation plan

**Goal:** Make a copy of a flip card retain both the copied card's normal and
alternative characteristics without inheriting the source's flipped status, so
that a new Clone starts normal and can later flip. Close #3364 and #3366 as one
rule.

**Rules decision:** The current CR does not explicitly say whether a flip card's
latent alternative characteristics accompany a copy. The official Champions of
Kamigawa FAQ does:

> If you copy a flipped permanent, you get the normal, unflipped version. That
> copy may flip later if certain conditions are met.

Source: Wizards, “Flipping Out,” 2004-09-20,
<https://magic.wizards.com/en/news/making-magic/flipping-out-2004-09-20>.

This does not conflict with the current rules. CR 707.2 does not copy flipped
status; CR 110.5 gives every permanent a flipped/unflipped status; CR 710.2
applies the alternative characteristics once the permanent is flipped; and CR
710 has no counterpart to CR 712.9's explicit requirement that a transforming
permanent be represented by a double-faced card or token.

**Architecture:** Keep `Object.flipped` as the copy's own uncopied status. Carry
the normal layer-1 reading and an optional flipped reading in one new
`Pawl.Types.CopySnapshot` value. This makes explicit the pair merged permanents
already carry indirectly through two reserved bindings. Store that value in the
one `Binding.copy` slot, and use the same carrier in `TokenLot` and `LastKnown`;
the normal and alternative readings then cannot fall out of step in transit. Do
not put a non-characteristic or a recursive second record in
`ProjectedCharacteristics`. A copy uses the normal reading when its own status
is unflipped. `Projection.stampedSnapshotOf` switches to the optional reading
when the recipient's own `Object.flipped` is true. Thus a new Clone starts
normal, while an already-flipped permanent that becomes a copy keeps its own
status and immediately uses the copied alternative; neither inherits the source
permanent's status.

**Primary proving card:** Clone copying Akki Lavarunner // Tok-Tok, Volcano Born.
Both are already in `data/cards/`.

## Before editing

- Read `CLAUDE.md`, `CONTRIBUTING.md`, and `docs/agents/implementing.md`.
- Fetch `origin` and branch from `origin/main`, not the current checkout:
  `3366-copying-flip-cards` is an appropriate branch name.
- Re-check CR 110.5, 707.2-3, 707.9, 710.1-4, and 712.9 in
  `docs/rules.txt`.
- Re-check the quoted FAQ at the Wizards URL. Record it in the issue and PR as
  the authority resolving the omission in CR 710.
- Correct #3366 before implementation: remove `needs-planning`, add `gap` and
  `rules-correctness`, and comment with the ruling above. Do not add an expiry
  label; Clone and Akki are already in the pool.
- Copy `cabal.project.local` and seed the worktree exactly as
  `docs/agents/implementing.md` requires.
- Run the full suite once and record its count. Prefix every `cabal` invocation
  with `script/with-build-lock.sh`; redirect output to a file rather than piping
  it.

## Decisions already made

### Implement #3364 and #3366 together

They are the two halves of one copy operation:

1. The copied permanent's current flipped status is not copied, so the copy
   operation's normal snapshot is independent of that status (#3364). A new
   Clone is unflipped; a permanent already flipped before a `BecomeCopy` effect
   keeps its own status.
2. The copied flip card's alternative reading remains available, so the copy
   can later flip (#3366).

Fixing either alone leaves an impossible intermediate model. In particular,
merely forcing the source unflipped while taking the current snapshot would make
a Clone enter as Akki but permanently discard Tok-Tok.

### Do not add a field to `ProjectedCharacteristics`

The alternative reading is not one more characteristic. It is a complete
counterfactual set of characteristics selected by status, just as
`Binding.flippedCopyOf` already models for a merged permanent. Adding a
`flippedFace`, `layout`, or nested `ProjectedCharacteristics` field would make
every projection builder carry layout state and would duplicate
`Projection.stampedSnapshotOf`'s existing selection.

Put the two readings in a dedicated `CopySnapshot` type instead. One value is
the source of truth at every transport boundary; parallel fields on `TokenLot`
and `LastKnown`, or two general-purpose binding slots, admit a normal reading
from one copy event beside an alternative reading from another. There are no API
or saved-game compatibility obligations, so codec churn is not a reason to keep
that invalid state representable.

### Do not use the copy's printed card

The printed card beneath a Clone is still Clone. Flippability and the
alternative reading must come from its layer-1 copy stamp. This is the same
printed-versus-copiable distinction already enforced for Rooms and preparation
cards.

### Preserve face-down copying

CR 707.2 expressly copies values as modified by face-down status. Do not obtain
the optional flipped reading by blindly forcing every source face up: that
would leak the identity and alternative characteristics of a face-down object.
Only a face-up source whose copiable values carry flip alternatives contributes
the optional reading. Existing face-down copy tests in `Pawl.CopySpec` must stay
green.

### Copy exceptions apply to both readings

CR 707.9 makes an exception part of the resulting copiable values. Apply the
same exception list independently to the normal and alternative snapshots.
Otherwise Sakashima copying Akki would be named Sakashima while unflipped and
Tok-Tok after flipping.

---

## Task 1: Add the red gameplay tests

**Files:**

- Modify: `source/libraries/test/Pawl/FlipSpec.hs`

Use the existing `halfReadings`, `normalHalf`, `alternativeHalf`,
`costReadings`, and combat helpers. Copy only the small prompt and resolution
helpers needed from `Pawl.CopySpec`; do not create a cross-spec dependency.

### Case A: Clone of unflipped Akki can flip

- [ ] Build a combat-ready board with Akki under alice's control.
- [ ] Put Clone on the stack and resolve it, answering
  `Prompt.ChooseCopyTarget` with Akki's exact id.
- [ ] Find the resulting permanent by its printed Clone card, not by projected
  name.
- [ ] Assert before combat that every projected reader reports `normalHalf` and
  the object itself has `Object.flipped == False`.
- [ ] Move the original Akki off the battlefield so the attack does not create
  two Tok-Toks and invoke the legend rule.
- [ ] Run combat with the Clone attacking bob unblocked. Akki's copied haste
  makes it legal to attack on the turn Clone entered.
- [ ] Put the copied trigger on the stack and resolve it through the ordinary
  combat/priority path.
- [ ] Make the first post-combat assertion the gameplay result:
  `halfReadings cloneId after == alternativeHalf`.
- [ ] Then assert that the copied permanent's status is flipped and that
  `costReadings` remains mana value 4 and red.

Expected before implementation: **FAIL** at the post-combat alternative-half
assertion. The copied trigger resolves, but `Game.flipsOver` rejects the Clone
because its printed card has `Layout.Normal`.

Suggested case name:

`CR 707.2 / 710 a Clone of unflipped Akki later flips into Tok-Tok`

### Case B: Clone of flipped Tok-Tok starts as Akki and can later flip

- [ ] Use the same board shape.
- [ ] Put the source into flipped status before resolving Clone. Calling
  `Game.flipPermanent` here is acceptable: the existing Akki gameplay case
  already proves the source's own transition, while this case is about what the
  copy operation observes.
- [ ] Resolve Clone copying that flipped source.
- [ ] Before doing anything else, assert
  `halfReadings cloneId copied == normalHalf` and
  `Object.flipped == False`.
- [ ] Remove the source and run the copied Clone through the same unblocked
  combat.
- [ ] Assert first that it becomes `alternativeHalf`, then assert its status and
  unchanged mana value/colour.

Expected before implementation: **FAIL** at the pre-combat normal-half
assertion. `Event.copiedSnapshot` currently freezes the source's alternative
reading, contrary to CR 707.2.

Suggested case name:

`CR 707.2 a Clone of flipped Tok-Tok starts as Akki and may flip later`

### Case C: The latent alternative survives a copy chain

- [ ] Have a first Clone copy Akki.
- [ ] Have a second Clone copy the first Clone.
- [ ] Remove the original and first Clone, attack with the second Clone, and
  resolve its trigger.
- [ ] Assert the second Clone becomes `alternativeHalf`.

This is a regression fence for CR 707.2b/707.3's copy-of-a-copy path. It prevents
an implementation that derives the alternative only from the first source's
printed `Layout.Flip`.

Suggested case name:

`CR 707.3 a copy of a Clone of Akki retains Tok-Tok`

- [ ] Run only the Flip subtree and record the three expected failures before
  changing engine code.
- [ ] Commit the red tests.

---

## Task 2: Make the dual reading one value

**Files:**

- Add: `source/libraries/types/Pawl/Types/CopySnapshot.hs`
- Add: `source/libraries/codec/Pawl/Codec/CopySnapshot.hs`
- Add: `source/libraries/codec/Pawl/Codec/CopySnapshotSpec.hs`
- Modify: `source/libraries/types/Pawl/Types/Binding.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Binding.hs`
- Modify: `source/libraries/codec/Pawl/Codec/BindingSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Binding.hs`
- Modify: `source/libraries/test/Pawl/BindingSpec.hs`
- Modify: `source/libraries/test/Pawl/CardSpec.hs`
- Modify: `source/libraries/test/Pawl/Test.hs`
- Modify: `pawl.cabal`
- Modify comments referring to `setMergeCopy` or `flippedMergeSource` in:
  - `source/libraries/engine/Pawl/Engine/Event.hs`
  - `source/libraries/engine/Pawl/Engine/Game.hs`
  - `source/libraries/engine/Pawl/Engine/Projection/View.hs`
  - `source/libraries/types/Pawl/Types/Source.hs`

- [ ] Read `docs/adding-a-module.md`, then add:

  ```haskell
  data CopySnapshot = MkCopySnapshot
    { normal :: ProjectedCharacteristics,
      flipped :: Maybe ProjectedCharacteristics
    }
    deriving (Eq, Ord, Show)
  ```

  The type module contains only the type and instances.

- [ ] Add a codec with required `normal` and optional/defaulted `flipped`
  fields, following the surrounding codec style. Add a round-trip case whose
  `flipped` value is nonempty, wire the spec into `Pawl.Test`, stage
  `pawl.cabal`, and run `cabal-gild pawl.cabal` as the adding-a-module guide
  requires.
- [ ] Change `Pawl.Types.Binding.copy` from
  `Maybe ProjectedCharacteristics` to `Maybe CopySnapshot`. Update its codec and
  put a snapshot with both readings in the existing all-fields codec fixture.
- [ ] Remove the second reserved `flippedMergeSource` binding entirely. Update
  the reserved-slot census in `Pawl.CardSpec`; one `copySource` slot now owns
  the indivisible pair.
- [ ] In `Pawl.Engine.Binding`, expose:
  - `copySnapshotOf :: Map SlotName Binding -> Maybe CopySnapshot`;
  - `copyOf`, returning `CopySnapshot.normal` for callers that ask for the
    ordinary layer-1 reading;
  - `flippedCopyOf`, returning the nested optional reading;
  - `setCopySnapshot`, replacing the whole value under `copySource`;
  - `setCopy pc`, a convenience that writes `MkCopySnapshot pc Nothing`.
- [ ] Replace `setMergeCopy` with a wrapper that writes one `CopySnapshot`.
  It may omit the nested reading when the normal and flipped merge results are
  equal because `Game.flipsOver` can still see the merged permanent's printed
  flip component. A copied permanent must not infer the *existence* of an
  alternative from snapshot inequality; `Event.copiedSnapshot` will record
  `Just` whenever the copied object carries flip alternatives, even if the two
  values happen to compare equal.
- [ ] Extend `Pawl.BindingSpec` to prove:
  - normal-only storage round-trips;
  - both readings are available through their accessors;
  - `setCopy` replaces an old two-reading snapshot and therefore removes the
    old alternative.
- [ ] Update all prose that describes the old second reserved slot.

No gameplay behavior should change yet.

---

## Task 3: Produce normal and alternative copy readings

**Files:**

- Modify: `source/libraries/engine/Pawl/Engine/Projection/View.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Game.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`

### Counterfactual projection

- [ ] Add `Projection.copiableCharacteristicsUnflipped` beside
  `copiableCharacteristicsFaceUp` and `copiableCharacteristicsFlipped`.
- [ ] Implement it by changing only `Object.flipped` to `False` in a
  counterfactual board, then asking `copiableCharacteristics`.
- [ ] Do not change `Object.facing`: a face-down source must keep the face-down
  values CR 707.2 names.
- [ ] Keep `copiableCharacteristicsFlipped` as the counterfactual flipped,
  face-up reading. Update its merge-only comments because ordinary copied flip
  cards will now consume it too.

### Separate “has alternatives” from “may flip now”

- [ ] Extract the non-zone part of `Game.flipsOver` into a classifier such as
  `hasFlipCharacteristics`.
- [ ] It should answer true when either:
  - `Binding.flippedCopyOf` is present on the object; or
  - any card representing the object has a `Card.flippedFace`.
- [ ] Keep the battlefield conjunct in `flipsOver` itself. Spells and cards can
  carry latent flip alternatives for copying even though only a permanent on
  the battlefield can flip.
- [ ] Preserve `cardsOfWithLastKnown`'s merged-component behavior from #3429.
  Do not regress the case where Akki is below a topmost mutate component.

### One copy-reading producer

- [ ] Replace the single-value logic centered on `Event.copiedSnapshot` with one
  function returning the pair, for example:

  ```haskell
  copiedSnapshot :: ObjectId -> GameState -> CopySnapshot
  ```

- [ ] `CopySnapshot.normal` is
  `Projection.copiableCharacteristicsUnflipped`, with the existing CR 202.3b/c
  mana-value override retained.
- [ ] `CopySnapshot.flipped` is
  `Projection.copiableCharacteristicsFlipped` only when the source is face up
  and `Game.hasFlipCharacteristics` is true.
- [ ] Apply the existing mana-value adjustment helper to each reading. It is
  inert for Akki, but factoring it once prevents the pair from drifting.
- [ ] State the official Champions FAQ in the function comment, together with
  CR 707.2 and the two proving `Pawl.FlipSpec` cases. Do not claim that CR 710
  itself explicitly makes the alternative characteristics copiable.
- [ ] Do not expose a tuple and do not retain a second producer returning only
  the normal reading. Callers that deliberately need one reading can select
  `CopySnapshot.normal` from this value.

At this point `Game.flipPermanent` can recognize a manually stamped copied
alternative, but no ordinary copy path writes one yet.

---

## Task 4: Carry the pair through every copy path

The proving card uses the `AsCopy` entry path, but this is one CR 707 rule.
Enumerate all `Binding.setCopy`, `Event.copiedSnapshot`, and
`Event.copiedSnapshotWithLastKnown` callers before editing and account for each
one.

### Live copy writers

**Files:**

- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replacement.hs`

- [ ] In `Event`'s `EntryRewrite.AsCopy` arm, obtain both readings from the
  selected source, map `AsCopy.exceptions` over `CopySnapshot.normal` and
  `CopySnapshot.flipped`, and stamp the result with
  `Binding.setCopySnapshot`.
- [ ] In `Effect.BecomeCopy`, do the same for every subject.
- [ ] In `Effect.CopyStackObject`, stamp the complete `CopySnapshot` on a copied
  permanent spell. This is needed because the spell-copy-to-token transition
  must retain a flip card's latent alternative.
- [ ] When a permanent spell copy resolves into a token in
  `Event.changeZoneAttaching`, carry `Binding.copySnapshotOf` intact instead of
  rebuilding only `Binding.copyOf`.
- [ ] In `Replacement.applyEntryOption`, do not accidentally erase an existing
  alternative by calling the normal-only setter. Update
  `CopySnapshot.normal` while preserving `CopySnapshot.flipped`. No current card
  combines this entry choice with flip alternatives, so report this as an
  audited regression fence rather than as gameplay coverage.

### Token-copy event transport

**Files:**

- Modify: `source/libraries/types/Pawl/Types/TokenLot.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replacement.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Resolve/Effect.hs`

- [ ] Change `TokenLot.copy` from `Maybe ProjectedCharacteristics` to
  `Maybe CopySnapshot`.
- [ ] Grep every `TokenLot.MkTokenLot` construction. Effect-defined tokens have
  `Nothing`; token copies receive the complete snapshot.
- [ ] Widen `Event.createTokens` to accept and stamp a `Maybe CopySnapshot`.
- [ ] Keep `Replacement.matchesTokenLot` reading
  `CopySnapshot.normal`. Token creation replacements inspect the token as it
  will initially exist, and tokens enter unflipped.
- [ ] Check every `TokenLot` record update. Count scaling and appended lots must
  preserve the complete snapshot of existing lots.

### Last-known-information transport

**Files:**

- Modify: `source/libraries/types/Pawl/Types/LastKnown.hs`
- Modify: `source/libraries/codec/Pawl/Codec/LastKnown.hs`
- Modify: `source/libraries/codec/Pawl/Codec/LastKnownSpec.hs`
- Modify every `LastKnown.MkLastKnown` construction in:
  - `source/libraries/engine/Pawl/Engine/Departure.hs`
  - `source/libraries/engine/Pawl/Engine/Event.hs`
  - `source/libraries/engine/Pawl/Engine/Setup.hs`
  - `source/libraries/test/Pawl/DamageSpec.hs`

- [ ] Change `LastKnown.copiable` from a strict
  `ProjectedCharacteristics` to a strict `CopySnapshot`.
- [ ] Update the field's rationale: the normal and alternative snapshots are
  two readings of one layer-1 result selected by an uncopied status.
- [ ] Change the codec to use `Pawl.Codec.CopySnapshot` and put a nonempty
  alternative in at least one codec round-trip fixture.
- [ ] Grep every construction manually. Two `Event` sites currently construct
  `LastKnown` positionally. The type change will make a wrong value fail to
  compile, but convert those constructions to record syntax while touching them
  so the next field cannot be absorbed in argument order.
- [ ] File `Event.copiedSnapshot oid gs` at each live departure site.
- [ ] Change `copiedSnapshotWithLastKnown` to return `CopySnapshot`: live
  objects call `copiedSnapshot`; departed objects return
  `LastKnown.copiable`.
- [ ] Ensure `CreateCopy` and `BecomeCopy` consume that value intact, so a copy
  effect resolving after its source left does not lose the latent alternative.

---

## Task 5: Prove CR 707.9 and copy-of-copy behavior

**Files:**

- Modify: `source/libraries/test/Pawl/FlipSpec.hs`

- [ ] Make Case C from Task 1 green using only the generalized copy stamp. Do not
  recover Tok-Tok from the original printed Akki after the first copy.
- [ ] Add a Sakashima case if the implementation can silently apply exceptions
  only to the normal reading:
  - Sakashima the Impostor copies unflipped Akki.
  - Remove the original.
  - Attack with Sakashima and resolve the copied trigger.
  - Assert the flipped permanent has Tok-Tok's alternative P/T, subtypes, and
    protection, but is still named Sakashima and remains legendary.

Suggested case name:

`CR 707.9 Sakashima's copy exceptions survive flipping`

This is the gameplay observer for mapping copy exceptions over both snapshots.
Do not replace it with a direct `Replacement.applyCopyExceptions` unit test.

- [ ] Re-run the existing face-down copy subtree in `Pawl.CopySpec`.
- [ ] Re-run the existing merged-flip cases in `Pawl.MutateSpec`.
- [ ] Run the whole suite.

---

## Task 6: Remove the resolved elisions and self-review

**Files with current issue citations:**

- `source/libraries/engine/Pawl/Engine/Card.hs` — #3364
- `source/libraries/engine/Pawl/Engine/Event.hs` — #3364
- `source/libraries/engine/Pawl/Engine/Game.hs` — #3366
- `source/libraries/engine/Pawl/Engine/Projection/View.hs` — #3364
- `source/libraries/test/Pawl/FlipSpec.hs` — #3366

- [ ] Grep the bare numbers `3364` and `3366` across the tree.
- [ ] Remove every resolved “not implemented” paragraph. Replace only the
  comments that still explain a live invariant; do not leave historical issue
  citations.
- [ ] Update `FlipSpec`'s module comment to name the Clone cases as the proof
  that copied flip alternatives survive while status does not.
- [ ] Sweep for merge-only prose made false by generalizing
  `flippedCopyOf`, removing `flippedMergeSource`, or changing `setMergeCopy`.
- [ ] Re-read every comment touched in `Binding`, `Event`, `Game`,
  `Projection.View`, `CopySnapshot`, `TokenLot`, and `LastKnown`.
- [ ] Re-check every CR citation against `docs/rules.txt`.
- [ ] Confirm the rules core still cases only on the layout classification and
  never on Akki, Clone, or an effect's identity.

### Invisible-site audit

- [ ] `CopySnapshot`: grep every constructor and record update. Confirm every
  normal reading is paired deliberately with `Just` or `Nothing`, and every
  rewrite preserves the untouched half.
- [ ] `LastKnown`: grep every constructor and record update. Confirm all
  constructors file the complete snapshot; explicitly call out the converted
  positional sites in the PR.
- [ ] `TokenLot`: grep every constructor and record update. Confirm appended
  effect-defined lots say `Nothing` and count rewrites preserve existing
  alternatives.
- [ ] Widened result: grep every caller of `copiedSnapshot` and
  `copiedSnapshotWithLastKnown`; say in the PR which copy paths were converted:
  AsCopy, CreateCopy, BecomeCopy, spell copies, spell-copy resolution, and
  last-known copies.
- [ ] Reserved binding slot: remove `flippedMergeSource` from
  `Pawl.CardSpec`'s reserved-name census and update the Binding codec fixture for
  `CopySnapshot`.
- [ ] `ProjectedCharacteristics` builders need no edit because no field is being
  added there. State that this was checked rather than leaving it implicit.

---

## Mutation checks

Run each mutation separately through `script/mutate.sh` under
`script/with-build-lock.sh`. Read and record the first failing assertion; it
must be the named gameplay assertion, not a prompt count or intermediate proxy.

1. **Copied alternative omitted.** In the `AsCopy` writer, set
   `CopySnapshot.flipped` to `Nothing`. Expected red:
   `Clone of unflipped Akki ...` at the first assertion that the Clone became
   `alternativeHalf`.
2. **Source's flipped status copied.** Build the normal snapshot through the
   current-status projection instead of
   `copiableCharacteristicsUnflipped`. Expected red:
   `Clone of flipped Tok-Tok ...` at the pre-combat assertion that the Clone is
   `normalHalf`.
3. **Copy-of-copy alternative dropped.** Make the paired producer ignore
   `Binding.flippedCopyOf` while handling a copied source. Expected red:
   `copy of a Clone of Akki ...` at the assertion that the second Clone became
   `alternativeHalf`.
4. **`flipsOver` ignores copied alternatives.** Remove the
   `Binding.flippedCopyOf` disjunct from `Game.hasFlipCharacteristics`. Expected
   red: the direct Clone gameplay case remains `normalHalf` after its trigger
   resolves.
5. **Exceptions applied only to normal.** Skip
   `Replacement.applyCopyExceptions` over the optional reading. Expected red:
   the Sakashima gameplay case at the assertion that the flipped permanent is
   still named Sakashima.
6. **Last-known transport omitted.** Replace the filed snapshot with one whose
   `CopySnapshot.flipped` is `Nothing`. If no gameplay-level card in the pool can
   distinguish this and the relevant subtrees stay green, report that green
   mutation explicitly: the `CopySnapshot` codec case proves the value can be
   encoded, but last-known filing remains an audited regression fence rather
   than gameplay proof. Do not claim otherwise.

After restoring every mutation, re-run the Flip, Copy, and Mutate subtrees. If
`origin/main` is merged during the unit, re-run the load-bearing mutations.

---

## Finish

- [ ] Run the full suite with CI's timeout and record count before -> after.
- [ ] Stage the changed files, run `hooky fix`, and stage again.
- [ ] Review the staged diff, especially every scripted or formatter edit.
- [ ] Push the branch and open a draft PR.
- [ ] Self-review the branch, fix findings, push, and mark the PR ready once the
  full suite is green. Do not wait for CI.

The PR body is the required terse bullets:

- What changed and why, with bare `Closes #3364` and `Closes #3366`.
- CR 110.5, 707.2-3, 707.9, 710.1-4, 712.9, plus the official Champions FAQ as
  the authority for the omitted copy interaction.
- Design: dual normal/optional-alternative snapshots; rejected a
  `ProjectedCharacteristics` field and printed-card lookup.
- Verification: suite count before -> after; name the direct Clone proving
  case, the flipped-source case, copy-of-copy, and Sakashima; list every
  mutation and the exact assertion it reddened.
- Explicitly: the rules core does not case on an effect's identity.
- Deferred: only real, card-producing gaps discovered during implementation.
  Do not invent a follow-up for a hypothetical edge no card exercises.
