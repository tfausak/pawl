# M2d castable black/green decks — design

Design for milestone **M2d**: make M2c's black and green keyword creatures
(Typhoid Rats, War Mammoth) **castable** by giving players real mono-color decks,
so a random game exercises the deathtouch state-based action (CR 704.5h) and
trample assignment (CR 702.19) — and their interaction (CR 702.2c) — through
actual casting and combat rather than through fixtures placed directly on the
battlefield. This discharges git-bug `14138aa`.

M2d adds **no new rules and zero opcodes.** The deathtouch SBA, trample
assignment, and the 702.2c threshold collapse were all built in M2c
(`docs/superpowers/specs/2026-07-17-m2c-deathtouch-trample-design.md`). M2d only
adds two basic-land types, two basic-land printings, a deck data model, a
per-player deck refactor of setup, and the tests that ride on them. It is a
setup-and-coverage milestone, not a rules milestone.

This is a types-and-architecture spec, not an implementation plan.

## Goal and scope

M2c introduced black (deathtouch, Typhoid Rats) and green (trample, War Mammoth)
cards, but the mana base was 36 Mountains per player, so those two cards were only
ever exercised as **fixtures** — placed directly on the battlefield by
`combatBoardOf`, never cast. Their combat behavior is proven by deterministic unit
tests, but nothing casts them, and no *random* game ever touches the 704.5h SBA or
trample assignment. M2d closes that gap.

**Exit criterion.** A random game can be played in which one player casts War
Mammoth and the other casts Typhoid Rats, they fight, and the deathtouch SBA and
trample overflow fire under random play — with conservation, termination, and the
"life never increases" property all still holding. A deterministic test casts each
card through the stack (not as a fixture), proving castability directly.

**Non-goals.** M2d does *not* remove M2c's synthetic 702.2c fixture (one creature
with both deathtouch and trample): no printed card has both as static abilities,
and putting deathtouch onto a trampler needs the granting M3 will build. M2d does
*not* introduce any mixed-color deck, any mana-source choice, or any new keyword,
card type, or opcode.

## The central decision: mono-color per player, so at most two colors

Making the black and green cards castable means real mana of those colors, which
means new basic lands. The constraint that keeps this cheap lives in
`Mana.payCost` (Mana.hs:148): it taps sources front-of-list until a cost is
covered, eliding *which* source to tap. That elision is legitimate **only while a
player's sources are indistinguishable.** A single deck holding both Swamps and
Forests breaks it — tapping a Forest to pay `{B}` is a real decision the engine
must not make (the engine-makes-no-choices invariant). So **each deck must be
mono-color**, and the elision survives because `payCost` works per player: every
source a given player controls still produces exactly one, identical mana type.

Two players, each mono-color, means **at most two colors** are cast in any one
game. The setup refactor is "per-player decks," and once decks are per-player the
property suite can run over **several matchups** rather than one golden game.

**Why green-black, added alongside red-red.** The CR 702.2c interaction — a
deathtouch blocker collapsing a trampler's lethal threshold to 1 — only appears
when deathtouch and trample face each other. A red-green game (trampler blocked
only by red creatures) never collapses the threshold; a red-black game has no
trampler at all. So isolating each color against red would give the 702.2c seam,
the subtlest thing M2c built, **zero** random coverage. A single **green-black**
game covers all three axes at once: the deathtouch SBA, trample assignment, and
their interaction (War Mammoth attacks, Typhoid Rats blocks → threshold collapses
to 1, the excess spills to the defending player per 702.19b). Keeping the existing
**red-red** game retains all of red's M2a/M2b keyword coverage (flying, first
strike, and the rest). The matchup set is therefore **{ red-red, green-black }** —
minimal proliferation, maximal interaction coverage.

alice plays green (tramplers), bob plays black (deathtouchers).

## 1. Subtypes and mana

`Pawl.Type.Subtype` gains `Swamp` and `Forest` (enum growth, no shape change, as
M2a's and M2b's subtypes were). `Pawl.Mana.subtypeMana` — the CR 305.6
classification that grants a basic land its intrinsic mana ability from its
subtype — gains `Swamp -> Just (Colored Black)` and `Forest -> Just (Colored
Green)`. `Color` already has all five colors, so no enum growth there.

The single-mana-type elision in `Mana.tapForMana` (Mana.hs:92) is untouched: a
Swamp produces exactly one black unit and a Forest exactly one green unit, so
there is still no choice of *which* type a tapped source makes. Only a dual land
would expire that, and M2d adds none.

## 2. Deck as a multiset

A deck is order-independent — it is shuffled before play, so any ordering among its
cards is meaningless. The honest model is a **multiset**, not a list:

- New `newtype Deck = MkDeck (Map Printing Natural)` in `Pawl.Type.Deck` (one type
  per module; a newtype over the naked `Map`, giving it a name and preventing
  accidental mixing with other maps). `Printing` and every type beneath it already
  derive `Ord`, so `Map Printing Natural` needs no new instances.
- `Setup.deckSize :: Deck -> Natural` = the sum of the map's values (replacing
  today's `deckSize :: Int` constant, which was the length of a 60-element list).

This replaces `Setup.deckList :: [Printing]`, whose 60 entries included 36
identical Mountain values. Composition assertions become a `Map.lookup` instead of
a `countByName` fold.

### The three decks

Every deck is **36 land + 24 creature = 60**, so `Game.objectCount` stays 120 in
any matchup and M1b's conservation property is untouched:

| Deck (in `Pawl.Setup`) | Lands | Creatures |
|---|---|---|
| `redDeck` (was `deckList`) | 36 Mountain | 16 Goblin Piker + 8 Bird Maiden |
| `greenDeck` (new) | 36 Forest | 24 War Mammoth |
| `blackDeck` (new) | 36 Swamp | 24 Typhoid Rats |

The green and black decks are mono-creature because War Mammoth and Typhoid Rats
are the only vanilla-plus-trample and vanilla-plus-deathtouch printings the corpus
holds (M2c §6); that is enough for reliable casting and combat, which is all the
mana base has ever aimed for. War Mammoth is `{3}{G}` and castable off 36 Forests;
Typhoid Rats is `{B}` and castable off 36 Swamps.

## 3. The explicit matchup and the setup refactor

A matchup is an explicit list of `(player, deck)` pairs — no privileged default
deck:

- `Setup.newGame :: NonEmpty (PlayerId, Deck) -> Game ()`. The pair list carries
  both the turn order (its `fst` projection) and each player's deck. For each
  `(printing, n)` in a player's deck it performs `replicateM n` of the existing
  `createCard`, then shuffles and draws the opening hand.
- `Engine.playFrom :: NonEmpty (PlayerId, Deck) -> Game Result` — unchanged body
  (`newGame` then `playGame`), new signature.
- `Setup.mirror :: Deck -> NonEmpty PlayerId -> NonEmpty (PlayerId, Deck)` — pairs
  every player with one deck, for the symmetric red-red case.

`Setup.emptyGame` still takes `NonEmpty PlayerId` (it builds no cards); callers
pass the matchup's `fst` projection. The callers that were implicitly red-red get
a one-word wrap:

- the benchmark's three game runners and the two full-game integration tests call
  `Engine.playFrom (Setup.mirror Setup.redDeck players)`;
- the random-game property runner takes the matchup as a parameter (§5).

`createCard`, `shuffleLibrary`, and `drawCard` are unchanged.

## 4. The basic-land printings

`Pawl.Card` gains `swampPrinting` and `forestPrinting`, each shaped exactly like
`mountainPrinting`: a `Basic Land — <Subtype>` with `manaCost = Nothing`, no
power/toughness, no keywords, and no stored mana ability (it is granted from the
subtype by CR 305.6, so the engine derives it — the card never names it). Basic
lands are trivially faithful to Scryfall and carry no rulings.

No new creature printings: `warMammothPrinting` and `typhoidRatsPrinting` already
exist from M2c and were Scryfall-verified there. M2d changes only where they live
(a deck, not just a fixture).

## 5. Testing approach

**`deckTests` retuned.** Composition is now a `Map.lookup`: each of `redDeck`,
`greenDeck`, `blackDeck` sums to 60 via `deckSize`; `redDeck` has 36 Mountain / 16
Piker / 8 Maiden, `greenDeck` 36 Forest / 24 War Mammoth, `blackDeck` 36 Swamp /
24 Typhoid Rats. A post-setup count per matchup confirms the cards reach the
library and hand (green-black puts 36 Forest across alice's library+hand and 36
Swamp across bob's), keeping the existing `countByName` check meaningful.

**`propertyTests` run over both matchups.** `runRandomGame` gains a matchup
parameter; each property is asserted across `{ redRed, greenBlack }` via
`QC.conjoin . map …` (no list comprehension, per house style). All surviving
properties still hold in green-black:

- **conservation** — 120 objects (36+24 per 60-card deck, two players);
- **termination** — every game ends with a result;
- **ids** — at least 120 minted;
- **no floating mana** at end;
- **life never increases** — green-black has no lifelink; trample spill (702.19b)
  and unblocked hits only *lower* a life total. (This property still dies at
  lifelink, a later milestone, not here.)
- **combat happens** — some seed changes a life total.

**New: castability, asserted directly.** A deterministic test casts Typhoid Rats
off a Swamp and War Mammoth off Forests **through the stack** (mana → cast →
resolve), and confirms each resolves onto the battlefield. Fixtures never proved
castability; this is the behavior M2d newly enables, so it is asserted on its own,
not left implicit in the random games.

**New: a green-black engagement guard.** Analogous to the existing "some seed
changes a life total": across green-black seeds, at least one game must send a
creature to the graveyard. This is cheap insurance that combat and the SBA
actually engage under random play, so the green-black matchup cannot silently
no-op while the suite stays green. It asserts that combat *occurs*, not that
deathtouch *specifically* fired — the precise deathtouch and trample assertions
remain M2c's deterministic fixtures, which M2d leaves in place.

The benchmark stays red-red (unchanged).

## 6. What M2d preserves

- **The `payCost` source elision (Mana.hs:148)** survives because every deck is
  mono-color: a player's sources remain indistinguishable. A mixed-color deck
  would expire it and force a real mana-source prompt — deliberately out of scope.
- **The `tapForMana` single-type elision (Mana.hs:92)** survives: Swamp and Forest
  each produce exactly one type. Only a dual land would expire it.
- **M2c's synthetic 702.2c fixture** stays: the falsifier needs both keywords on
  one creature, which needs M3's granting, independent of castability.
- **Mono-red's expiry** (M2c §6) is honored: black and green cards now truly enter
  the game, and each stays in its own mono-color deck, so no incidental color
  complexity leaks into a milestone.

## 7. Expiries this milestone opens

- **The mono-color-per-player deck constraint** expires the first time a player
  needs two colors of mana — a real mana-base or multicolor milestone — at which
  point `Mana.payCost`'s source elision becomes a `Prompt` for which source to
  tap. M2d records the constraint; it does not build the prompt.
- **The green/black mono-creature decks** are a floor, not a ceiling: they hold one
  creature type each only because the vanilla corpus offers one per keyword. They
  grow naturally as more black/green vanilla-ish cards land.

## 8. Explicitly deferred past M2d

- **Mixed-color decks and the mana-source-choice prompt** — the scope jump the
  constraint above names. Not M2d.
- **A green-black benchmark** — the benchmark stays red-red; adding a second
  matchup there is optional and unrequested.
- **Casting the M2a/M2b red keyword cards' colors elsewhere, three-color coverage,
  or additional matchups** (red-green, red-black) — the {red-red, green-black} set
  already covers every M2c axis; more matchups are cost without new coverage.
- **Everything M2c already deferred** (lifelink and the rest of the punchlist, the
  layer-6 grant that replaces the synthetic fixture, planeswalker trample) — M2d
  changes none of it.
