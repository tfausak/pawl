# M4c tokens — design

Design for milestone **M4c**, the third letter of M4 (see the split table in
`docs/design.md` §3): **tokens — the first *card-less* object.** M4c's job is to
break the standing assumption that every `Object` references a `Printing`, and to
establish that an object whose characteristics come from an *effect* (CR 111.3),
not a printing, flows through the whole engine — the projection, combat,
state-based actions, the event funnel — with no special case, plus the one rule
tokens add that cards do not have: a token that leaves the battlefield **ceases
to exist** (CR 704.5d).

The gate card and its foil are one card doing both jobs — Scryfall-verified
(`api.scryfall.com/cards/named`, fetched 2026-07-19):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Dragon Fodder** | `{1}{R}` | Sorcery | "Create two 1/1 red Goblin creature tokens." |

Dragon Fodder **falsifies its own naive implementation** on the one axis this
letter owns. A model that stored a token like a card — as an object that must
reference a printing, and that stays in the graveyard when it dies — does the
wrong thing twice: it has nothing to point the token's characteristics at (a
token has no printing, CR 111.6), and it leaves a dead Goblin sitting in the
graveyard forever (CR 704.5d says the token ceases to exist). The two Goblins
also prove the *positive* half — **characteristics come from the effect**: the
projection reads their 1/1 and their creature-ness off the embedded token `Card`,
so they attack, block, take combat damage, and die through the exact code paths a
printed creature uses. One card, both directions of the gate.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the ordered spine

M4c is a small milestone with one genuinely new idea. The `Source` type already
predicts it: its own comment reads *"Becomes a `data` once there is more than one
way to be an object's source (`OfToken`, `OfCopy`, `OfEmblem`, …)."* M4c cashes
the first of those. Everything else is the consequence of a card-less object
existing: a way to mint one from nothing, and a way to remove one when it leaves
the battlefield.

The load-bearing insight is that a token **is a `Card` with no `Printing`**. Rule
111.3 states the characteristics defined by the creating effect "are functionally
equivalent to the characteristic values that are printed on a card," and §2.8 of
`design.md` already draws the line the other way — `Printing = Card + metadata`,
and game objects reference the printing for its *metadata*, never for
characteristics the `Card` already carries. So a token's source carries a `Card`
directly, and `Game.cardOf` — the single chokepoint every characteristic read
funnels through — returns it. The entire projection / mana / quantity / combat /
SBA pipeline then consumes a token with no new plumbing.

**The ordered spine** (gate first, then the two consequences — the plan owns the
exact commit decomposition):

1. **The card-less object + the Create opcode (the gate).** `Source.OfToken
   Card`, the `OfToken` arm at every site that cases on a source, `Effect.Create
   Quantity Card`, and `Event.createToken`. Card: Dragon Fodder resolves; two
   Goblins exist on the battlefield and can fight. This is the whole "card-less
   object flows through the engine" claim.
2. **Cease-to-exist (CR 704.5d).** The new state-based action in
   `Sba.performStateBasedActions`: a token whose zone is not the battlefield is
   removed from the game entirely. This is what makes a dead token vanish.
3. **The Rest in Peace interaction (M3f composes for free).** No new code — a
   test. A dying token funnels through `changeZone` like everything else, so Rest
   in Peace's redirect sends it to *exile* instead of the graveyard, and 704.5d
   (keyed to "not on the battlefield," not "in the graveyard") finishes it there.

Step 2 depends on step 1 (there must be tokens to remove). Step 3 is additive
over both and adds no mechanism.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **A card-less object exists and fights (CR 111.2/111.3).** Dragon Fodder
  resolves for its controller; two `OfToken` Goblin objects are on the
  battlefield, each a 1/1 creature (the projection reads P/T and creature-ness off
  the embedded `Card`), owned by and under the control of Dragon Fodder's
  controller (CR 111.2), summoning-sick this turn (CR 302.6). The count field is
  exercised: `Create (Literal 2)` mints **two distinct** objects with distinct
  ids.
- **A token that leaves the battlefield ceases to exist (CR 704.5d — the gate).**
  A Goblin token blocks a Goblin Piker (2/1), takes lethal combat damage, and dies
  — the CR 704.5g SBA buries it to the graveyard (a fresh `OfToken` incarnation,
  CR 400.7), and the next SBA check removes it from the game entirely. After the
  dust settles the token is on the battlefield: no; in the graveyard: **no**
  (the falsifier in a comment: a card-model leaves it in the graveyard); minted-id
  count still advanced (it existed).
- **The funnel composes with Rest in Peace (CR 614 + 704.5d).** With Rest in Peace
  on the board, a dying Goblin is redirected to **exile** (never the graveyard —
  RiP's oracle says "card *or token*"), and 704.5d removes it from exile because
  the SBA is keyed to "not on the battlefield," not to a specific zone. End state:
  gone from battlefield, exile, and graveyard alike.

The `DecisionLog` replays deterministically (no new prompt — creating a token is
never a choice). The honesty round-trip (`jsonToCard . cardToJson ≡ Right`) holds
over `allPrintings` including Dragon Fodder, whose `Create` opcode embeds a nested
token `Card`.

**Non-goals** (each a named expiry in §8):

- **No token printing metadata.** A token references a `Card`, not a `Printing`.
  Physical/printed tokens carry an artist, art, and set symbol, and un-set cards
  may one day read them — but `Printing` carries **no metadata today** (it is a
  bare `newtype` over `Card`; the M3.5 log already defers "`Printing`-granularity"
  metadata). Choosing `OfToken Card` is therefore the *same* deferral the engine
  already makes for cards, not a decision to deny tokens their artist. The future
  shape is `OfToken Card (Maybe Printing)`, landed when `Printing` grows metadata
  **and** a card in scope reads a token's artist/art/set (§2.8's "don't collapse a
  field just because you can't populate it").
- **No replacement on token *entry*.** Doubling Season and "if a token would be
  created, instead…" (CR 614 applied to creation) are future. `createToken` does
  **not** consult replacements. `changeZone` already consults them for *moves*
  (including a dying token's move to the graveyard — that is how the RiP
  interaction works); creation joins when a card needs it.
- **No copy-tokens.** "Create a token that's a copy of…" (Clone-shaped, CR 707)
  needs copy machinery — the future `OfCopy` the `Source` comment predicts, not
  M4c. The token's `Card` is embedded literally in the `Create` opcode.
- **No predefined tokens (CR 111.10), no emblems, no legendary-token rule (CR
  111.9).** Dragon Fodder defines its Goblin inline; the predefined-token catalog
  (Treasure, Food, Clue, …) and emblems are later, additive.
- **Token color carried descriptively only.** CR 111.3 has the effect set the
  token's color ("*red* Goblin"), but no rule in scope reads color (protection,
  devotion, and color words are unbuilt), and `Card` has no color field yet.
  Explicit token color lands with the first color-consuming milestone.

## 1. New and grown types

**`Pawl.Type.Source`** grows its first card-less constructor. The type is already
`data`; the comment predicting `OfToken` is discharged:

```haskell
data Source
  = OfCard Printing
  | -- CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones), with no Printing (a token isn't a card, CR
    -- 111.6; and Printing carries no metadata yet regardless). Game.cardOf returns
    -- this Card, so the whole projection/mana/combat/SBA pipeline reads a token
    -- with no special case. A future OfToken Card (Maybe Printing) carries a
    -- physical token's metadata when Printing grows any (§8).
    OfToken Card
  | OfAbility ObjectId ActivatedAbility
  | OfTrigger ObjectId TriggeredAbility
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Effect`** grows one constructor. `Resolve` remains the sole module
that may `case` on `Effect`; the new arm is added to `slotsOf`, `readsX`,
`manaProduced`, `searchesLibrary`, and `rewriteEffect` (§4, §5).

```haskell
  | -- CR 111: create this many tokens with the given effect-defined
    -- characteristics. The Card is the token's "text" (CR 111.3), embedded
    -- literally in the card data (nested Card-in-Card; the codec and round-trip
    -- cover it). Quantity is how many (reused from M4a, as Draw/Mill/Discard do);
    -- Create (Literal 2) mints two distinct objects. Targetless and unprompted --
    -- creating a token is never a choice. Executed by Resolve.applyEffect via the
    -- new Event.createToken primitive. NOT a copy-token (CR 707) and NOT a
    -- predefined token (CR 111.10): the characteristics are given, not derived.
    Create Quantity Card
```

**`Pawl.Type.Subtype`** needs **no change** — `Goblin` already exists (Goblin
Piker is "Goblin Warrior"), so Dragon Fodder's token subtype is already in the
catalog. (Where a future token needs a novel creature type, the catalog grows one
entry, following M4b's `Myr`.)

No new `Prompt` and no new `Response` (creation is unprompted). No new `Zone`. No
new `ZoneChange` field — the token's enters-the-battlefield event is an ordinary
`ZoneChange` with `to = Battlefield`, and cease-to-exist is not a zone change at
all (§3).

## 2. The create-from-nothing primitive (the gate machinery)

**`Event.createToken :: PlayerId -> Card -> GameState -> GameState`**, a sibling
of `changeZone` in `Pawl.Event`. It lives here, not in `Pawl.Game`, for the same
reason `changeZone` does: it must emit into `GameState.zoneChanges`, which this
module owns, and it is a change-and-emit primitive.

A token is created *from nothing* — it has no prior zone, so `changeZone` (which
begins `Game.lookupObject oid` on an existing object) cannot mint it. `createToken`:

- takes a fresh id and a fresh timestamp (`Game.freshObjectId`, `Game.freshTimestamp`);
- builds an `Object` with `owner = controller` (CR 111.2: the creator is the
  owner and it enters under their control), `source = OfToken card`, `zone =
  Battlefield`, `tapped = Untapped` (CR 110.5b), `damage = 0`, `sickness = Sick`
  (CR 302.6 — a freshly created creature has summoning sickness), `bindings =
  empty`, and the fresh `timestamp`;
- inserts it into `objects` and the battlefield zone (`Game.insertIntoZone`);
- **emits the enters-battlefield `ZoneChange`** — so an ETB trigger (CR 603.6a)
  fires through the *same* path as a resolved permanent. Nothing in scope has such
  a trigger, but the emit is what keeps a token indistinguishable from a card to
  the trigger scan. A token has **no origin zone**, but `ZoneChange.from` is a
  required `Zone` (it exists only for the future leaves-the-battlefield pass, and
  is unread today); the event uses `from = Battlefield`, so `to == from ==
  Battlefield` reads as "appeared on the battlefield" and can never be mistaken
  for a *leave* (a leaves pass keys on `to /= Battlefield`). If distinguishing
  creation from a true battlefield event ever matters, a `Zone.Outside` /
  `from :: Maybe Zone` refinement is future — not M4c.

**Shared tail with `changeZone`.** Both primitives end the same way: put a fresh
object into a zone and emit the `to`-zone event. `changeZone` builds its object by
carrying an existing one forward and resetting per-incarnation state (CR 400.7);
`createToken` builds from scratch. The common step — *insert object under a fresh
id into `dest`, then append the emitted `ZoneChange`* — is extracted into one
helper both call, so there is a single place that "an object entered a zone" is
recorded. The plan owns the exact factoring.

**`Effect.Create Quantity Card`** in `Resolve.applyEffect`: evaluate the quantity
against the resolving spell object (`Quantity.evaluate`; a `Literal 2` for Dragon
Fodder — but X-token cards fall out for free), then fold `Event.createToken
controller card` that many times over the state. `controller` is the resolving
spell's controller, already threaded into `applyEffect`. A quantity of zero or an
unevaluable quantity creates nothing (the `powerOf` no-op posture).

## 3. Cease-to-exist and the Rest in Peace interaction

**CR 704.5d as a state-based action.** `Sba.performStateBasedActions` gains one
check: any object whose `source` is `OfToken` and whose `zone` is **not** the
battlefield is removed from the game — deleted from `GameState.objects` and from
whatever zone holds it. This runs in the same pass as the existing 704.5f/g/h
creature-death checks and the player-loss check; performing it sets the `acted`
flag so the CR 704.4 / CR 117.5 settle loop repeats.

**Cease-to-exist is a *direct delete*, not a `changeZone`.** This is the one
asymmetry with the funnel, and it is correct: the token does not *move to* a zone,
it *leaves the game*. There is no destination, so routing it through `changeZone`
would demand a bogus target zone and would emit a spurious `ZoneChange`. A token
ceasing to exist triggers nothing (CR 704.5d is a removal, not a zone change), so
no emit is wanted. `createToken`'s true opposite is this delete, not `changeZone`.

**The two-pass rhythm is exactly the rules' (CR 111.7).** When a Goblin dies:
pass 1 detects lethal damage (CR 704.5g) and `changeZone`s it to the graveyard —
a fresh `OfToken` incarnation now sits in the graveyard, and the leaves-the-
battlefield event is emitted (a future dies-trigger would see it here). `acted =
True`, so the settle loop repeats. Pass 2: the token is `OfToken` and its zone is
Graveyard ≠ Battlefield → 704.5d removes it. CR 111.7's own parenthetical states
this ordering — "if a token changes zones, applicable triggered abilities will
trigger before the token ceases to exist" — so the token *must* land in the
graveyard for one check, not be intercepted on the way. Keying the SBA to
`OfToken` + zone (never "in the graveyard specifically") is what makes that true.

**Rest in Peace composes with zero new code (the funnel payoff).** Rest in Peace's
`RedirectZoneChange Graveyard → Exile` sits in `changeZone`'s replacement pass
(M3f). A dying Goblin buries via `changeZone _ Graveyard`; the redirect rewrites
the destination to **Exile**, so the fresh `OfToken` incarnation lands in exile
instead. Then 704.5d — because it asks "not on the battlefield," not "in the
graveyard" — removes it from exile on the next pass. The Goblin never touches the
graveyard, exactly as RiP's "card or token" clause requires, and cease-to-exist
still finishes it. This is the M4c echo of M4b's "the funnel generalizes past
RiP's single redirect": here it is *tokens* that funnel through the same redirect
with no token-specific code. A named deterministic fixture asserts it.

**CR 111.8 note (no return).** A token that has left the battlefield can't move to
another zone or return. In M4c nothing tries to move a token off the battlefield
except a death (which is immediately followed by cease-to-exist), so 111.8 has
nothing to bite on yet; it becomes load-bearing only when a card would bounce or
reanimate a token. Recorded, not implemented.

## 4. Source classification and the projection pipeline

The whole point of `OfToken Card` is that `Game.cardOf` — not the projection, not
combat, not the SBA sweep — is the single site that learns a token exists:

```haskell
cardOf oid gs = case ... source of
  Source.OfCard printing -> Just (Printing.card printing)
  Source.OfToken card    -> Just card          -- new: the pipeline is unchanged below this line
  Source.OfAbility _ _   -> Nothing
  Source.OfTrigger _ _   -> Nothing
```

Every characteristic reader — `Projection.baseCharacteristics`, `Mana`,
`Quantity.evaluate`, `Target`, `Combat`, `Sba.creatureDies` — already funnels
through `cardOf`, so a token is a creature, has 1/1, can be targeted, blocks, and
dies with **no change to any of them**. That is the architectural claim of the
milestone, and it is discharged by one arm.

The remaining `case ... of OfCard` sites are exhaustiveness obligations, each
getting an `OfToken` arm that reads the token's `Card` honestly (the plan
enumerates the exact list found at M4b's HEAD):

- **`Action.isLand`** → `Card.isLand card` (a Goblin token is not a land; a
  hypothetical land token would be read correctly).
- **`Cast`** (mana cost, timing) → reads the token's `Card`; a token is never on
  the stack (it is created onto the battlefield, never cast), so these arms are
  total-but-unreachable, not a code path.
- **`Stack.resolveTop`** → likewise unreachable (nothing puts a token on the
  stack), but the case must be total.

**The D4 dataflow lint and the classifications.** `Create` is added to the same
five `Resolve` classifications every opcode declares: `slotsOf` (`Create`
references **no** slot — it is targetless — so it contributes the empty set,
exactly like `Search`/`ExileAllGraveyards`); `readsX` (a `Create (Literal n)`
reads no X; the reserved X slot is unaffected); `manaProduced` (`False` — a token
maker is not a mana ability); `searchesLibrary` (`False`); `rewriteEffect`
(identity on `Create`, or recurse into the embedded `Card` if text-changing ever
must reach a token's printed words — deferred, no in-scope card needs it). The §1
invariant holds: `Resolve` is still the only module that cases on an `Effect`, and
it reads classifications, never an effect's identity, everywhere else.

**The codec.** `Pawl.Codec` gains a `Create` arm serializing `Quantity` and the
nested `Card` (`cardToJson`/`jsonToCard` recurse — a token-maker card contains a
full card). `Source`/`Object`/`GameState` are never serialized (only card data
files are), so `OfToken` needs no codec. The honesty round-trip iterates
`allPrintings`, so Dragon Fodder's nested token `Card` is covered automatically —
the day a closure is smuggled into a token's `Card`, the round-trip fails loudly.

## 5. Invariants preserved

- **The engine never cases on a card's identity.** `Create` embeds a token's
  characteristics as data; `Resolve` cases on the `Effect` constructor (permitted),
  never on *which* token. The closed half asks `cardOf`/the projection, never
  "is this the Goblin token."
- **The engine makes no choices it should ask about.** Creating a token has no
  legal alternatives (CR 111 leaves nothing to choose), so it is unprompted — the
  correct application of the elision rule, not an elision with an expiry.
- **`Resolve` is the sole `case effect of` home; `Event` is the sole `case
  ReplacementEffect`/`TriggerCondition` home.** `createToken` adds no casing on
  either; it only emits an ordinary `ZoneChange`.
- **Every "an object entered a zone" is one recorded event.** `createToken` and
  `changeZone` share the emit tail, so a token entering the battlefield is the same
  kind of event as a permanent entering — the trigger scan cannot tell them apart.

## 6. Setup, decks, and testing

**Deck.** Dragon Fodder joins `redDeck` for random-game token-churn coverage,
swapping in for existing cards to keep the deck at **60** (so the pre-token
conservation counts are unchanged before any token is made) — the same
deck-preserving swap M4a used for Blaze, not a size increase.

**The conservation property must count card-backed objects only.** `PropertySpec`
asserts `Game.objectCount == 120` at game end and `nextIdOf >= 120`. Tokens
legitimately create objects (from nothing) and destroy them (cease-to-exist), so
a token alive at game end — or the transient extra objects during a game — would
false-fail the `== 120` assertion. The fix: the conservation property counts only
**`OfCard`** objects (the 120 real cards are conserved; tokens come and go). The
`nextIdOf >= 120` bound is unchanged and still holds — tokens only mint *more*
ids, never fewer. This is the M4c analog of M4a's deck-count note and M4b's
funnel-generalization; it is a property refinement, not a weakening (the real-card
conservation it asserts is exactly as strong).

**Deterministic fixtures** (the gate, per the fixture posture — most gates land
deterministic, with random-game coverage as the churn tail):

- **Create + fight.** Cast Dragon Fodder; assert two `OfToken` Goblins on the
  battlefield, each a projected 1/1 creature owned/controlled by the caster,
  summoning-sick this turn. Assert two *distinct* ids (the count field).
- **Cease-to-exist (the gate falsifier).** A Goblin blocks a Goblin Piker, takes
  lethal damage; after SBAs settle, assert the token is gone from the battlefield
  **and** absent from the graveyard (the comment names the falsifier: a card-model
  would leave it in the graveyard). Assert `nextIdOf` advanced (it existed).
- **Rest in Peace composition.** With Rest in Peace on the board, kill a Goblin;
  assert it never entered the graveyard, briefly held in exile, and ceased to
  exist — gone from all three zones.
- **Round-trip.** `allPrintings` (now including Dragon Fodder) round-trips
  byte-stable, exercising the nested token `Card`.

## 7. What M4c preserves

- **M4a/M4b are untouched behaviorally.** No opcode changes shape; `Quantity` is
  reused as-is; the `changeZone` funnel and every M4b verb keep their semantics.
  The M3–M4b suite is the regression net.
- **A milestone exit may retire a property.** The `objectCount == 120` property is
  refined to `OfCard`-only counting; that is the milestone landing (tokens now
  exist), not a regression, and the property comment says so.
- **The go/no-go is behind us.** M4c adds no architectural bet — trial application
  (M3c) and the rewritable AST (M3d) already returned YES. This is breadth on a
  proven substrate: one new object *source*, riding the projection and funnel
  built for cards.

## 8. The expiries M4c opens

| Expiry | Retired by |
|---|---|
| **Token printing metadata** — `OfToken Card` carries no artist/art/set; the shape becomes `OfToken Card (Maybe Printing)` | the first card that reads a token's printing metadata (un-set "artist matters"), gated on `Printing` growing metadata at all |
| **No replacement on token entry** — `createToken` doesn't consult CR 614 replacements | the first token-entry replacement (Doubling Season) |
| **No copy-tokens** — the `Create` `Card` is embedded literally | the first copy-token (Clone-shaped), which brings `Source.OfCopy` and CR 707 |
| **No predefined tokens / emblems / legendary-token rule** (CR 111.9/111.10) | the first Treasure/Food/Clue/emblem card |
| **Token color carried descriptively only** — no `Card` color field is read | the first color-consuming rule (protection, devotion) |
| **CR 111.8 (a token can't return)** — nothing moves a token off the battlefield except death | the first card that would bounce or reanimate a token |
| **`rewriteEffect` is identity on `Create`** — text-changing doesn't reach a token's embedded `Card` | the first text-changer that must rewrite a token's printed words |

Spec companion: the implementation plan under
`docs/superpowers/plans/2026-07-19-m4c-tokens.md` (written next, via the
writing-plans skill) owns the commit decomposition and the TDD step order.
