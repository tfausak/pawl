# CR 103.5b: actions taken any time a player could mulligan

*Design pass 2026-07-25. Closes GitHub issue #182 ("CR 103.5b: Allow cards to
impact mulligans"). This is a **phase-sized** spec: one new `Effect` opcode, one
new `Card` field, one new prompt channel, a performer parameter threaded through
four setup entry points, and one new card. Every rules claim is checked against
`docs/rules.txt` and cited by number. The next step is a `writing-plans` plan
derived from this spec.*

## 0. Why this exists

`docs/superpowers/specs/2026-07-24-mulligans-cr-103-5-design.md` §3.2 kept the
per-player mulligan count in a local `Map` rather than in `GameState`, on the
grounds that "no in-game effect asks how many mulligans a player took," and
named the exception in the same breath:

> (Serum Powder […] would want it observable; both are deferred, and if either
> ever lands, promoting the local map to a field is a localized change.)

That prediction turns out to be half wrong, and usefully so. Serum Powder does
**not** want the count observable — its action is not a mulligan and never reads
the count. What it wants is a *window*: a place in the CR 103.5 round loop where
a card in a player's hand may do something. That window is CR 103.5b, and
nothing in pawl has one today, so no card can affect the mulligan process at
all.

## 1. The rules (verbatim ground truth)

CR 103.5b, in full:

> If an effect allows a player to perform an action "any time [that player] could
> mulligan," the player may perform that action at a time they would declare
> whether they will take a mulligan. This need not be in the first round of
> mulligans. Other players may have already made their mulligan declarations by
> the time the player has the option to perform this action. If the player
> performs the action, they then declare whether they will take a mulligan.

The gate card, Serum Powder (Scryfall, `ima/228`):

> Serum Powder {3}
> Artifact
> {T}: Add {C}.
> Any time you could mulligan and this card is in your hand, you may exile all
> the cards from your hand, then draw that many cards.

and its two rulings (2017-11-17):

> You can use Serum Powder's second ability only while it's in your hand. If this
> card is in your hand, you can choose either to mulligan or to use Serum Powder's
> ability. Using the ability doesn't prevent you from taking further mulligans,
> and taking a mulligan doesn't prevent you from using a Serum Powder's ability if
> you happen to draw one in your new hand.

> The cards in your hand are exiled for the rest of the game; they aren't shuffled
> back into your library if you take another mulligan. Cards exiled are always
> face up unless the effect that exiled them says they aren't.

Six load-bearing facts:

1. **The window is the declaration point**, not a separate step: "at a time they
   would declare whether they will take a mulligan."
2. **Every round, not just the first** ("This need not be in the first round").
3. **The declaration still follows** ("If the player performs the action, they
   then declare whether they will take a mulligan") — performing the action does
   not skip, force, or substitute for the declaration.
4. **Performing the action is not taking a mulligan.** Nothing is shuffled,
   nothing is bottomed, and the CR 103.5 mulligan count is untouched — so it
   feeds neither the bottom count nor CR 103.5c's free allowance.
5. **The ability functions in the hand**, a zone CR 613's layer system does not
   reach.
6. **Repeats are legal.** Neither CR 103.5b nor the card limits the action to
   once, and the first ruling's "you can choose either to mulligan or to use
   Serum Powder's ability" is a choice offered at the window, not a budget. A
   hand that redraws into a second Powder can use it in the same window.

## 2. Scope

**In scope.** The CR 103.5b window in the CR 103.5 round loop for all three
opening-hand paths (main game, restart CR 727, subgame CR 729), a card-data
carrier for the action, the one opcode Serum Powder's payload needs, and Serum
Powder itself.

**Out of scope, unchanged:**

- **CR 103.6** — opening-hand actions (Leyline of the Void, Gemstone Caverns).
  Adjacent and frequently confused with 103.5b, but a different mechanism at a
  different time: 103.6 runs *after* the whole mulligan process completes.
  Already tracked as #149 and expected to be the next piece of work.
- **CR 103.5a / 103.5d** — Vanguard hand-size modifiers and the shared-team-turns
  declaration order. Both need a format/variant concept pawl does not have
  (#175), the posture CR 800.6's siblings already take.
- **The starting hand size constant.** `Mulligan.openingHand` stays seven; the
  redraw count is untouched by this work.

## 3. Architecture

### 3.1 The carrier: `Card.mulliganAction`

```haskell
-- CR 103.5b: the effects of this card's "any time you could mulligan" action,
-- in written order. Empty for every card that grants none, which is every
-- printing but Serum Powder.
--
-- Read DIRECTLY from the card and never through the projection, the
-- castingPermissions / additionalCosts precedent: the ability functions in the
-- HAND (CR 113.6), where the CR 613 layer system does not reach.
mulliganAction :: [Effect Card]
```

A flat effect list rather than a wrapper type: there is nothing else to carry.
CR 103.5b's action has no cost, no target, and no condition beyond the zone (the
card's own "and this card is in your hand" is the zone the engine reads it from),
so a record with one field would be a name and nothing more.

An empty list means *no action*, not *an action that does nothing* — the two are
indistinguishable in play (an action with no effects changes nothing and is
therefore not worth offering), so the ambiguity costs nothing and buys a `Maybe`
we would otherwise thread everywhere.

**One action per card.** A printing declaring two would be unrepresentable, and
no printing does. If one ever appears, the field becomes a list of lists and the
prompt gains a discriminator — the `OrderTriggers` / `ChooseReplacement`
positional caveat, filed as an issue rather than built now (§9).

**Codec.** The field takes the optional-key treatment `playerAbilities` and
`additionalCosts` already use — omitted when empty on encode, defaulted to `[]`
on decode via `listFromDefault (getOpt …)`. None of the 86 existing card files
change.

**`Pawl.Card.allEffects`** stays the spell's effects only; it exists for the D4
lint and the CR 612 text-change scan, both of which range over a *spell's* text.
A mulligan action is not part of the spell, has no slots, and cannot be
text-changed from the hand. The lint family's coverage of the new field is §9's
open question, not a silent extension of `allEffects`.

### 3.2 The opcode: `Effect.ExileHandThenDraw`

```haskell
| -- CR 103.5b / Serum Powder: exile every card in the resolving controller's
  -- hand, then draw that many cards. Targetless and controller-scoped, the
  -- ExileAllGraveyards / Draw shape.
  --
  -- ONE opcode rather than an exile composed with a Draw: "that many" is the
  -- hand size BEFORE the exile, and a following Draw would read a hand that is
  -- already empty. Splitting it needs a Count that reads a value produced
  -- earlier in the same resolution, which nothing else wants.
  ExileHandThenDraw
```

Executed by `Resolve.applyEffectWith`: read the controller's hand through
`Game.zoneMembers`, move each card to `Zone.Exile` through the `Event.changeZone`
funnel, then `Event.drawCard` that many times. The count is captured before the
first move.

Two consequences worth stating rather than special-casing:

- **The Powder exiles itself.** It is a card in the hand; CR 103.5b's action is
  not a cost and nothing sets it aside. The ruling's "the cards in your hand are
  exiled for the rest of the game" is exactly the plain zone move.
- **Exile is face up.** pawl models no face-down exile, which is what the ruling
  says the default is ("always face up unless the effect that exiled them says
  they aren't"). Nothing to implement.

Drawing from a short library during the action sets `drewFromEmpty` exactly as
the mulligan redraw already does, so CR 727.3 / 729.3's short-deck loss keeps
working through this path with no extra machinery.

### 3.3 The cycle, and the performer parameter

`Pawl.Mulligan` **cannot** import `Pawl.Resolve`:

```
Resolve  --(Effect.RestartGame)-->  Setup.restartGame
Setup    --(startGameFromCards)-->  Mulligan.openingHands
```

That is a genuine mutual recursion in the rules, not an accident of layout: an
opcode restarts a game, a game start draws opening hands, and (with CR 103.5b)
drawing opening hands performs opcodes. `hs-boot` is out (project norm), and no
narrowing of `Setup` breaks it — moving `restartGame` to its own module just
moves the same cycle.

So the effect performer is **passed in**, the existing
`resolveSpellWith runSubgame` precedent for exactly this situation:

```haskell
-- Pawl.Type.MulliganPerformer (new; the Pawl.Type.Game synonym precedent)
--
-- CR 103.5b: how the closed half performs a mulligan-window action's effects --
-- the source object, the acting player, and the effects. A PARAMETER rather
-- than an import because Pawl.Resolve sits ABOVE Pawl.Mulligan
-- (Effect.RestartGame -> Setup.restartGame -> startGameFromCards ->
-- openingHands), so importing it here would close a cycle.
type MulliganPerformer = ObjectId -> PlayerId -> [Effect Card] -> Game ()
```

Threaded through, in dependency order:

```haskell
Resolve.performMulliganAction :: MulliganPerformer     -- the one implementation
Setup.restartGame            :: MulliganPerformer -> PlayerId -> Game ()
Setup.startGameFromCards     :: MulliganPerformer -> Game ()
Setup.newGame                :: MulliganPerformer -> NonEmpty (PlayerId, Deck) -> Game ()
Mulligan.openingHands        :: MulliganPerformer -> [PlayerId] -> Game ()
Mulligan.mulliganRounds      :: MulliganPerformer -> Map PlayerId Natural -> [PlayerId] -> Game ()
```

**No default.** `resolveSpellWith` can pair with `Resolve.noSubgame` because "no
subgame runner" is a real state of the world; "no mulligan performer" is not — it
would silently disable every 103.5b card at whichever call site forgot. A
required parameter makes the compiler ask.

`Pawl.Mulligan` mentions `Effect` and never cases on it: the candidate list is
built from a *classification* (does this card declare a non-empty
`mulliganAction`?), and the payload is handed to the performer opaquely. That is
the closed/open split holding, not bending.

### 3.4 The window in `Mulligan.mulliganRounds`

A new classification, beside `freeMulligans`:

```haskell
-- CR 103.5b: the cards in this player's hand that declare an action they may
-- take at their mulligan declaration, each with the effects that action
-- performs. A classification, not an identity test: the engine asks whether the
-- card declares an action, never which card it is.
actionsFor :: PlayerId -> GameState -> [(ObjectId, [Effect Card])]
```

The declaration sub-pass gains a loop immediately before each still-deciding
player's `DeclareMulligan` prompt:

```
for each still-deciding player pid, in turn order:
  window loop:
    candidates := actionsFor pid gs
    if null candidates -> leave the loop
    answer <- prompt MulliganAction (deciderFor pid) pid (fst <$> candidates)
    case answer of
      Nothing                       -> leave the loop        (declined)
      Just oid not among candidates -> leave the loop        (total; see below)
      Just oid                      -> perform oid pid (its effects); loop again
  then: DeclareMulligan, exactly as today
```

Fidelity notes:

- **Every round.** The loop is inside the per-round declaration pass, so a player
  who mulliganed in round 1 gets the window again in round 2 (CR 103.5b: "This
  need not be in the first round of mulligans").
- **Only still-deciding players.** A player who kept has left the pool and never
  declares again, so they have no window — CR 103.5b's "at a time they would
  declare" says so.
- **Not a mulligan.** `counts` is untouched by the window, so CR 103.5's bottom
  count and CR 103.5c's free allowance are unaffected, and the `DeclareMulligan`
  prompt still reports the same `taken`.
- **Repeats terminate.** Each performance permanently removes at least one card
  from the deck (the exile is for the rest of the game), and a hand with no
  card declaring an action offers no candidates, so the loop is bounded even
  against an interpreter that always accepts. An empty library makes the redraw
  draw nothing, so the next pass has an empty hand and no candidates.
- **The zero-card hand.** The existing `handSize <= 0 -> Keep` short-circuit is
  unchanged: candidates come from the hand, so a player with no cards has no
  action to take and nothing is elided by not asking.
- **An unoffered answer is a decline.** The engine validates by membership rather
  than trusting the id — the `Action.Activate` posture (validated by membership
  in `Projection.abilitiesOf`, never an index) — which keeps the loop total with
  no partial lookup.

## 4. The prompt

```haskell
-- CR 103.5b: an effect that lets this player act "any time [they] could
-- mulligan" -- offered at the moment they would declare (CR 103.5), before the
-- declaration, and again after each action they take. The [ObjectId] is the
-- cards in their hand declaring such an action; the answer is which to use, or
-- Nothing to decline. Performing one is NOT a mulligan: nothing is shuffled or
-- bottomed and the mulligan count is untouched, so the declaration still
-- follows (CR 103.5b last sentence). Offered every round, not just the first,
-- and only to a player who has not yet kept. Not asked when the list is empty.
MulliganAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

`Response` gains `TookMulliganAction (Maybe ObjectId)` — its own constructor, not
a reuse of `Searched` / `CastWhileSearched`, for the reason `ChoseDefender`'s
comment already records: `decode`'s job is to return `Nothing` for a response
that does not match the prompt being asked, and two prompts sharing a constructor
cannot do that.

`Pawl.Replay` gains the three mechanical arms: `encode`
(`MulliganAction {} -> Response.TookMulliganAction answer`), `decode` (the
matching branch), and `defaultAnswer` (`Nothing` — declining is always legal and
the least-eventful fallback when a transcript runs short, mirroring
`DeclareMulligan -> Keep`).

The exhaustive `Prompt` answerers that gain the new arm all answer `Nothing`:
five interpreters in `Support.hs`, three in `benchmark/Main.hs`, three in
`CastSpec.hs` and two in `GameSpec.hs` — thirteen in all. (`MulliganSpec`'s own
answerers end in a wildcard delegating to `S.identityAnswer` and need none.)
`-Werror` turns each omission into a compile error, so none can be silently
missed.

## 5. Card data

`data/cards/serum-powder.json`, from the oracle text in §1:

- `manaCost`: `[{Generic 3}]`; `typeLine`: types `[Artifact]`, no subtypes or
  supertypes; no power or toughness.
- `activatedAbilities`: one — cost `[TapThis]` with no mana, mode effects
  `[AddMana Colorless]`. The Llanowar Elves shape with a colorless payload; it is
  a mana ability (CR 605.1a), so `Resolve.manaProduced` classifies it and
  `Mana.tapForMana` executes it, both unchanged.
- `mulliganAction`: `[ExileHandThenDraw]`.
- Everything else empty.

The `{T}: Add {C}` half is not what the issue is about, but it is printed on the
card and this project's cards are whole printings, not the fragment a milestone
needs. It also costs nothing: colorless mana already exists (`ManaType.Colorless`)
and Reliquary Tower already produces it.

## 6. Testing (`Pawl.MulliganSpec`, extended)

The window is exercised with a real deck containing real Serum Powders, per
tests-prefer-real-cards. Two interpreters: one that takes the action once then
keeps, one that always declines.

1. **The action exiles the whole hand and redraws that many.** After one use,
   hand size is back to what it was, the old hand's ids are all in `Zone.Exile`
   (including the Powder that was used), and the library is that many cards
   shorter.
2. **The declaration still follows** (CR 103.5b last sentence). A recording
   interpreter shows `MulliganAction` then `DeclareMulligan` for the same player,
   in that order.
3. **The action is not a mulligan.** Taking it and then mulliganing bottoms
   **one** card, not two — the `DeclareMulligan` prompt's reported count is
   unchanged by the action, and CR 103.5's bottom count comes out at 1.
4. **Offered in a later round** (CR 103.5b: "need not be in the first round"). A
   player who mulligans in round 1 and draws into a Powder is offered the window
   in round 2.
5. **Repeats within one window.** An interpreter that accepts twice exiles two
   hands; the loop then ends when the third hand holds no Powder.
6. **Not offered without a candidate.** A deck with no Powder issues no
   `MulliganAction` prompt at all (assert over the recorded asks).
7. **A kept player gets no window.** A player who keeps in round 1 is offered
   neither a declaration nor an action in round 2 while an opponent keeps the
   loop alive.
8. **Replay determinism.** A `record` / `replay` round-trip over a game
   containing a mulligan action reproduces the final state.
9. **Short-deck loss survives the action** (CR 727.3 / 729.3). Exiling into a
   library too short to redraw still sets `drewFromEmpty`.

`Pawl.CardsSpec` / `Pawl.CardSpec` cover `serum-powder.json` by their existing
whole-directory sweeps (each file re-parses to its compiled card; each file name
is a slug). `Pawl.CodecSpec` gains the `ExileHandThenDraw` and `mulliganAction`
round-trips its per-constructor coverage expects.

## 7. Blast radius

- **New:** `Pawl.Type.MulliganPerformer`, `data/cards/serum-powder.json`.
- **Modified types:** `Pawl.Type.Effect` (+1 constructor), `Pawl.Type.Card`
  (+1 field), `Pawl.Type.Prompt` (+1 constructor), `Pawl.Type.Response`
  (+1 constructor).
- **Modified logic:** `Pawl.Codec` (effect + card arms), `Pawl.Replay`
  (encode/decode/defaultAnswer), `Pawl.Resolve` (the new opcode arm,
  `performMulliganAction`, and the `RestartGame` arm passing it),
  `Pawl.Mulligan` (`actionsFor`, the window loop, the performer parameter),
  `Pawl.Setup` (parameter on `newGame`, `startGameFromCards`, `restartGame`),
  `Pawl.Engine` (two call sites), `Pawl.hs` (the `Setup.newGame` re-export's
  signature).
- **Modified tests/benchmark:** ~25 call sites of the four re-signatured setup
  functions across `SetupSpec`, `GameSpec`, `CastSpec`, `MulliganSpec`, plus the
  new `Prompt` arm in every exhaustive answerer (`Support.hs`,
  `benchmark/Main.hs`, and the spec-local answerers).

The call-site churn is mechanical and compiler-enforced. There are no API
stability obligations (`CLAUDE.md`), so the four signatures change in place with
no shim.

## 8. Definition of done

1. `cabal build all --enable-tests --enable-benchmarks` is warning-clean under
   `+pedantic` (after a `cabal clean`, since incremental builds hide warnings
   from unchanged modules).
2. `hooky fix` applied, `hooky run` passes; HLint clean.
3. `Pawl.MulliganSpec`'s new cases pass; the existing mulligan, setup, restart
   and subgame suites still pass.
4. Every rules claim cites CR 103.5b (or the specific sub-rule) and was checked
   against `docs/rules.txt`.
5. Serum Powder loads from `data/cards` and round-trips through the codec.
6. Issue #182 closed; `docs/progress.md` gains an entry; §9's issues filed with
   `(#N)` citations at their code sites.

## 9. Deferrals to file

- **A card declaring two CR 103.5b actions is unrepresentable.** `mulliganAction`
  is one action's effects, and `Prompt.MulliganAction` identifies an action by its
  source object alone. No printing declares two. Labels:
  `gap, expires:card-driven`. Cited at the `Card.mulliganAction` field.
- **The `Pawl.CardSpec` lint family does not range over `mulliganAction`.**
  `cardOffends` walks the spell's modes; a slot named in a mulligan action would
  go unlinted. Nothing can name one today (the opcode is targetless and
  slotless), so this is a shape the lint should grow *with* the first slotted
  mulligan action, not before. Labels: `gap, expires:card-driven`. Cited at
  `Pawl.Card.allEffects`.
