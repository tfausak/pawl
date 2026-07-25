# CR 103.6a: opening-hand actions

*Design pass 2026-07-25. Closes GitHub issue #149 ("CR 103.6: opening-hand
actions (Leyline, Gemstone Caverns)"). This is a **phase-sized** spec: one new
`Card` field, one new prompt channel, one new `ControllerRelation` constructor,
one new window in the opening-hand pipeline, and one new card. **No new
opcode.** Every rules claim is checked against `docs/rules.txt` and cited by
number; the gate card's text is checked against Scryfall. The next step is a
`writing-plans` plan derived from this spec.*

## 0. Why this exists

`Mulligan.openingHands` stops when the CR 103.5 mulligan process ends. CR 103.6
defines one more step before the first turn, and pawl has no seam for it: no card
can begin the game on the battlefield, and no card can be revealed from an
opening hand.

This lands directly on top of the CR 103.5b work (#182), which built the first
"a card in hand may act before the game begins" window. Almost everything that
window needed — the performer parameter, the hand-scanning classification, the
decline-or-repeat loop — is reused here rather than rebuilt.

## 1. The rules (verbatim ground truth)

> **103.6.** Some cards allow a player to take actions with them from their
> opening hand. Once the mulligan process (see rule 103.5) is complete, the
> starting player may take any such actions in any order. Then each other player
> in turn order may do the same.

> **103.6a** If a card allows a player to begin the game with that card on the
> battlefield, the player taking this action puts that card onto the battlefield.

> **103.6b** If a card allows a player to reveal it from their opening hand, the
> player taking this action does so. The card remains revealed until the first
> turn begins. Each card may be revealed this way only once.

> **103.6c** In a multiplayer game using the shared team turns option, first each
> player on the starting team, in whatever order that team likes, may take such
> actions. Teammates may consult while making their decisions. Then each player
> on each other team in turn order does the same.

The gate card, Leyline of the Void (Scryfall, verified 2026-07-25 — `{2}{B}{B}`,
Enchantment, no P/T):

> If this card is in your opening hand, you may begin the game with it on the
> battlefield.
> If a card would be put into an opponent's graveyard from anywhere, exile it
> instead.

Five load-bearing facts:

1. **After the whole mulligan process**, not inside it — "Once the mulligan
   process … is complete". This is a separate window from CR 103.5b's, which sits
   *at* a declaration.
2. **Turn order, starting player first**, exactly like the declaration pass.
3. **"any such actions in any order"** — a player may take several, and chooses
   the order, so the window is a loop, not a single ask.
4. **103.6a moves the card to the battlefield.** Nothing else: no cost, no
   trigger of its own, no choice beyond whether to do it.
5. **103.6b's reveal carries state** the rest of the rule does not: a card stays
   revealed "until the first turn begins", and each card may be revealed "only
   once".

## 2. Scope

**In scope.** The complete CR 103.6 window for all three opening-hand paths (main
game, restart CR 727, subgame CR 729), the CR 103.6a action, and Leyline of the
Void.

**Explicitly deferred** (file as a GitHub issue, cite `(#N)` at the code site):

- **CR 103.6b** — revealing a card from the opening hand (the Chancellor cycle).
  It needs two pieces of state nothing else in pawl wants yet: a per-object
  *revealed* flag cleared when the first turn begins, and once-only tracking so a
  card cannot be revealed twice. 103.6a needs neither — putting the card onto the
  battlefield removes it from the hand, which is its own once-only guard. Labels:
  `gap, expires:card-driven`.
- **CR 103.6c** — shared team turns. Needs the format/variant concept pawl does
  not have (#175), the same posture CR 103.5d and CR 801–811 already take. No new
  issue; it belongs to #175.
- **Gemstone Caverns** — a 103.6a card, but its rider ("with a luck counter on
  it. If you do, exile a card from your hand") needs a new `CounterKind`, a
  discard-shaped choice, and a "you're not the starting player" condition on the
  action itself. The window this spec builds is what it will eventually plug
  into. Labels: `gap, expires:card-driven`.

**Out of scope, unchanged:** CR 103.5 and CR 103.5b, which run to completion
before this window opens.

## 3. Architecture

### 3.1 Where the window lives

`Mulligan.openingHands` gains a third phase:

```haskell
openingHands perform owners = do
  Monad.forM_ owners (Monad.replicateM_ openingHand . Event.drawCard)  -- CR 103.5
  mulliganRounds perform Map.empty owners                              -- CR 103.5 + 103.5b
  openingHandActions perform owners                                    -- CR 103.6
```

`owners` is already in turn order with the starting player first, which is
exactly the order CR 103.6 asks for, and all three setup paths already funnel
through this one entry point.

**`Pawl.Mulligan` keeps its name.** The module will own a rule that is explicitly
*not* a mulligan, which is a fair objection — the alternative considered was
renaming it `Pawl.OpeningHand` to match its own entry point and its actual
charter, which the no-API-stability rule makes cheap (ten importers). Rejected
because the split would be worse than the mismatch: `openingHands` has to stay
one function (both setup paths call it, and CR 103.6 is *defined* relative to the
mulligan process — "once the mulligan process is complete"), so a rename buys a
better module name at the cost of churning ten files and splitting the
`Mulligan.*` type family (`MulliganDecision`, `MulliganOffer`) away from the
module that produces them. The module header gains a sentence saying it owns CR
103.5 through 103.6.

### 3.2 The carrier: `Card.openingHandAction`

```haskell
-- CR 103.6: the effects of this card's opening-hand action, in written order --
-- what "you may begin the game with it on the battlefield" (CR 103.6a) does when
-- the player takes it. Empty for every printing but Leyline of the Void.
--
-- Read DIRECTLY from the card and never through the projection, the
-- mulliganAction / castingPermissions precedent: the ability functions in the
-- HAND (CR 113.6), where the CR 613 layer system does not reach.
--
-- The SIBLING of mulliganAction, not a reuse of it: the two windows are at
-- different times (CR 103.5b sits at a declaration, CR 103.6 opens once the
-- whole mulligan process is complete), and a card that acts at one must not be
-- offered at the other.
openingHandAction :: [Effect Card]
```

`Mulligan.actionsFor` is generalized to take the field selector, so both windows
share one piece of hand-scanning plumbing rather than growing a near-copy:

```haskell
actionsFor :: (Card.Card -> [Effect Card.Card]) -> PlayerId -> GameState -> [(ObjectId, [Effect Card.Card])]
```

Its two callers pass `Card.mulliganAction` and `Card.openingHandAction`.

### 3.3 No new opcode: `MoveToZone` plus the reserved `self` slot

CR 103.6a's action is "puts that card onto the battlefield" — a single-object
move to a known zone, which `Effect.MoveToZone SlotName Zone` already *is*. The
only question is how the card names itself, and the codebase has already answered
it. `Pawl.Type.Effect`'s `Sacrifice` comment:

> One opcode, not a targetless `SacrificeSelf` plus a slotted variant: "this
> creature" is expressible because `Engine.placeOne` binds the trigger's SOURCE
> into the reserved `Pawl.Binding.triggerSource` slot

So Leyline's action is data, with no ISA change at all:

```haskell
openingHandAction = [MoveToZone Binding.triggerSource Zone.Battlefield]
```

A `PutSourceOntoBattlefield` (or a general `MoveSourceToZone Zone`) opcode was
considered and rejected on exactly the precedent above: it would be the
self-variant the `Sacrifice` comment argues against, and it would be the second
way to express one operation.

**The performer binds the slot.** `Resolve.performMulliganAction` currently
passes three empty maps. It now binds the granting card into the reserved slot:

```haskell
Monad.mapM_
  ( applyEffect source player Map.empty
      (Map.singleton Binding.triggerSource True)
      (Map.singleton Binding.triggerSource (Recipient.ToObject source))
  )
```

This is uniformly correct for **both** windows — each passes "the card in hand
that granted the action" as `source` — so CR 103.5b gains the same expressiveness
for free. The legality map entry is `True` because there is no CR 608.2b
target-legality question here: this is not a target (CR 115.1), it is the
reserved self-reference, and the card is in the hand of the player acting by
construction.

**The move goes through `Event.changeZone`**, so a permanent that begins the game
on the battlefield gets the full CR 614.12a as-enters treatment
(`Replacement.runEntry` fires for a battlefield destination) and any enters
trigger is queued like every other. Nothing in the pool has one; the engine's
existing settle schedule would put such a trigger on the stack at the first
priority of turn 1, which is where CR 103.6 leaves it.

### 3.4 The performer's name

`Pawl.Type.MulliganPerformer` is now used by two windows, one of which CR 103.6
explicitly is not. It is renamed `Pawl.Type.HandActionPerformer`, with
`Resolve.performMulliganAction` → `Resolve.performHandAction`; `S.performer` keeps
its name. Same drift argument as §3.1, but here the fix is genuinely cheap: the
type has one implementation, is threaded through four functions, and every site
is compiler-enforced. Landed 2026-07-25 under #182, so nothing outside this
repository can be relying on it.

### 3.5 `ControllerRelation.Opponents`, and whose zone the destination is

Leyline's second ability is Rest in Peace's shape with a different relation:

```haskell
replacementEffects = [ZoneChangeR (MkZoneChangePattern Graveyard Opponents) Exile]
```

`ControllerRelation` today is `Yours | Anyones`, so `Opponents` is new. Two arms
consume it, and they must answer **different questions**:

- **`CounterR` / `TokenR`** ask about the object's *controller* (CR 109.5's "you",
  Hardened Scales). Unchanged; `Opponents` gains a controller-based arm for
  totality, with no producer today.
- **`ZoneChangeR`** asks about the destination *zone's* player, which CR 400.3
  ("if an object would go to any library, graveyard, or hand other than its
  owner's, it goes to its owner's corresponding zone") and CR 404.1 make the
  object's **owner**, not its controller. Leyline says "an opponent's graveyard";
  a creature an opponent stole with Act of Treason still dies to *its owner's*
  graveyard, so a controller-based test would get that case backwards.

So the zone-change subject test is split out of `matchesController` into its own
owner-based function, cited to CR 400.3. This is not merely an `Opponents`
question: it makes `Yours` correct for the zone-change class too, where it means
"would be put into *your* graveyard". No card produces that combination today, so
the split changes no behavior in the current pool — `Anyones`, the only relation
any committed `ZoneChangeR` uses (Rest in Peace), answers `True` either way.

"Opponent" is read as "any other player still in the game" — CR 102.1 with CR
806.1's free-for-all reading, the `/= you` test `Count.playersFor` and
`Filter.matches` already use. Teams (CR 102.3) would make this wrong and are
#175.

## 4. The prompt

```haskell
-- CR 103.6: an action a card in this player's opening hand lets them take once
-- the mulligan process is complete -- "begin the game with it on the
-- battlefield" (CR 103.6a). The [ObjectId] is the cards in hand offering one;
-- the answer is which to take, or Nothing to decline.
--
-- Offered in turn order, starting player first, and repeatedly to the same
-- player until they decline: CR 103.6 lets a player take "any such actions in
-- any order", so both which and how many are theirs to choose.
--
-- A SEPARATE channel from MulliganAction, not a reuse: that window sits at a
-- mulligan declaration and this one opens after the whole process, so an
-- interpreter that could not tell them apart could not answer either well.
-- Not asked when the list is empty.
OpeningHandAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

`Response` gains `TookOpeningHandAction (Maybe ObjectId)` — its own constructor
for the reason `ChoseDefender`'s comment records: `decode` must return `Nothing`
for a response that does not match the prompt being asked, and two prompts
sharing a constructor cannot do that.

`Pawl.Replay` gains the three mechanical arms; `defaultAnswer` is `Nothing`
(declining is always legal and least-eventful, mirroring `MulliganAction`).

Every exhaustive `Prompt` answerer gains an arm answering `Nothing`. As with
#182, most match with `{}` and are unaffected by shape, but a **new constructor**
breaks all of them regardless: five interpreters in `Support.hs`, three in
`benchmark/Main.hs`, three in `CastSpec.hs`, two in `GameSpec.hs`, and the
`MulliganSpec` locals that do not end in a wildcard.

## 5. The window

```
openingHandActions perform owners:
  for each owner, in turn order (starting player first):
    loop:
      candidates := actionsFor Card.openingHandAction owner
      if null candidates -> leave the loop
      answer <- prompt OpeningHandAction (deciderFor owner) owner (fst <$> candidates)
      Nothing, or an id not among the candidates -> leave the loop
      Just oid -> perform oid owner (its effects); loop again
```

The body is `mulliganWindow`'s, with a different field selector and a different
prompt, so the two are written as one shared loop parameterized by both — the
duplication is otherwise exact:

```haskell
-- The shared CR 103.5b / CR 103.6 loop. `field` selects which of a card's two
-- hand-action lists this window offers; `ask` is the prompt channel it offers
-- them on. Both windows are otherwise identical: offer, perform, repeat until
-- declined or empty.
handWindow ::
  (Card.Card -> [Effect Card.Card]) ->
  (Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)) ->
  HandActionPerformer ->
  PlayerId ->
  Game ()
```

Passing the prompt constructor as a value needs no extension: its result type is
the fixed `Prompt (Maybe ObjectId)`, not a polymorphic one, so this is an
ordinary function argument rather than a rank-2 one.

**Termination.** Each CR 103.6a action moves its card out of the hand, so the
candidate list strictly shrinks; a player who never declines still runs out. This
is the same argument the CR 103.5b window makes, and it is why 103.6a needs no
once-only tracking while 103.6b (§2) would.

**A player with no such card is never asked** — where the rules leave nothing to
ask, don't prompt.

**Departed players.** `owners` is the still-playing seats for the rebuild paths
(`startGameFromCards` derives it from `Departure.stillPlayingInOrder`), so a
player who has left gets no window, exactly as they get no opening hand.

## 6. Testing (`Pawl.MulliganSpec`, extended)

1. **The action is offered after the mulligan process, not during it.** A
   recording interpreter shows every `DeclareMulligan` for both players before the
   first `OpeningHandAction`.
2. **Taking it puts the card onto the battlefield.** Leyline moves from hand to
   battlefield; the hand is one smaller; its controller is the player who acted.
3. **Declining leaves it in hand.** The Leyline is still in the opening hand and
   the battlefield is empty.
4. **Turn order.** With a Leyline in both opening hands, the starting player is
   offered before the other (CR 103.6, and it matches `owners`).
5. **Two Leylines, both taken.** The window re-offers after each action and ends
   when the hand holds none — CR 103.6's "any such actions in any order".
6. **Not offered without a granting card.** A Mountain-only deck issues no
   `OpeningHandAction` prompt at all.
7. **A card taken this way survives to turn 1.** Running a real game start leaves
   the Leyline on the battlefield when the first turn begins.
8. **Replay determinism.** A `record` / `replay` round-trip over a game with an
   opening-hand action reproduces the final state.
9. **Leyline's replacement works** (`Pawl.ReplacementSpec` or `ResolveSpec`): a
   card that would be put into an opponent's graveyard is exiled instead, while
   the Leyline controller's own card still reaches their graveyard — the
   assertion that distinguishes `Opponents` from `Anyones`.
10. **Owner, not controller** (the §3.5 claim): a creature owned by the Leyline's
    controller but controlled by the opponent still goes to its owner's
    graveyard, so it is *not* exiled. This is the case a controller-based test
    would get wrong.

`Pawl.CardsSpec` covers `leyline-of-the-void.json` by its whole-directory sweeps;
`Pawl.CodecSpec` gains the `openingHandAction` and `ControllerRelation.Opponents`
round-trips.

## 7. Blast radius

- **New:** `data/cards/leyline-of-the-void.json`.
- **Renamed:** `Pawl.Type.MulliganPerformer` → `Pawl.Type.HandActionPerformer`;
  `Resolve.performMulliganAction` → `Resolve.performHandAction`.
- **Modified types:** `Pawl.Type.Card` (+`openingHandAction`),
  `Pawl.Type.Prompt` (+`OpeningHandAction`), `Pawl.Type.Response`
  (+`TookOpeningHandAction`), `Pawl.Type.ControllerRelation` (+`Opponents`).
- **Modified logic:** `Pawl.Codec` (the card field, the relation arm),
  `Pawl.Replay` (three arms), `Pawl.Resolve` (the performer's bindings and its
  name), `Pawl.Mulligan` (`actionsFor`'s selector, the shared window loop,
  `openingHandActions`, the third phase), `Pawl.Replacement` (the owner-based
  zone-change subject test).
- **Modified tests:** every exhaustive `Prompt` answerer, the four
  `Card.Type.MkCard` literals, `MulliganSpec`, `CodecSpec`, `CardsSpec`, and the
  replacement suite.

## 8. Definition of done

1. `cabal build all --enable-tests --enable-benchmarks` is warning-clean under
   `+pedantic`, from a `cabal clean`.
2. `hooky fix` applied, `hooky run` passes; HLint clean.
3. The new `MulliganSpec` and replacement cases pass; the whole existing suite
   still passes.
4. Every rules claim cites CR 103.6 (or the specific sub-rule) and was checked
   against `docs/rules.txt`; Leyline's text was checked against Scryfall.
5. Issue #149 closed; `docs/progress.md` gains an entry; §9's issues filed with
   `(#N)` citations at their code sites.

## 9. Deferrals to file

- **CR 103.6b, revealing from the opening hand.** Needs a per-object revealed
  flag cleared when the first turn begins, plus once-only tracking. The window
  this spec builds is where it plugs in. Labels: `gap, expires:card-driven`.
- **Gemstone Caverns.** A 103.6a card whose rider needs a luck `CounterKind`, an
  exile-from-hand choice, and a condition on the action ("you're not the starting
  player"). Labels: `gap, expires:card-driven`.
- **#184 gains a note rather than a sibling.** The `CardSpec` lint family does not
  range over `Card.mulliganAction`; it now does not range over
  `Card.openingHandAction` either. That issue also inherits the trap
  `Pawl.Binding`'s comment already documents: `Resolve.slotsOf` returns the
  reserved `self` slot for a `MoveToZone Binding.triggerSource _`, so an
  equality-style D4 lint widened to these fields must subtract the reserved slot
  names before comparing, or it becomes unsatisfiable. Comment on #184; do not
  file a duplicate.
