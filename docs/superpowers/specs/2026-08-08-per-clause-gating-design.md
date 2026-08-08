# Per-clause gating (#335)

## The problem

`Mode` carries two resolution-time gates -- `optionality` (CR 603.5's printed "may") and
`unlessPaid` (CR 118.12a's resolution cost) -- and both cover the mode's WHOLE effect
list. A card whose "may" governs only some of its instructions has nowhere to be
written: the ungated instructions would be skipped along with the gated one.

The same sentence is true of `unlessPaid` (#703), and #701's positive polarity and
#487's reflexive "if you do" sit beside them. This is the single largest shape-cluster
in the backlog.

## What the CR says

The rulebook already splits the two units this design separates, and already has a word
for the smaller one.

- **The mode is fixed as the spell is cast.** CR 601.2b/700.2b choose the modes, CR
  601.2c chooses the targets, and none of it is revisited.
- **The clause is decided as the effect is applied.** CR 608.2c: "The controller of the
  spell or ability follows its instructions in the order written." CR 608.2d: "If an
  effect of a spell or ability offers any choices other than choices already made as
  part of casting the spell ... the player announces these while applying the effect."
- **And CR 608.2e names the unit outright:** "Some spells and abilities have multiple
  steps or actions, **denoted by separate sentences or clauses**, that involve multiple
  players."

CR 608.2d's own example is a partial gate: "A spell's instruction reads, 'You may
sacrifice a creature. If you don't, you lose 4 life.'" -- a spell's *instruction*, not a
spell.

So `Mode` today is doing two jobs the CR keeps apart. It gets away with it because no
card in the pool has ever pulled them apart.

## The card

**Shed Weakness** -- `{G}` Instant, Amonkhet #185 (2017), common; reprinted in Ultimate
Masters #181 and Amonkhet Remastered #216. Verified against Scryfall.

> Target creature gets +2/+2 until end of turn. You may remove a -1/-1 counter from it.

One mode, one target, two instructions, and only the second is gated. The `+2/+2`
happens whether or not the "may" is exercised, which is exactly what a mode-wide gate
gets wrong.

**#335's own nominated card is not expressible.** Satyr Wayfinder needs "reveal the top
four cards of your library, you may put a land card from among them into your hand" --
pawl has no look-at-N-and-choose machinery at all (`SearchDestination.RevealThenHand` is
reachable only from the `Search` opcode). That section of the issue needs correcting.

**And there is a trap in this shape worth recording.** Urban Evolution, Scale the
Heights and the real Explore all print "You may play an additional land this turn."
That "may" is the permission's own wording, not CR 603.5's
resolution-time choice: pawl models it `Mandatory` plus
`PlayerEffect.PlayAdditionalLands`, which already exists, and such a card would exercise
nothing here. The issue's original Explore example fell into exactly this, and the
Oracle text it quoted does not even match the printing.

Two other candidates were rejected on cost rather than on shape, and are worth recording
so the search is not redone. **Put Away** (`{3}{U}` Instant, "Counter target spell. You
may shuffle up to one target card from your graveyard into your library") needs nothing
new in the ISA -- `Counter` and `ShuffleIntoLibrary` both exist -- but `Pool` has no
graveyard-cards arm and there is no "up to one" optional target, so it drags two axes
rather than one. **Broken Bond** (`{2}{G}` Sorcery, "Destroy target artifact or
enchantment. You may put a land card from your hand onto the battlefield") drags
hidden-zone selection, which is #559's neighbourhood and larger than either.

## Design

### `Pawl.Types.Clause`

A new type in the `types` sublibrary:

    data Clause card = MkClause
      { optionality :: Optionality.Optionality,
        unlessPaid :: Maybe UnlessPaid.UnlessPaid,
        effects :: Seq.Seq (Effect.Effect card)
      }

Parametric in `card` for `Mode`'s and `Effect`'s reason -- `Card` embeds the payload, so
a concrete `Effect Card` here would cycle.

A **product of independent riders**, not a `Gate` sum, which is what `Mode` already is:
two optional riders asked in printed order, each with its own CR citation and its own
haddock. Gates that compose ("if you control a Forest, you may ...") compose without a
combinator. A sum would need `And`, and once `And` exists the gate is a small expression
language rather than a classification -- the thing `design.md` section 1 guards against.

**No `condition` rider in this change.** The adapt/monstrosity/amass family (#876) and
#808's third bullet want one, and no card in this PR exercises it. Building it now is
the capability-without-a-card `design.md` section 4 forbids. Tracked as #1045.

**No polarity on `unlessPaid` in this change**, for the same reason: #701's positive
branch lands with Standstill, not before.

**No `targetSpecs` on a `Clause`.** CR 601.2c fixes targets as the spell is cast, which
is the mode's job, not the clause's. That asymmetry IS the design.

### `Pawl.Types.Mode`

    data Mode card = MkMode
      { clauses :: Seq.Seq (Clause card),
        targetSpecs :: Map.Map SlotName.SlotName TargetSpec.TargetSpec
      }

`optionality` and `unlessPaid` move down wholesale; nothing is left behind. Today's
`Mode` is exactly a `Mode` with one `Clause`, which is what makes the migration
mechanical rather than semantic.

### `Pawl.Types.ClauseIndex`

    newtype ClauseIndex = MkClauseIndex { unwrap :: Natural.Natural }

A sibling of `ModeIndex`, existing for the same reason: a prompt has to say which clause
is asking.

### `Pawl.Types.Effect`: `RemoveCounters`

    RemoveCounters CounterKind.CounterKind Quantity.Quantity SlotName.SlotName

`PutCounters`' mirror (CR 122), and a separate constructor rather than a signed amount
for the reason `RemovePlayerCounters` already gives against `GainPlayerCounters`: a
signed delta fuses two events that "whenever a counter is put on" text tells apart. Pawl
has **no counter removal at any site today** -- `RemovePlayerCounters` is the player
axis -- so this closes a real gap rather than scaffolding one card.

Asking for more counters than are present removes the ones that are there and no more --
CR 122 states no rule making the instruction fail. `CounterKind.MinusOneMinusOne`
already exists (CR 122.1a), and the P/T consequence stays the projection's (CR 613.4c),
exactly as `PutCounters`' haddock says of the other direction.

### Resolution

Both resolvers -- `Resolve.resolveModes` and the spell path in `Resolve.resolveSpellWith`
-- change from

> per mode: ask the "may", ask the "unless", run the effects

to

> per mode: **per clause**: ask the "may", ask the "unless", run *that clause's* effects.

`applyEffect` is untouched. CR 608.2b's fizzle stays at the mode/spell level, where the
targets are. The spell path's per-effect re-read of live bindings stays per effect.

The existing comment -- "asks them in printed order -- the 'may' first, since a declined
mode has no instruction left for an 'unless' to qualify" -- survives verbatim, one level
down.

**Grouping falls out structurally, with nothing left to check.** One printed "may" over
two instructions is one clause holding two effects, hence one prompt, which is CR
608.2d's single announcement (Renewed Faith, Deem Worthy). Hidden Strings' two adjacent
"may"s are two clauses, hence two prompts. This is the property a per-instruction
`Optionality` wrapper cannot have: no property of an individual instruction distinguishes
"second instruction of one printed may" from "first instruction of a second printed may".

### Prompts

`Prompt.ChooseOptional` and `Prompt.ChooseToPay` each gain a `ClauseIndex` beside their
`ModeIndex`. That is an ABI change reaching `Pawl.Engine.Replay` and the four spec
modules that build these prompts.

A clause whose effects can do nothing is still asked, exactly as an optional mode with
nothing live is today -- that redundant-prompt family is #336, and this change neither
widens nor narrows it. Deciding not to ask by looking at *which* effects a clause holds
would be the closed-half-cases-on-effect-identity failure, so it is not an option.

### Codec

A new `Pawl.Codec.Clause` and its spec; `Pawl.Codec.Mode` encodes `clauses` in place of
`effects`/`optionality`/`unlessPaid`. A new `Pawl.Codec.ClauseIndex` and spec.
`pawl.cabal` regenerated with `cabal-gild pawl.cabal` directly -- six new modules
counting `Pawl.Types.Clause` and `Pawl.Types.ClauseIndex`, and `hooky fix` will not pick
any of them up.

### The card data migration

224 of the 347 card files carry a mode; each `"effects": [...]` becomes
`"clauses": [{"effects": [...]}]`. Five carry `optionality` and four carry `unlessPaid`,
which move inside the single clause. `Common.optionalPair` keeps `clauses` omitted when
empty, exactly as `effects` is today.

This is a scripted edit across 224 files, which is the case `CLAUDE.md`'s "verify a
scripted edit's blast radius" is written for: read the diff stat and confirm the shape
before staging.

### The rest of the blast radius

`Modal.modeEffects` and `Modal.modesEffects` flatten one level further.
`Resolve.slotsOf`, `Resolve.definedSlots` and `Resolve.armedAbilities` walk clauses. The
two CR 612 text-change `rewriteMode` sites (`Projection`, `Resolve`) gain a nested
`fmap`. 50 lint sites in `Pawl.CardSpec`. And the inline `MkMode` mints in `Face`,
`Keyword` (five), `Monarch`, `Battle`, `Rad` and `Speed` each wrap their effects in one
default clause.

Wide and shallow, and `-Werror` finds every site.

## Rejected

- **A per-instruction `Optionality` wrapper**, which is what #335's own comment sketches.
  It has no grouping story, and the issue says so. Hidden Strings prints two identical
  adjacent "may"s that must be two prompts while Renewed Faith prints one "may" over two
  instructions that must be one, and nothing about an individual instruction separates
  those two cases. A gated span cannot get it wrong.
- **A `SkipUnless Gate N` predicated-execution opcode**, which keeps the effect list
  flat. A relative skip count is a jump, and `design.md` section 1 rules it out in as
  many words: "real bytecode has jumps, and yours must not."
- **An `If condition [Effect] [Effect]` arm on `Effect`.** `Pawl.Types.Effect`'s own
  header already rejects it: it puts a branch between two effect lists and makes
  `Resolve.applyEffect` recurse.
- **Reusing `Modal` with a new `ModeSelection` constructor meaning "all of them, in
  order".** Zero new types, and genuinely the cheapest thing that compiles. It makes the
  closed half believe a non-modal card is modal: CR 707.10's copy-with-new-modes,
  `Binding.modes`, and CR 700.2d's repetition (#791, #996) all start answering about
  clauses as though they were modes. That is the closed/open fusion `CLAUDE.md` names as
  the project's single failure mode.
- **A `Gate` sum with an `And` combinator.** See `Pawl.Types.Clause` above.
- **Riders staying on `Mode` with an index range naming which clauses they cover.**
  Smallest diff to the types, but it puts an index into card data that nothing checks,
  and a stale range is a silent miscompile of the card.
- **A codec that also accepts a bare `effects` key as sugar for one default clause**, so
  none of the 195 card files change. `ModeSelection`'s two-constructor split (#997,
  landed 26 commits ago) is the tempting precedent -- "that is what keeps the card data untouched:
  a repeat-free selection still encodes as it always did." It does not transfer. That
  split kept the data untouched by choosing a *type* whose default encoding was already
  right; this would add a *second spelling* of one value to the codec, so two card files
  could represent the same card differently and the lint family could no longer say
  anything about either. It is also a compat shim, which the no-API-stability rule
  forbids outright.

## Verification

- **Gameplay test, proving both branches on one board.** `alice` controls a 2/2 with a
  -1/-1 counter on it -- put there by **Instill Infection** (`{3}{B}` Instant, already in
  the pool: "Put a -1/-1 counter on target creature. Draw a card."), so no synthetic card
  and no hand-built object state. The creature reads 1/1. Cast Shed Weakness on it:

  | answer to the "may" | expected |
  |---|---|
  | declines | **3/3** -- the counter stays, the `+2/+2` happened anyway |
  | exercises | **4/4** -- the counter is gone |

  Under today's mode-wide gate the declining case reads **1/1**, because declining skips
  the pump too. The two branches and the regression are all separated by power and
  toughness alone, with no second observation needed.

- **Mutation, per `CLAUDE.md` step 3.** Widen the clause gate back to mode scope (run
  `exercises` once per mode rather than once per clause) and confirm the declining case
  fails at 1/1 rather than 3/3. Then break `RemoveCounters` to remove nothing and confirm
  the exercising case fails at 3/3 rather than 4/4. Both put back.

- **Codec round trip** for `Clause` and `ClauseIndex` in the usual spec shape, plus the
  existing `Pawl.Codec.ModeSpec` cases rewritten to the new nesting.

- **Suite count before -> after**, and `ormolu --mode check $(git ls-files '*.hs')`
  repo-wide before pushing, since `hooky fix` only formats staged files.

- **Does the diff make the rules core case on an effect's identity?** No. `Resolve` is
  already the one module permitted to case on `Effect`, and a clause's gate is a
  classification read *beside* the effect list: the resolver asks "is this clause
  optional", never "which effect is this". `applyEffect` gains no arm beyond
  `RemoveCounters`' own.

## Out of scope, staying open

- **#703** ("unless" over part of a mode) -- the carrier is free after this change; what
  remains is the card's own cost. Condescend wants `{X}` in a resolution cost (CR
  107.3 / 118.4), and Thirst for Knowledge wants a *filtered* discard cost, where
  `CostComponent` offers only a count.
- **#701** (CR 118.12's positive branch) -- adds a polarity to the clause's resolution
  cost when Standstill lands, which also needs a `PlayerRef` naming the opponents of a
  slot-bound player.
- **#487** (Baral's reflexive "if you do") -- deliberately out. It reads whether an
  earlier clause *happened*, which is dataflow between clauses rather than a gate over a
  choice or over state, and it is the piece that most resembles a jump target.

  **The doubt raised on #487 should be resolved in its favour.** The cluster comment
  wonders whether CR 118.12's "regardless of what events actually occurred" means the
  clause reads the *choice* rather than the outcome. That sentence is scoped to a
  **cost** -- "the action [do something] is a cost, paid when the spell or ability
  resolves" -- and Baral's draw is an effect, not a cost. So CR 118.12 does not reach it
  and the outcome reading stands: #487 really does need per-clause outcomes.

- **#1045**, the `condition` rider -- adapt (CR 701.46a), monstrosity (CR 701.37a) and
  amass (CR 701.47a) from #876, plus #808's third bullet reached from the other
  direction. Adapt is the cheap producer: its condition is already expressible as
  `Quantity.ObjectCounters PlusOnePlusOne` against `Literal 0`, where monstrosity needs a
  *monstrous* designation pawl has no carrier for.
- **#996** is untouched: a clause index is not a slot name, and `Modal.instanceSlot`'s
  printed-name collision is unaffected either way.
