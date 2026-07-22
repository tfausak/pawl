# M4.5 P7 — Player continuous effects, restrictions and cost modification: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give pawl the third clause of CR 611.1 — a continuous effect that "affects players or the rules of the game" — as a `PlayerEffect` axis with one sole casing home, two carriers, and five real gate cards: **Rule of Law**, **Thalia, Guardian of Thraben**, **Sapphire Medallion**, **Reliquary Tower** and **Silence**.

**Architecture:** A new leaf family `Pawl.Type.PlayerEffect`, scoped by `Pawl.Type.PlayerScope` and filtered by `Pawl.Type.SpellCriterion`, riding two carriers: `Card.playerAbilities :: [PlayerStaticAbility]` (printed, re-derived live from the battlefield per CR 604.2) and `GameState.playerEffects :: [ActivePlayerEffect]` (stored by the new `Effect.AffectPlayers` opcode, expiring through P6's existing `Pawl.Expiry` sweeps). A new `Pawl.PlayerEffect` module is the **sole home of `case … PlayerEffect`/`PlayerScope`/`SpellCriterion`** and answers three typed questions — `prohibitsCasting`, `costAdjustments`, `maximumHandSize`. A second new module `Pawl.Cost` owns CR 601.2f's total-cost computation. Six read sites consume them. **`Pawl.Projection`, `Layer`, `Modification` and `Affected` are not touched**: CR 613.10/613.11 put this axis *after* the layer fold, not in it.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

**Spec:** `docs/superpowers/specs/2026-07-22-p7-player-effects-design.md`. Read §0 (the falsifiers) before anything; §2.1–§2.4 before Task 1, §2.7 before Task 2, §2.5 and §2.8 before Task 3, §2.6 before Tasks 4–5, §2.9–§2.10 before Tasks 7–8, §8–§10 before Task 9.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core and in test modules that already carry the pragma. A module that already carries a `{-# LANGUAGE … #-}` pragma keeps it.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents; `A.B.C` must not import `A.B` or `A`. A sibling `Pawl.Type.*` import is fine.
- **Qualified imports aliased to the last component** (`Data.Set` → `Set`, `Pawl.Type.PlayerScope` → `PlayerScope`). One import group, alphabetical. A logic module may import its same-named type module under that alias (`Pawl.Mana` imports `Pawl.Type.Mana` as `Mana`; `Pawl.Expiry` imports `Pawl.Type.Expiry` as `Expiry`) — `Pawl.PlayerEffect` importing `Pawl.Type.PlayerEffect` as `PlayerEffect` is that same established shape. In the **test suite**, where both must be imported at once, the type module takes the `X.Type` alias (`Pawl.Type.Expiry as Expiry.Type` is the standing precedent); `Pawl.Support` is aliased `S`.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches. **`Natural` subtraction is partial** — it throws on underflow. Every subtraction of a `Natural` in this plan is guarded; never remove a guard.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do`/`pure` + record syntax, not `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions. Named local predicates over lambdas in `filter`.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`. `prohibitsCasting :: … -> Bool` is the documented exception the spec mandates (§2.5): it answers one yes/no question with no third state, exactly as `Cast.castable` and `Mana.canPay` already do.
- **Derive at least `Eq` and `Show`.** Every new card-data type here also derives `Ord`, because `Card` derives `Ord` and will carry them. `ActivePlayerEffect` derives `Eq`/`Ord`/`Show` too, matching `ActiveReplacement`.
- **No API stability obligations.** Rename, reshape, and delete freely; never add a compat shim or keep an old name.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules — **including the citations written in this plan**. If `rules.txt` disagrees with a citation below, `rules.txt` wins: fix the citation and say so in the completion note.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Import lists are not spelled out** in every snippet below. When a step's code names `Monad.when`, `Set.*`, `Map.*`, `List.*`, `Maybe.*`, `Foldable.*` or a `Pawl.*` module, add the qualified import (aliased per the rule above, one alphabetical group). GHC names every missing one. Equally, when a step *deletes* the last use of an import, delete the import — `-Wunused-imports` is an error here.
- **New library modules** go under `source/library/` and are picked up by the `-- cabal-gild: discover` directive — add the file and run `hooky fix`; never hand-edit `exposed-modules`. **New test modules** are discovered the same way into the test-suite `other-modules`, and must additionally be wired into `source/test-suite/Main.hs`'s `testTree`.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add -u`, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing. For a task whose first test names a module or constructor that does not exist yet, "fails" means the **build** fails with a specific `Not in scope` / `Could not find module` error — record that as the observed failure, then implement.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.

## Card verification (already done — do not re-fetch)

All five gate cards were fetched live from the Scryfall API during the design pass **and re-verified while this plan was written**. Use these values verbatim; no network access is needed during execution.

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| Rule of Law | `{2}{W}` | Enchantment | — | "Each player can't cast more than one spell each turn." |
| Thalia, Guardian of Thraben | `{1}{W}` | Legendary Creature — Human Soldier | 2/1 | "First strike\nNoncreature spells cost {1} more to cast." |
| Sapphire Medallion | `{2}` | Artifact | — | "Blue spells you cast cost {1} less to cast." |
| Reliquary Tower | — | Land | — | "You have no maximum hand size.\n{T}: Add {C}." |
| Silence | `{W}` | Instant | — | "Your opponents can't cast spells this turn." |

Gatherer rulings, verbatim. **They are the specification of the tests** — do not paraphrase them into test names.

**Rule of Law**
> - *"Rule of Law looks at the entire turn to see if a player has cast a spell, even if Rule of Law wasn't on the battlefield when that spell was cast. Notably, you can't cast Rule of Law and then cast another spell during the same turn."*
> - *"If you cast a spell that was countered, you can't cast another spell during the same turn."*

**Thalia, Guardian of Thraben**
> - *"Thalia's ability affects each spell that's not a creature spell, including your own."*
> - *"To determine the total cost of a spell, start with the mana cost or alternative cost you're paying, add any cost increases, then apply any cost reductions. The mana value of the spell remains unchanged, no matter what the total cost to cast it was."*

**Sapphire Medallion**
> - *"The ability doesn't change the mana cost or mana value of any spell. It changes only the total cost you pay."*
> - *"The ability can't reduce the amount of colored mana you pay for a spell. It reduces only the generic mana component of that cost."*
> - *"If there are additional costs to cast a spell, or if the cost to cast a spell is increased by an effect (such as the one created by Thalia, Guardian of Thraben's ability), apply those increases before applying cost reductions."*
> - *"The cost reduction can apply to alternative costs such as flashback costs."*
> - *"If a spell you cast has {X} in its mana cost, you choose the value of X before calculating the spell's total cost."*

**Silence**
> - *"Silence won't affect spells that your opponents cast before you cast Silence, including any spells that are still on the stack. Silence also won't stop your opponents from casting spells after you cast Silence but before Silence resolves."*
> - *"The only thing Silence stops is casting spells. Your opponents can still activate abilities, including abilities of cards in their hands (like cycling). Their triggered abilities work as normal, they can still play lands, and so on."*
> - *"A player who can't cast a spell can't suspend a card."*

**Reliquary Tower**
> - *"If multiple effects modify your hand size, apply them in timestamp order. For example, if you put Null Profusion (an enchantment that says your maximum hand size is two) onto the battlefield and then put Reliquary Tower onto the battlefield, you'll have no maximum hand size. However, if those permanents enter in the opposite order, your maximum hand size would be two."*

**None of the five is added to any deck in `Pawl.Cards`.** They are deterministic fixtures, like Master Thief and Hag of Inner Weakness — adding them to a deck would perturb `PropertySpec`'s card-backed conservation counts for no gain. They *are* added to the `Cards` record, `loadCards` and `allPrintings`, which the `CardSpec` directory lint requires.

## Four deliberate departures from the spec

State these in the completion note (Task 9). They are refinements, not drift.

1. **`Pawl.Cost` is factored into a stateful `total` and a pure `applyAdjustments`.** The spec's §2.6 gives one entry point, `total :: PlayerId -> ObjectId -> ManaCost -> GameState -> ManaCost`. That signature is kept, but the CR 601.2f/118.7a arithmetic lives in `applyAdjustments :: ([Natural], [Natural]) -> ManaCost -> ManaCost`, which needs no board at all. This is what lets the ordering rule and the generic-only floor be unit-tested directly, rather than only through a two-permanent board.
2. **The total cost is canonicalized: one leading `Generic` symbol, then the printed typed symbols in order, with a zero generic component dropped entirely.** The spec's §2.6 says "append every increase as a `Generic n` symbol", which would make `{2}{U}` taxed by Thalia render as `{2}{U}{1}` — payable but not comparable, and the spec's own order test demands the answer be *exactly* `{U}`. `Mana.spend` sums every generic symbol and matches typed symbols first, so this is presentation only.
3. **`Pawl.PlayerEffect`'s printed gather honours CR 305.7.** The spec does not mention it, but Reliquary Tower is a *nonbasic land* and Blood Moon is in the pool: an ability read straight off the card would survive having its land's subtype set to a basic type, which CR 305.7 forbids. The gather reuses `Projection.liveGiven` with a hoisted `setLandSubtypeEffects`, exactly as `Projection.gather` does. One extra test in Task 6 covers it.
4. **`Pawl.PlayerEffect.applying` grows its second carrier in Task 7 rather than arriving with both.** Introducing `GameState.playerEffects` in Task 3, where no opcode can produce one, would add a field nothing writes. Tasks 3–6 read the printed carrier only; Task 7 adds the stored half to the same function.

## File structure

**New library modules.**

| Module | Task | Responsibility |
|---|---|---|
| `source/library/Pawl/Type/PlayerEffect.hs` | 1 | the leaf family: `CantCastSpells \| CantCastMoreThan \| IncreaseSpellCost \| ReduceSpellCost \| NoMaximumHandSize` |
| `source/library/Pawl/Type/PlayerScope.hs` | 1 | `You \| Opponents \| EachPlayer` |
| `source/library/Pawl/Type/SpellCriterion.hs` | 1 | `NoncreatureSpell \| SpellOfColor Color` |
| `source/library/Pawl/Type/PlayerStaticAbility.hs` | 1 | the printed carrier: `{scope, effect}` |
| `source/library/Pawl/Type/ActivePlayerEffect.hs` | 7 | the stored carrier: `{source, controller, timestamp, expiry, scope, effect}` |
| `source/library/Pawl/PlayerEffect.hs` | 3 | **sole casing home**: `applying`, `inScope`, `prohibitsCasting`, `costAdjustments` (4), `matchesSpell` (4), `maximumHandSize` (6) |
| `source/library/Pawl/Cost.hs` | 4 | CR 601.2f: `total`, `applyAdjustments` |

**New test module.** `source/test-suite/Pawl/PlayerEffectSpec.hs` (Task 3) — near-mirrors `Pawl.PlayerEffect` and `Pawl.Cost`; holds every gate card's gameplay-level tests and the sweep/unit tests.

**New card data.** `data/cards/rule-of-law.json` (3), `data/cards/thalia-guardian-of-thraben.json` (4), `data/cards/sapphire-medallion.json` (5), `data/cards/reliquary-tower.json` (6), `data/cards/silence.json` (8).

**Changed library modules.** `Pawl/Type/Card.hs`, `Pawl/Type/GameEvent.hs`, `Pawl/Type/GameState.hs`, `Pawl/Type/Effect.hs`, `Pawl/Type/Subtype.hs`, `Pawl/Codec.hs`, `Pawl/Event.hs`, `Pawl/Quantity.hs`, `Pawl/Cast.hs`, `Pawl/Engine.hs`, `Pawl/Expiry.hs`, `Pawl/Resolve.hs`, `Pawl/Mana.hs`, `Pawl/Setup.hs`.

**Untouched, deliberately.** `Pawl/Projection.hs` (read from, never edited), `Pawl/Type/Layer.hs`, `Pawl/Type/Modification.hs`, `Pawl/Type/Affected.hs`, `Pawl/Type/ContinuousEffect.hs`, `Pawl/Type/StaticAbility.hs`, `Pawl/Type/Player.hs`, `Pawl/Type/AdditionalCost.hs`, `Pawl/Type/AbilityCost.hs`, `Pawl/Replacement.hs`.

---

### Task 1: The vocabulary and the printed carrier

Card data can *say* a player effect. Nothing reads one yet. Behaviour-neutral: every existing test and every committed card file must be unchanged, byte for byte.

**Files:**
- Create: `source/library/Pawl/Type/PlayerEffect.hs`, `source/library/Pawl/Type/PlayerScope.hs`, `source/library/Pawl/Type/SpellCriterion.hs`, `source/library/Pawl/Type/PlayerStaticAbility.hs`
- Modify: `source/library/Pawl/Type/Card.hs` (new `playerAbilities` field)
- Modify: `source/library/Pawl/Codec.hs` (four codec pairs, `listFromDefault`, the two `Card` arms)
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `PlayerEffect.PlayerEffect = CantCastSpells | CantCastMoreThan Natural | IncreaseSpellCost SpellCriterion Natural | ReduceSpellCost SpellCriterion Natural | NoMaximumHandSize`; `PlayerScope.PlayerScope = You | Opponents | EachPlayer`; `SpellCriterion.SpellCriterion = NoncreatureSpell | SpellOfColor Color`; `PlayerStaticAbility.MkPlayerStaticAbility {scope :: PlayerScope, effect :: PlayerEffect}`; `Card.playerAbilities :: [PlayerStaticAbility]`; `Codec.playerEffectToJson`/`jsonToPlayerEffect`, `playerScopeToJson`/`jsonToPlayerScope`, `spellCriterionToJson`/`jsonToSpellCriterion`, `playerStaticAbilityToJson`/`jsonToPlayerStaticAbility`, `listFromDefault`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CodecSpec.hs`, add a new group to `tests`'s list (after the `"effect"` group), and register the needed imports (`Pawl.Type.PlayerEffect as PlayerEffect`, `Pawl.Type.PlayerScope as PlayerScope`, `Pawl.Type.PlayerStaticAbility as PlayerStaticAbility`, `Pawl.Type.SpellCriterion as SpellCriterion`, `Pawl.Type.Printing as Printing` — already there):

```haskell
      Tasty.testGroup
        "player effects (P7)"
        [ HU.testCase "every PlayerScope round-trips" $
            mapM_
              (roundTrip "scope" Codec.playerScopeToJson Codec.jsonToPlayerScope)
              [PlayerScope.You, PlayerScope.Opponents, PlayerScope.EachPlayer],
          HU.testCase "every SpellCriterion round-trips" $
            mapM_
              (roundTrip "criterion" Codec.spellCriterionToJson Codec.jsonToSpellCriterion)
              [SpellCriterion.NoncreatureSpell, SpellCriterion.SpellOfColor Color.Blue],
          HU.testCase "every PlayerEffect round-trips" $
            mapM_
              (roundTrip "effect" Codec.playerEffectToJson Codec.jsonToPlayerEffect)
              [ PlayerEffect.CantCastSpells,
                PlayerEffect.CantCastMoreThan 1,
                PlayerEffect.IncreaseSpellCost SpellCriterion.NoncreatureSpell 1,
                PlayerEffect.ReduceSpellCost (SpellCriterion.SpellOfColor Color.Blue) 1,
                PlayerEffect.NoMaximumHandSize
              ],
          HU.testCase "PlayerStaticAbility round-trips" $
            roundTrip
              "ability"
              Codec.playerStaticAbilityToJson
              Codec.jsonToPlayerStaticAbility
              (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)),
          HU.testCase "a Card carrying player abilities round-trips" $
            let base = Printing.card (Cards.bloodMoonPrinting cards)
                c = base {CardT.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]}
             in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The same posture
          -- colorIndicator and delayedAbilities already take.
          HU.testCase "an empty playerAbilities list is omitted from the JSON" $
            let base = Printing.card (Cards.bloodMoonPrinting cards)
             in do
                  HU.assertEqual "the fixture really has none" [] (CardT.playerAbilities base)
                  case J.asObject (Codec.cardToJson base) of
                    Left err -> HU.assertFailure (Text.unpack err)
                    Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "playerAbilities") (map fst pairs))
        ],
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Type.PlayerEffect'` (and the three siblings).

- [ ] **Step 3: Create the four types**

`source/library/Pawl/Type/SpellCriterion.hs`:

```haskell
module Pawl.Type.SpellCriterion where

import Pawl.Type.Color (Color)

-- Which spells a cost-modifying continuous effect applies to (CR 613.11). The
-- third sibling of Pawl.Type.CardCriterion and Pawl.Type.PermanentCriterion,
-- deliberately NOT merged with either: P9 merges all of them into one filter
-- language, and merging two of them here would be building half of P9 with one
-- customer (#38/#39/#40 are the same deferral for the other three).
--
-- Both inhabitants are evaluated against the PROJECTION by
-- Pawl.PlayerEffect.matchesSpell, never against a printed characteristic: a card
-- type is CR 613 layer 4 and a colour is layer 5, so Blood Moon and a colour
-- changer both change the answer.
data SpellCriterion
  = -- Thalia, Guardian of Thraben: "Noncreature spells cost {1} more to cast."
    NoncreatureSpell
  | -- Sapphire Medallion: "Blue spells you cast cost {1} less to cast."
    SpellOfColor Color
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/PlayerEffect.hs`:

```haskell
module Pawl.Type.PlayerEffect where

import Numeric.Natural (Natural)
import Pawl.Type.SpellCriterion (SpellCriterion)

-- CR 611.1's third clause: a continuous effect that "affects players or the
-- rules of the game" rather than the characteristics of an object. The player
-- analogue of Pawl.Type.Modification, and NOT a member of it: CR 613.1 makes the
-- seven layers a machine for computing an OBJECT's characteristics, while CR
-- 613.10 and 613.11 apply these AFTER that machine has run. There is no Layer
-- constructor here and Pawl.Projection never sees this type.
--
-- Open-half card data. Pawl.PlayerEffect is the ONLY module that may case on it.
data PlayerEffect
  = -- CR 601.3 / Silence: this player can't cast spells at all.
    CantCastSpells
  | -- CR 601.3 / Rule of Law: this player can't cast more than this many spells
    -- each turn. The limit is carried, not hardcoded: Rule of Law and Arcane
    -- Laboratory both say one, and a card that says two must not need a sibling
    -- constructor.
    CantCastMoreThan Natural
  | -- CR 613.11 / 601.2f / Thalia: matching spells cost this much more generic
    -- mana to cast.
    IncreaseSpellCost SpellCriterion Natural
  | -- CR 613.11 / 601.2f / Sapphire Medallion: matching spells cost this much
    -- less to cast.
    --
    -- A SEPARATE constructor from IncreaseSpellCost, never one signed delta. The
    -- rules distinguish them in two ways a signed integer cannot express: CR
    -- 601.2f applies every increase BEFORE any reduction, and CR 118.7a gives a
    -- reduction a restriction an increase does not have -- it "can't affect the
    -- colored or colorless mana components". Collapsing them would put both
    -- rules into arithmetic that cannot state either.
    ReduceSpellCost SpellCriterion Natural
  | -- CR 402.2 / Reliquary Tower: this player has no maximum hand size.
    NoMaximumHandSize
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/PlayerScope.hs`:

```haskell
module Pawl.Type.PlayerScope where

-- Which players a Pawl.Type.PlayerEffect applies to: the player-side analogue of
-- Pawl.Type.Affected, and much smaller because CR 109.5 fixes what "you" means.
-- Resolved against the effect's CONTROLLER by Pawl.PlayerEffect.inScope.
--
-- The scope is always resolved DYNAMICALLY, on both carriers. CR 611.2c's first
-- sentence freezes a stored effect's object set -- which is what every stored
-- ContinuousEffect does (Affected.TheseObjects) -- but its third sentence carves
-- out exactly this axis: "A continuous effect generated by the resolution of a
-- spell or ability that doesn't modify the characteristics or change the
-- controller of any objects modifies the rules of the game, so it can affect
-- objects that weren't affected when that continuous effect began." So there is
-- no stored-set analogue of TheseObjects here and no one-way door: this is the
-- same type on the printed and the stored carrier. Freeze it and Silence, which
-- resolves with no opponent spell on the stack, does literally nothing.
data PlayerScope
  = -- CR 109.5: the effect's controller.
    You
  | -- CR 102.1: every other player in the game.
    Opponents
  | -- Every player, the controller included ("including your own", Thalia's own
    -- ruling).
    EachPlayer
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/PlayerStaticAbility.hs`:

```haskell
module Pawl.Type.PlayerStaticAbility where

import Pawl.Type.PlayerEffect (PlayerEffect)
import Pawl.Type.PlayerScope (PlayerScope)

-- A card's printed player/rules-modifying static ability (CR 604.1/604.2: a
-- static ability creates a continuous effect active while its permanent is on
-- the battlefield). The player-axis sibling of Pawl.Type.StaticAbility, whose
-- Affected/Modification pair this mirrors with a PlayerScope/PlayerEffect pair.
--
-- Gathered LIVE from every battlefield permanent by Pawl.PlayerEffect.applying on
-- every read, never captured -- so Rule of Law leaving the battlefield lifts its
-- restriction with nothing to unwind. Rule of Law, Thalia, Sapphire Medallion and
-- Reliquary Tower each declare exactly one.
data PlayerStaticAbility = MkPlayerStaticAbility
  { scope :: PlayerScope,
    effect :: PlayerEffect
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the `Card` field**

In `source/library/Pawl/Type/Card.hs`, add the import of `Pawl.Type.PlayerStaticAbility (PlayerStaticAbility)` and append this field to the record, after `castingPermissions`:

```haskell
    -- CR 604.1/604.2 / 611.1: this card's printed PLAYER and RULES-modifying
    -- static abilities (Rule of Law, Thalia, Sapphire Medallion, Reliquary
    -- Tower). The sibling of staticAbilities on the axis CR 613.10/613.11 put
    -- OUTSIDE the layer system, so these are read by Pawl.PlayerEffect and never
    -- by Pawl.Projection. Empty for every other printing.
    playerAbilities :: [PlayerStaticAbility]
```

Remember the comma after `castingPermissions :: [CastingPermission]`.

- [ ] **Step 5: Add the codecs**

In `source/library/Pawl/Codec.hs`, add the four pairs. Put `playerScopeToJson`/`jsonToPlayerScope` and `spellCriterionToJson`/`jsonToSpellCriterion` next to `permanentCriterionToJson` (the leaf-enum neighbourhood, around line 447), `playerEffectToJson`/`jsonToPlayerEffect` after them, and `playerStaticAbilityToJson`/`jsonToPlayerStaticAbility` beside `staticAbilityToJson` (around line 1096).

```haskell
playerScopeToJson :: PlayerScope.PlayerScope -> Value
playerScopeToJson s = nullary . Text.pack $ case s of
  PlayerScope.You -> "You"
  PlayerScope.Opponents -> "Opponents"
  PlayerScope.EachPlayer -> "EachPlayer"

jsonToPlayerScope :: Value -> Either Text PlayerScope.PlayerScope
jsonToPlayerScope =
  decodeNullary
    (Text.pack "PlayerScope")
    [ (Text.pack "You", PlayerScope.You),
      (Text.pack "Opponents", PlayerScope.Opponents),
      (Text.pack "EachPlayer", PlayerScope.EachPlayer)
    ]

spellCriterionToJson :: SpellCriterion.SpellCriterion -> Value
spellCriterionToJson c = case c of
  SpellCriterion.NoncreatureSpell -> nullary (Text.pack "NoncreatureSpell")
  SpellCriterion.SpellOfColor color -> Json.tagged (Text.pack "SpellOfColor") (Just (colorToJson color))

jsonToSpellCriterion :: Value -> Either Text SpellCriterion.SpellCriterion
jsonToSpellCriterion value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("NoncreatureSpell", _) -> Right SpellCriterion.NoncreatureSpell
    ("SpellOfColor", Just v) -> SpellCriterion.SpellOfColor <$> jsonToColor v
    _ -> Left (Text.pack "unknown SpellCriterion: " <> t)

playerEffectToJson :: PlayerEffect.PlayerEffect -> Value
playerEffectToJson e = case e of
  PlayerEffect.CantCastSpells -> nullary (Text.pack "CantCastSpells")
  PlayerEffect.CantCastMoreThan n -> Json.tagged (Text.pack "CantCastMoreThan") (Just (natTo n))
  PlayerEffect.IncreaseSpellCost c n -> Json.tagged (Text.pack "IncreaseSpellCost") (Just (Array [spellCriterionToJson c, natTo n]))
  PlayerEffect.ReduceSpellCost c n -> Json.tagged (Text.pack "ReduceSpellCost") (Just (Array [spellCriterionToJson c, natTo n]))
  PlayerEffect.NoMaximumHandSize -> nullary (Text.pack "NoMaximumHandSize")

jsonToPlayerEffect :: Value -> Either Text PlayerEffect.PlayerEffect
jsonToPlayerEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> natFrom v
    ("IncreaseSpellCost", Just (Array [c, n])) -> PlayerEffect.IncreaseSpellCost <$> jsonToSpellCriterion c <*> natFrom n
    ("ReduceSpellCost", Just (Array [c, n])) -> PlayerEffect.ReduceSpellCost <$> jsonToSpellCriterion c <*> natFrom n
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    _ -> Left (Text.pack "unknown PlayerEffect: " <> t)

playerStaticAbilityToJson :: PlayerStaticAbility.PlayerStaticAbility -> Value
playerStaticAbilityToJson pa =
  Object
    [ (Text.pack "scope", playerScopeToJson (PlayerStaticAbility.scope pa)),
      (Text.pack "effect", playerEffectToJson (PlayerStaticAbility.effect pa))
    ]

jsonToPlayerStaticAbility :: Value -> Either Text PlayerStaticAbility.PlayerStaticAbility
jsonToPlayerStaticAbility value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "scope") ps >>= jsonToPlayerScope
  e <- Json.field (Text.pack "effect") ps >>= jsonToPlayerEffect
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)
```

- [ ] **Step 6: Wire the field through the `Card` codec, omitted when empty**

Beside `setFromDefault` (around line 1404) add its list sibling:

```haskell
-- An omitted list field decodes to empty, the list counterpart of
-- setFromDefault. Lets an all-default field stay OUT of the committed JSON, so
-- every existing card file remains byte-identical.
listFromDefault :: (Value -> Either Text a) -> Value -> Either Text [a]
listFromDefault f value = case value of
  Null -> Right []
  _ -> listFrom f value
```

In `cardToJson`, append a fourth optional block after the `delayedAbilities` one:

```haskell
        ++ ( if null (CardT.playerAbilities c)
               then []
               else [(Text.pack "playerAbilities", listTo playerStaticAbilityToJson (CardT.playerAbilities c))]
           )
```

In `jsonToCard`, add the read and the field:

```haskell
  playerAbilities <- listFromDefault jsonToPlayerStaticAbility (getOpt (Text.pack "playerAbilities") ps)
```

```haskell
        CardT.playerAbilities = playerAbilities
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the new `player effects (P7)` group green, and **every pre-existing test still green**, `CardsSpec`'s byte-stability check included. A byte-stability failure means the field is being emitted when empty: fix `cardToJson`, never the assertion.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl source/test-suite pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): PlayerEffect, PlayerScope, SpellCriterion and the printed carrier

CR 611.1's third clause as card data: a continuous effect that affects
players or the rules of the game, scoped by PlayerScope and carried on
Card.playerAbilities. No reader yet. The JSON field is omitted when
empty, so every committed card file stays byte-identical.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: `GameEvent.SpellCast` — the `P4 → P7` edge

The event kind Rule of Law counts. The umbrella predicted this edge ("each later phase adds the event kind it reads — `SpellCast` at **P7**").

**Files:**
- Modify: `source/library/Pawl/Type/GameEvent.hs` (new constructor)
- Modify: `source/library/Pawl/Event.hs` (new `castOf`; `movedOf`, `damageOf` and both inner cases of `matchesTrigger` gain an arm)
- Modify: `source/library/Pawl/Quantity.hs:88` (`died` gains an arm)
- Modify: `source/library/Pawl/Codec.hs:975-988` (`gameEventToJson`/`jsonToGameEvent`)
- Modify: `source/library/Pawl/Cast.hs` (`castSpell` emits it)
- Test: `source/test-suite/Pawl/CastSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: `GameEvent.SpellCast :: PlayerId -> GameEvent`; `Event.castOf :: GameEvent -> Maybe PlayerId`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CastSpec.hs`, add these cases to the module's top-level `tests` group (add imports for `Pawl.Event as Event`, `Pawl.Type.GameEvent as GameEvent`, `Data.Foldable as Foldable`, `Data.Maybe as Maybe` as needed):

```haskell
      HU.testCase "CR 601.2i casting a spell records a SpellCast event for the caster" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid)
            casts = Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after))
         in do
              HU.assertEqual "no cast before" [] (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))
              HU.assertEqual "exactly one cast, by alice" [S.alice] casts,
      HU.testCase "CR 601.2i a cast that is rejected records nothing" $
        -- A Bolt with no mana available: legalActions would never offer it, and
        -- castSpell's payment fails, so no event is recorded.
        let (gs, oid) = S.boltInHand cards 0 Phase.PrecombatMain
            after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid)
         in HU.assertEqual "no cast recorded" [] (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after))),
```

In `source/test-suite/Pawl/CodecSpec.hs`, add to the group that already round-trips `GameEvent` values (search for `gameEventToJson`; if there is none, add this case to the `"records"` group):

```haskell
          HU.testCase "GameEvent.SpellCast round-trips" $
            roundTrip "ev" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.SpellCast S.alice),
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Data constructor not in scope: GameEvent.SpellCast`, `Variable not in scope: Event.castOf`.

- [ ] **Step 3: Add the constructor**

In `source/library/Pawl/Type/GameEvent.hs`, append:

```haskell
  | -- CR 601.2i: a player cast a spell. The event Rule of Law counts, and the
    -- reason the count is a fold over P4's whole turn log rather than a
    -- per-effect watermark: its ruling looks at "the entire turn ... even if
    -- Rule of Law wasn't on the battlefield when that spell was cast".
    --
    -- The CAST is the event, not the resolution -- its second ruling ("If you
    -- cast a spell that was countered, you can't cast another spell during the
    -- same turn") is what fixes that.
    SpellCast PlayerId
```

- [ ] **Step 4: Add `castOf` and the four new arms in `Pawl.Event`**

In `source/library/Pawl/Event.hs`, add beside `damageOf`:

```haskell
-- The caster an event describes, if it is a cast (CR 601.2i).
castOf :: GameEvent -> Maybe PlayerId
castOf event = case event of
  GameEvent.SpellCast pid -> Just pid
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
```

Add `GameEvent.SpellCast _ -> Nothing` to `movedOf` (line 65) and to `damageOf` (line 72). Add `GameEvent.SpellCast _ -> False` to both inner cases of `matchesTrigger` — the `SelfEnters` arm (line 338) and the `StepBegins` arm (line 346).

In `source/library/Pawl/Quantity.hs`, add `GameEvent.SpellCast _ -> False` to `died` (line 89).

- [ ] **Step 5: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add to `gameEventToJson`:

```haskell
  GameEvent.SpellCast pid -> Json.tagged (Text.pack "SpellCast") (Just (playerIdToJson pid))
```

and to `jsonToGameEvent`'s case:

```haskell
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> jsonToPlayerId v
```

- [ ] **Step 6: Emit it from `Cast.castSpell`**

In `source/library/Pawl/Cast.hs`, inside the `Just paid -> do` branch (line 201), insert the emission between `Event.changeZone` and `moved <- State.get`:

```haskell
              Just paid -> do
                State.put paid
                Event.changeZone oid Zone.Stack
                -- CR 601.2i: the spell has been cast. Emitted here, AFTER the
                -- last step that can fail, so a rejected announcement records
                -- nothing. Rule of Law counts this event and not the
                -- resolution, so a countered spell still counted (its second
                -- Gatherer ruling).
                State.modify' (Event.recordEvent (GameEvent.SpellCast pid))
                moved <- State.get
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — both `CastSpec` cases and the codec round-trip green, everything else unchanged. `PropertySpec`'s conservation properties must still hold: a `SpellCast` entry in the log is not an object and moves nothing.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): GameEvent.SpellCast, the P4 -> P7 edge

CR 601.2i: Cast.castSpell records the cast after the last failing step,
so a rejected announcement records nothing. The CAST is the event, not
the resolution -- which is what makes a countered spell still count.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: `Pawl.PlayerEffect`, the casting gate, and Rule of Law

The sole casing home arrives with its first question, its first two read sites, and its first gate card.

**Files:**
- Create: `source/library/Pawl/PlayerEffect.hs`
- Create: `source/test-suite/Pawl/PlayerEffectSpec.hs`
- Create: `data/cards/rule-of-law.json`
- Modify: `source/library/Pawl/Cast.hs:84-94` (`castable`), `:111-118` (`castableWhileSearching`)
- Modify: `source/test-suite/Pawl/Cards.hs` (record field, `loadCards`, `allPrintings`), `source/test-suite/Main.hs` (wire the spec)
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `PlayerEffect.PlayerEffect`, `PlayerScope.PlayerScope`, `PlayerStaticAbility.{scope,effect}`, `Card.playerAbilities` (Task 1); `Event.castOf`, `GameEvent.SpellCast` (Task 2).
- Produces: `Pawl.PlayerEffect.applying :: PlayerId -> GameState -> [PlayerEffect]`; `Pawl.PlayerEffect.inScope :: PlayerId -> PlayerId -> PlayerScope -> Bool`; `Pawl.PlayerEffect.prohibitsCasting :: PlayerId -> GameState -> Bool`; `Cards.ruleOfLawPrinting :: Cards -> Printing`.

- [ ] **Step 1: Write the failing tests**

Create `source/test-suite/Pawl/PlayerEffectSpec.hs`. Note the dual alias: the logic module is `PlayerEffect`, the type module is `PlayerEffect.Type` (the `Expiry.Type` precedent).

```haskell
-- Covers Pawl.PlayerEffect and Pawl.Cost, plus the four types they case on
-- (Pawl.Type.PlayerEffect, PlayerScope, SpellCriterion, PlayerStaticAbility) and
-- the stored carrier Pawl.Type.ActivePlayerEffect. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The five gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Reliquary Tower and Silence.
module Pawl.PlayerEffectSpec where

import qualified Pawl.Action as Action
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Rule of Law {2}{W} Enchantment: "Each player can't cast more than one spell
-- each turn." alice has nine untapped Plains (mana is never the reason a cast is
-- unavailable) and three Rule of Law cards in hand, in her own precombat main
-- phase with an empty stack.
ruleOfLawBoard :: Cards.Cards -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ruleOfLawBoard cards =
  let base = S.landsInPlay (Cards.plainsPrinting cards) 9
      (a, gs1) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice base
      (b, gs2) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs1
      (c, gs3) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs2
   in ( a,
        b,
        c,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

ruleOfLawTests :: Cards.Cards -> Tasty.TestTree
ruleOfLawTests cards =
  let resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      (a, b, _, board) = ruleOfLawBoard cards
      -- Cast Rule of Law itself, and let it resolve onto the battlefield.
      afterFirst = resolveAll (S.runPure S.identityAnswer board (Cast.castSpell S.alice a))
   in Tasty.testGroup
        "RuleOfLaw"
        [ HU.testCase "before any spell is cast, both cards are castable" $
            do
              HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice board))
              HU.assertBool "a offered" (elem (Action.Type.Cast a) (Action.legalActions S.alice board))
              HU.assertBool "b offered" (elem (Action.Type.Cast b) (Action.legalActions S.alice board)),
          -- Ruling: "Rule of Law looks at the entire turn to see if a player has
          -- cast a spell, even if Rule of Law wasn't on the battlefield when that
          -- spell was cast. Notably, you can't cast Rule of Law and then cast
          -- another spell during the same turn." THE FALSIFIER: the spell that
          -- used up the allowance is Rule of Law itself, cast BEFORE the effect
          -- existed. Any per-effect watermark or counter fails here.
          HU.testCase "CR 601.3 casting Rule of Law itself uses up the turn's one spell" $
            do
              HU.assertBool "alice is now prohibited" (PlayerEffect.prohibitsCasting S.alice afterFirst)
              HU.assertEqual
                "no cast is offered at all"
                []
                (filter isCast (Action.legalActions S.alice afterFirst)),
          -- The limit is counted PER PLAYER: bob has cast nothing this turn, so
          -- EachPlayer does not prohibit him.
          HU.testCase "CR 611.1 the EachPlayer scope still counts each player's own casts" $
            HU.assertBool "bob is not prohibited" (not (PlayerEffect.prohibitsCasting S.bob afterFirst)),
          -- CR 608.2i: the log is cleared at turn handoff, so "this turn" is
          -- exactly the log's own extent.
          HU.testCase "CR 608.2i the restriction lifts on the next turn" $
            let handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
                nextOwnTurn =
                  (handoff (handoff afterFirst))
                    { GameState.phase = Phase.PrecombatMain,
                      GameState.priority = Just S.alice
                    }
             in do
                  HU.assertEqual "alice is active again" S.alice (GameState.activePlayer nextOwnTurn)
                  HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice nextOwnTurn))
                  HU.assertBool "b offered again" (elem (Action.Type.Cast b) (Action.legalActions S.alice nextOwnTurn)),
          -- Ruling: "If you cast a spell that was countered, you can't cast
          -- another spell during the same turn." The counted event is the CAST.
          HU.testCase "CR 601.2i a countered spell still counted" $
            let (_, x, _, plain) = ruleOfLawBoard cards
                onBoard = snd (S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain)
                cast = S.runPure S.identityAnswer onBoard (Cast.castSpell S.alice x)
             in case GameState.stack cast of
                  [] -> HU.assertFailure "expected the spell on the stack"
                  top : _ ->
                    let countered = S.runPure S.identityAnswer cast (Event.counter top)
                     in do
                          HU.assertEqual "the stack is empty again" [] (GameState.stack countered)
                          HU.assertBool "still prohibited" (PlayerEffect.prohibitsCasting S.alice countered),
          -- The effect is RE-DERIVED from the battlefield on every read, so there
          -- is no stored state to unwind when its source leaves.
          HU.testCase "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $
            let (_, _, z, plain) = ruleOfLawBoard cards
                (rol, onBoard) = S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain
                castOne = S.withEvent (GameEvent.SpellCast S.alice) onBoard
                gone = S.runPure S.identityAnswer castOne (Event.destroy rol)
             in do
                  HU.assertBool "prohibited while it stands" (PlayerEffect.prohibitsCasting S.alice castOne)
                  HU.assertBool "not prohibited once it is gone" (not (PlayerEffect.prohibitsCasting S.alice gone))
                  HU.assertBool "and a cast is offered again" (elem (Action.Type.Cast z) (Action.legalActions S.alice gone))
        ]

isCast :: Action.Type.Action -> Bool
isCast action = case action of
  Action.Type.Cast _ -> True
  Action.Type.Play _ -> False
  Action.Type.Activate _ _ -> False
  Action.Type.Pass -> False

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.PlayerEffectSpec" [ruleOfLawTests cards]
```

No new `Support` helper is needed: `S.landsInPlay`, `S.addHandCard`, `S.addCreature`, `S.withEvent` and `S.runPure` all already exist, and the one hand-built event is `GameEvent.SpellCast S.alice` written directly.

Wire the spec into `source/test-suite/Main.hs`: add `import qualified Pawl.PlayerEffectSpec as PlayerEffectSpec` in alphabetical position (after `PowerToughnessSpec`), and `PlayerEffectSpec.tests cards,` into `testTree` (after `PowerToughnessSpec.tests cards,`).

In `source/test-suite/Pawl/CardSpec.hs`, add a new group `m45p7CardTests` (registered in `tests`) with:

```haskell
m45p7CardTests :: Cards.Cards -> Tasty.TestTree
m45p7CardTests cards =
  Tasty.testGroup
    "M4.5 P7"
    [ HU.testCase "Rule of Law is a {2}{W} enchantment with one EachPlayer CantCastMoreThan 1 player ability" $
        let c = Printing.card (Cards.ruleOfLawPrinting cards)
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
         in do
              HU.assertEqual "name" (Text.pack "Rule of Law") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white])) (Card.Type.manaCost c)
              HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
              HU.assertEqual "no object-axis static abilities" [] (Card.Type.staticAbilities c)
              HU.assertEqual
                "one player ability"
                [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)]
                (Card.Type.playerAbilities c)
    ]
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.PlayerEffect'`, `Variable not in scope: Cards.ruleOfLawPrinting`.

- [ ] **Step 3: Create `Pawl.PlayerEffect`**

`source/library/Pawl/PlayerEffect.hs`:

```haskell
-- CR 613.10 / 613.11: the continuous effects that affect PLAYERS and the RULES
-- OF THE GAME rather than the characteristics of objects. A sibling TIER to the
-- CR 613 layer system, not a layer in it: CR 613.1 opens "the values of an
-- object's characteristics are determined by starting with the actual object",
-- so the seven layers are a machine for computing object characteristics and
-- nothing else, and 613.10/613.11 both apply AFTER that machine has run.
-- Pawl.Projection is untouched by this module and never sees these types.
--
-- This module is the ONLY module that may case on Pawl.Type.PlayerEffect,
-- Pawl.Type.PlayerScope or Pawl.Type.SpellCriterion -- the standing Pawl.Resolve
-- has over Effect, Pawl.Projection over Modification, Pawl.Event over
-- TriggerCondition and Pawl.Expiry over Expiry. Every consumer asks a TYPED
-- QUESTION and never sees a constructor.
module Pawl.PlayerEffect where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.Card as Card
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.PlayerEffect (PlayerEffect)
import qualified Pawl.Type.PlayerEffect as PlayerEffect
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.PlayerScope (PlayerScope)
import qualified Pawl.Type.PlayerScope as PlayerScope
import qualified Pawl.Type.PlayerStaticAbility as PlayerStaticAbility

-- CR 109.5: "the words 'you' and 'your' on an object refer to the object's
-- controller ... for a static ability, this is the current controller of the
-- object it's on". `pid` is the player being asked about; `controller` is the
-- effect's controller. The argument order is (asked-about, effect's) and the two
-- are never interchangeable.
inScope :: PlayerId -> PlayerId -> PlayerScope -> Bool
inScope pid controller scope = case scope of
  PlayerScope.You -> pid == controller
  -- CR 102.1: a player's opponents are the other players in the game.
  PlayerScope.Opponents -> pid /= controller
  -- Thalia's ruling: "including your own".
  PlayerScope.EachPlayer -> True

-- CR 604.2: every player effect applying to `pid` right now. Gathered LIVE from
-- the battlefield on every read and never captured, the same posture
-- Projection.gather takes for staticAbilities -- which is why Rule of Law
-- leaving the battlefield lifts its restriction with nothing to unwind.
--
-- The scope is resolved DYNAMICALLY (see Pawl.Type.PlayerScope): CR 611.2c
-- classifies a rules-modifying effect as one that "can affect objects that
-- weren't affected when that continuous effect began", so no set is ever frozen
-- on this axis.
--
-- The (controller, scope, effect) triples are local: nothing outside this
-- function ever sees one.
applying :: PlayerId -> GameState -> [PlayerEffect]
applying pid gs =
  let -- Hoisted out of the walk exactly as Projection.gather hoists it: an
      -- inlined call would recompute the whole game's SetLandSubtype list once
      -- per permanent.
      setEffs = Projection.setLandSubtypeEffects gs
      fromPermanent oid = case Game.cardOf oid gs of
        Nothing -> []
        Just card -> case Card.playerAbilities card of
          -- The overwhelming majority of permanents: no ability, so no
          -- controller projection and no CR 305.7 check is paid for.
          [] -> []
          abilities -> case Projection.controllerOf oid gs of
            Nothing -> []
            Just controller ->
              -- CR 305.7: a land whose subtype has been SET to a basic type
              -- loses its rules-text abilities, this one included (Blood Moon on
              -- Reliquary Tower).
              if null setEffs || Projection.liveGiven setEffs Set.empty oid gs
                then map (\ability -> (controller, PlayerStaticAbility.scope ability, PlayerStaticAbility.effect ability)) abilities
                else []
      printed = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
      keep (controller, scope, _) = inScope pid controller scope
      effectOf (_, _, effect) = effect
   in map effectOf (filter keep printed)

-- CR 601.2i: how many spells this player has cast this turn. A fold over P4's
-- whole log, which is exactly "this turn" because Engine.handoffTurn clears it at
-- the handoff and no reader ever drains it (scannedThrough is a watermark, not a
-- consumption). Rule of Law's ruling demands precisely this: "looks at the entire
-- turn ... even if Rule of Law wasn't on the battlefield when that spell was
-- cast."
castsThisTurn :: PlayerId -> GameState -> Integer
castsThisTurn pid gs =
  let mine caster = caster == pid
   in toInteger (length (filter mine (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))))

-- CR 601.3: "a player can begin to cast a spell only if ... no rule or effect
-- prohibits that player from casting it". The prohibit half; Cast
-- .permitsCastWhileSearching is the allow half.
--
-- CR 101.2 is why this folds as a DISJUNCTION: "When a rule or effect allows or
-- directs something to happen, and another effect states that it can't happen,
-- the 'can't' effect takes precedence." One applicable prohibition is enough and
-- nothing outvotes it.
--
-- Deliberately does NOT take the spell. Both of P7's prohibitions are
-- quality-free -- "can't cast spells", "can't cast more than one spell" -- so the
-- answer does not depend on WHICH spell, and a parameter nothing reads would
-- assert a generality this phase has not built. It grows an ObjectId when CR
-- 601.3a's quality-bearing prohibitions do (#N).
prohibitsCasting :: PlayerId -> GameState -> Bool
prohibitsCasting pid gs =
  let cast = castsThisTurn pid gs
      prohibits effect = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantCastMoreThan limit -> cast >= toInteger limit
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
   in any prohibits (applying pid gs)
```

- [ ] **Step 4: Gate both casting sites**

In `source/library/Pawl/Cast.hs`, `castable` becomes:

```haskell
-- Affordable and correctly timed, actually in this player's hand, fillable, and
-- not prohibited.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs = case costOf oid gs of
  Nothing -> False
  Just cost ->
    timingOk pid oid gs
      && elem oid (Game.zoneMembers Zone.Hand pid gs)
      -- CR 601.3: no rule or effect prohibits this player from casting a spell
      -- (Rule of Law, Silence). Gated HERE, upstream of Action.legalActions,
      -- because the engine never offers an illegal action and then rejects it.
      && not (PlayerEffect.prohibitsCasting pid gs)
      -- CR 601.2b: a {X} cost is affordable when payable at X=0 (the caster may
      -- always choose X=0); substituteX 0 is the identity on any Variable-free
      -- cost, so every existing card is unaffected.
      && Mana.canPay pid (Mana.substituteX 0 cost) gs
      && targetable oid gs
```

and `castableWhileSearching` gains the same gate:

```haskell
-- The library cards this player may cast while searching their own library:
-- permitted, not prohibited, affordable (Mana.canPay), and with a fillable target
-- set (Cast.targetable). Deliberately omits timingOk -- the permission IS the CR
-- 601.3 timing exception (the ruling: "follows all normal rules ... except for
-- timing").
--
-- The prohibition is NOT omitted, and that is the point: CR 601.3 is one
-- sentence with two halves, and the Panglacial permission excepts only the
-- timing one. A Rule of Law still stops a cast from the library.
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = case costOf oid gs of
        Nothing -> False
        -- CR 601.2b castability floor: payable at X=0 (see Cast.castable).
        Just cost -> Mana.canPay pid (Mana.substituteX 0 cost) gs
      allowed oid = permitted oid && affordable oid && targetable oid gs
   in if PlayerEffect.prohibitsCasting pid gs
        then []
        else filter allowed (Game.zoneMembers Zone.Library pid gs)
```

Add `import qualified Pawl.PlayerEffect as PlayerEffect` to `Pawl.Cast`.

- [ ] **Step 5: Write the card file**

Create `data/cards/rule-of-law.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Rule of Law","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Enchantment"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"playerAbilities":[{"scope":{"type":"EachPlayer"},"effect":{"type":"CantCastMoreThan","value":1}}]}
```

`CardsSpec.checkFile` asserts **byte-stability** — the committed file must equal `Json.render (Codec.printingToJson p) <> "\n"` exactly. If it reports a mismatch, the render is authoritative: canonicalize the file through the codec rather than weakening the assertion.

```bash
cabal repl lib:pawl <<'HS'
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
:{
let regen p = do
      c <- TIO.readFile p
      case Json.parse c >>= Codec.jsonToPrinting of
        Left e -> putStrLn (p ++ ": " ++ T.unpack e)
        Right pr -> TIO.writeFile p (Json.render (Codec.printingToJson pr) <> T.pack "\n")
:}
regen "data/cards/rule-of-law.json"
HS
```

Expected: no output (the file parsed and was re-rendered in place).

- [ ] **Step 6: Register the printing**

In `source/test-suite/Pawl/Cards.hs`, add `ruleOfLawPrinting :: Printing.Printing` to the `Cards` record (at the end, after `hagOfInnerWeaknessPrinting`), `ruleOfLawPrinting_ <- loadPrinting "rule-of-law"` to `loadCards`, the field to the returned `MkCards`, and `ruleOfLawPrinting cards,` to `allPrintings`. Do **not** add it to any deck.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the `RuleOfLaw` group green (all six cases), the `CardSpec` shape assertion green, `CardsSpec`'s directory lint and byte-stability green, everything else unchanged.

If the "casting Rule of Law itself uses up the turn's one spell" case fails, the count is being taken from somewhere other than the whole turn log — fix `castsThisTurn`, never the test.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl data/cards/rule-of-law.json source/test-suite pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): Pawl.PlayerEffect and the CR 601.3 casting gate

Rule of Law lands. Its ruling is its test: the spell that uses up the
turn's allowance is Rule of Law itself, cast before the effect existed,
so the count is a fold over P4's whole turn log and not a watermark. The
gate is in Cast.castable, upstream of legalActions -- the engine never
offers an illegal action -- and it also covers the cast-from-library
path, because Panglacial's permission excepts only CR 601.3's timing.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: `Pawl.Cost`, CR 601.2f, and Thalia

The total-cost computation and the cost-increase arm, taxing both castability and payment. Taxing only one is the falsifier: tax castability but not payment and the player underpays; tax payment but not castability and the engine offers a cast that cannot be afforded — and there is no mid-announcement rewind (#56), so that is a wedged game.

**Files:**
- Create: `source/library/Pawl/Cost.hs`
- Create: `data/cards/thalia-guardian-of-thraben.json`
- Modify: `source/library/Pawl/PlayerEffect.hs` (`costAdjustments`, `matchesSpell`)
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Soldier`), `source/library/Pawl/Mana.hs` (`subtypeMana`'s new arm), `source/library/Pawl/Codec.hs` (`subtypeToJson`/`jsonToSubtype`)
- Modify: `source/library/Pawl/Cast.hs` (`castable`, `castableWhileSearching`, `castSpell` — the three cost sites)
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `PlayerEffect.applying` (Task 3).
- Produces: `Pawl.Cost.total :: PlayerId -> ObjectId -> ManaCost -> GameState -> ManaCost`; `Pawl.Cost.applyAdjustments :: ([Natural], [Natural]) -> ManaCost -> ManaCost`; `Pawl.PlayerEffect.costAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [Natural])`; `Pawl.PlayerEffect.matchesSpell :: SpellCriterion -> ObjectId -> GameState -> Bool`; `Subtype.Soldier`; `Cards.thaliaPrinting :: Cards -> Printing`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/PlayerEffectSpec.hs`, add two groups and register both in `tests`:

```haskell
-- The CR 601.2f arithmetic, with no board at all. The unit half of the cost
-- axis; the gate cards below are the gameplay half.
red :: ManaSymbol.ManaSymbol
red = ManaSymbol.OfType (ManaType.Colored Color.Red)

blue :: ManaSymbol.ManaSymbol
blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)

adjustmentTests :: Tasty.TestTree
adjustmentTests =
  Tasty.testGroup
    "Adjustments"
    [ HU.testCase "no adjustments is the identity on a printed cost" $
        HU.assertEqual
          "unchanged"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
          (Cost.applyAdjustments ([], []) (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])),
      HU.testCase "CR 601.2f an increase adds generic mana" $
        HU.assertEqual
          "{R} taxed by {1} is {1}{R}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
          (Cost.applyAdjustments ([1], []) (ManaCost.MkManaCost [red])),
      -- CR 118.7a: "Effects that reduce a cost by an amount of generic mana
      -- affect only the generic mana component of that cost. They can't affect
      -- the colored or colorless mana components."
      HU.testCase "CR 118.7a a reduction with no generic component to take is lost" $
        HU.assertEqual
          "{U} reduced by {1} is still {U}"
          (ManaCost.MkManaCost [blue])
          (Cost.applyAdjustments ([], [1]) (ManaCost.MkManaCost [blue])),
      HU.testCase "CR 118.7a a reduction takes only the generic component" $
        HU.assertEqual
          "{2}{U} reduced by {1} is {1}{U}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])
          (Cost.applyAdjustments ([], [1]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue])),
      HU.testCase "CR 601.2f the total can't be reduced below {0}" $
        HU.assertEqual
          "{1} reduced by {3} is {0}"
          (ManaCost.MkManaCost [])
          (Cost.applyAdjustments ([], [3]) (ManaCost.MkManaCost [ManaSymbol.Generic 1])),
      -- THE ORDER TEST, in the small. Increase first gives {1}{U}, which the
      -- reduction takes back to {U}. Reduce first loses the reduction to CR
      -- 118.7a's empty generic component, and the increase then leaves {1}{U}.
      HU.testCase "CR 601.2f every increase applies before any reduction" $
        HU.assertEqual
          "{U} +{1} -{1} is {U}"
          (ManaCost.MkManaCost [blue])
          (Cost.applyAdjustments ([1], [1]) (ManaCost.MkManaCost [blue]))
    ]

-- Thalia, Guardian of Thraben {1}{W} Legendary Creature -- Human Soldier 2/1:
-- "First strike / Noncreature spells cost {1} more to cast."
thaliaTests :: Cards.Cards -> Tasty.TestTree
thaliaTests cards =
  let -- alice controls Thalia and `n` untapped Mountains; her hand holds one
      -- Lightning Bolt ({R} instant -- noncreature) and one Goblin Piker
      -- ({1}{R} creature).
      board n =
        let base = S.landsInPlay (Cards.mountainPrinting cards) n
            (_, gs1) = S.addCreature (Cards.thaliaPrinting cards) S.alice base
            (bolt, gs2) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs1
            (piker, gs3) = S.addHandCard (Cards.pikerPrinting cards) S.alice gs2
         in ( bolt,
              piker,
              gs3
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      untaxed n =
        let base = S.landsInPlay (Cards.mountainPrinting cards) n
            (bolt, gs1) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice base
         in (bolt, gs1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})
   in Tasty.testGroup
        "Thalia"
        [ HU.testCase "CR 601.2f a noncreature spell's total cost is one more" $
            let (bolt, _, gs) = board 3
             in HU.assertEqual
                  "{R} becomes {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.alice bolt (ManaCost.MkManaCost [red]) gs),
          -- Ruling: "Thalia's ability affects each spell that's not a creature
          -- spell, including your own." SpellCriterion reads the PROJECTION.
          HU.testCase "CR 613.11 a creature spell is unaffected" $
            let (_, piker, gs) = board 3
             in HU.assertEqual
                  "{1}{R} stays {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.alice piker (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs),
          -- BOTH sites, one scenario. Taxing castability but not payment lets
          -- the player underpay; taxing payment but not castability offers a
          -- cast that cannot be afforded, and there is no mid-announcement
          -- rewind (#56) -- that is a wedged game, not a rejected action.
          HU.testCase "CR 601.2f castability is measured against the total cost" $
            let (boltOne, _, oneLand) = board 1
                (boltTwo, _, twoLands) = board 2
                (_, pikerTwo, twoLandsAgain) = board 2
             in do
                  HU.assertBool "one Mountain is not enough for a taxed Bolt" (not (Cast.castable S.alice boltOne oneLand))
                  HU.assertBool "two Mountains are" (Cast.castable S.alice boltTwo twoLands)
                  HU.assertBool "and an untaxed creature spell needs only its printed two" (Cast.castable S.alice pikerTwo twoLandsAgain),
          HU.testCase "CR 601.2f payment spends the total cost" $
            let (bolt, _, gs) = board 3
                paid = S.runPure S.identityAnswer gs (Cast.castSpell S.alice bolt)
                (boltU, gsU) = untaxed 3
                paidU = S.runPure S.identityAnswer gsU (Cast.castSpell S.alice boltU)
             in do
                  HU.assertEqual "taxed: two lands tapped" 2 (S.tappedCount S.alice paid)
                  HU.assertEqual "untaxed: one land tapped" 1 (S.tappedCount S.alice paidU),
          -- The EachPlayer scope: Thalia's controller is taxed (every assertion
          -- above is alice's own spell) and so is her opponent.
          HU.testCase "CR 611.1 the opponent is taxed too" $
            let (_, _, gs) = board 3
                (bobBolt, withBob) = S.addHandCard (Cards.lightningBoltPrinting cards) S.bob gs
             in HU.assertEqual
                  "bob's {R} is also {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.bob bobBolt (ManaCost.MkManaCost [red]) withBob)
        ]
```

`red` and `blue` are the two `ManaSymbol` shorthands defined in `adjustmentTests` above; keep them as top-level bindings so both groups share them.

In `source/test-suite/Pawl/CardSpec.hs`, add to `m45p7CardTests`:

```haskell
      HU.testCase "Thalia is a {1}{W} 2/1 Legendary Human Soldier with first strike and one IncreaseSpellCost ability" $
        let c = Printing.card (Cards.thaliaPrinting cards)
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
         in do
              HU.assertEqual "name" (Text.pack "Thalia, Guardian of Thraben") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white])) (Card.Type.manaCost c)
              HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
              HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
              HU.assertEqual "supertypes" (Set.singleton Supertype.Legendary) (TypeLine.supertypes (Card.Type.typeLine c))
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Soldier]) (TypeLine.subtypes (Card.Type.typeLine c))
              HU.assertEqual "keywords" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords c)
              HU.assertEqual
                "one player ability"
                [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.IncreaseSpellCost SpellCriterion.NoncreatureSpell 1)]
                (Card.Type.playerAbilities c),
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Cost'`, `Data constructor not in scope: Subtype.Soldier`, `Variable not in scope: Cards.thaliaPrinting`.

- [ ] **Step 3: Add the subtype**

In `source/library/Pawl/Type/Subtype.hs`, append at the **end** of the constructor list (declaration order is the JSON's `Set.toAscList` order, so `Human` must keep preceding `Soldier`):

```haskell
  | Soldier -- CR 205.3m (a creature type; Thalia, Guardian of Thraben's)
```

Add the matching arms to `subtypeToJson` and `jsonToSubtype` in `source/library/Pawl/Codec.hs`, in the same position. Add the arm `Subtype.Soldier -> Nothing` to `Mana.subtypeMana`, with the comment the existing creature-type arms carry:

```haskell
  -- CR 205.3m: Soldier is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Soldier -> Nothing
```

- [ ] **Step 4: Add `costAdjustments` and `matchesSpell`**

In `source/library/Pawl/PlayerEffect.hs`:

```haskell
-- CR 613.11: does this spell match the criterion? Both inhabitants read the
-- PROJECTION -- a card type is CR 613 layer 4 and a colour is layer 5 -- and
-- never a printed characteristic, per the standing house rule.
matchesSpell :: SpellCriterion -> ObjectId -> GameState -> Bool
matchesSpell criterion oid gs = case criterion of
  SpellCriterion.NoncreatureSpell -> not (Set.member CardType.Creature (Projection.cardTypesOf oid gs))
  SpellCriterion.SpellOfColor color -> Set.member color (Projection.colorsOf oid gs)

-- CR 613.11 / 601.2f: the cost increases and the cost reductions that apply to
-- `pid` casting `oid`, as two lists.
--
-- Kept APART, never summed into one signed delta: CR 601.2f applies every
-- increase before any reduction, and CR 118.7a gives a reduction a restriction
-- an increase does not have. Pawl.Cost.applyAdjustments is what consumes the
-- pair; this function only decides membership.
--
-- matchesSpell is called only from inside an arm that already matched a
-- cost-modifying constructor, so a board with no Thalia and no Medallion runs no
-- projections at all.
costAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [Natural])
costAdjustments pid oid gs =
  let matching criterion amount = if matchesSpell criterion oid gs then Just amount else Nothing
      increaseOf effect = case effect of
        PlayerEffect.IncreaseSpellCost criterion amount -> matching criterion amount
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
      reductionOf effect = case effect of
        PlayerEffect.ReduceSpellCost criterion amount -> matching criterion amount
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
      effects = applying pid gs
   in (Maybe.mapMaybe increaseOf effects, Maybe.mapMaybe reductionOf effects)
```

- [ ] **Step 5: Create `Pawl.Cost`**

`source/library/Pawl/Cost.hs`:

```haskell
-- CR 601.2f: what a spell's total cost IS. Pawl.Mana keeps pools, production and
-- payment; this module keeps the cost itself, and is where P8's additional and
-- alternative costs land.
module Pawl.Cost where

import Numeric.Natural (Natural)
import qualified Pawl.PlayerEffect as PlayerEffect
import Pawl.Type.GameState (GameState)
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions." `cost` arrives with X already substituted, because CR 601.2b
-- precedes 601.2f -- Sapphire Medallion's own ruling says so: "If a spell you cast
-- has {X} in its mana cost, you choose the value of X before calculating the
-- spell's total cost."
--
-- Additional costs are NOT here: AdditionalCost is untouched by this phase (P8).
total :: PlayerId -> ObjectId -> ManaCost -> GameState -> ManaCost
total pid oid cost gs = applyAdjustments (PlayerEffect.costAdjustments pid oid gs) cost

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: "add any cost increases, then apply any cost
--    reductions").
-- 2. Every REDUCTION comes off the generic component ONLY (CR 118.7a: "Effects
--    that reduce a cost by an amount of generic mana affect only the generic
--    mana component of that cost. They can't affect the colored or colorless
--    mana components."), floored at zero -- a reduction with no generic left to
--    take is simply lost.
-- 3. CR 601.2f's "if the mana component of the total cost is reduced to nothing
--    ... it is considered to be {0}. It can't be reduced to less than {0}" needs
--    no special case: ManaCost is a list of symbols and the empty list IS {0}.
--
-- Reductions are SUMMED rather than applied one at a time. CR 118.7e's "if
-- multiple cost reductions apply, the player may apply them in any order" is a
-- prompt in the rules and an elision here (#N): every reduction P7 can express is
-- an amount of generic mana routed to the same component by CR 118.7a, so summing
-- is not merely equivalent to some order -- it is equivalent to EVERY order.
--
-- The result is CANONICAL: one leading Generic symbol carrying the whole generic
-- component (omitted entirely when it is zero), then the printed typed symbols in
-- their original order. Presentation, not semantics -- Mana.spend sums every
-- generic symbol and matches typed symbols first -- but it is what makes a total
-- cost comparable, so "{U} taxed and then discounted is exactly {U}" is a
-- statement a test can make.
applyAdjustments :: ([Natural], [Natural]) -> ManaCost -> ManaCost
applyAdjustments adjustments cost =
  let (increases, reductions) = adjustments
      ManaCost.MkManaCost symbols = cost
      genericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        ManaSymbol.Variable -> True
      raised = sum (map genericOf symbols) + sum increases
      taken = sum reductions
      -- Natural subtraction is PARTIAL (it throws on underflow), so the CR
      -- 601.2f floor is also what keeps this total.
      lowered = if raised >= taken then raised - taken else 0
      leading = if lowered == 0 then [] else [ManaSymbol.Generic lowered]
   in ManaCost.MkManaCost (leading ++ filter isTyped symbols)
```

- [ ] **Step 6: Route the three cost sites through `Pawl.Cost`**

In `source/library/Pawl/Cast.hs`, add `import qualified Pawl.Cost as Cost` and change the three sites.

`castable`'s affordability line becomes:

```haskell
      -- CR 601.2f: affordability is measured against the TOTAL cost, not the
      -- printed one. Taxing castability without taxing payment lets the player
      -- underpay; taxing payment without taxing castability offers a cast that
      -- cannot be afforded, and there is no mid-announcement rewind (#56).
      && Mana.canPay pid (Cost.total pid oid (Mana.substituteX 0 cost) gs) gs
```

`castableWhileSearching`'s `affordable` becomes:

```haskell
      affordable oid = case costOf oid gs of
        Nothing -> False
        -- CR 601.2b castability floor at the CR 601.2f total (see Cast.castable).
        Just cost -> Mana.canPay pid (Cost.total pid oid (Mana.substituteX 0 cost) gs) gs
```

`castSpell`'s payment line becomes:

```haskell
            -- CR 601.2b then 601.2f: substitute X, then compute the total cost.
            -- The object is still in HAND here, one step before 601.2a moves it
            -- to the stack, so the criterion is read against its hand
            -- projection (#N).
            let paidCost = Cost.total pid oid (maybe cost (\x -> Mana.substituteX x cost) mAmount) gs
```

- [ ] **Step 7: Write the card file and register the printing**

Create `data/cards/thalia-guardian-of-thraben.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Thalia, Guardian of Thraben","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],"typeLine":{"supertypes":[{"type":"Legendary"}],"types":[{"type":"Creature"}],"subtypes":[{"type":"Human"},{"type":"Soldier"}]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":1},"keywords":[{"type":"FirstStrike"}],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"playerAbilities":[{"scope":{"type":"EachPlayer"},"effect":{"type":"IncreaseSpellCost","value":[{"type":"NoncreatureSpell"},1]}}]}
```

Run the `regen` recipe from Task 3 Step 5 against `data/cards/thalia-guardian-of-thraben.json` and confirm it produces no output. Register `thaliaPrinting` in `source/test-suite/Pawl/Cards.hs` (record field, `loadCards` with slug `"thalia-guardian-of-thraben"`, `MkCards`, `allPrintings`). Do **not** add it to any deck.

- [ ] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `Adjustments` and `Thalia` green, the `CardSpec` shape assertion green, everything else unchanged.

- [ ] **Step 9: Commit**

```bash
git add source/library/Pawl data/cards/thalia-guardian-of-thraben.json source/test-suite pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): Pawl.Cost and CR 601.2f, with Thalia as the gate

Total cost taxes BOTH castability and payment: taxing one lets the
player underpay or wedges the game on an unaffordable offer, and pawl
has no mid-announcement rewind. Increases and reductions stay two
lists, because 601.2f orders them and 118.7a restricts only one.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Sapphire Medallion — CR 118.7a and the ordering falsifier

The reduction producer, the `You` scope, and the one test that can tell CR 601.2f's order from its reverse.

**Files:**
- Create: `data/cards/sapphire-medallion.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `Cost.total`, `Cost.applyAdjustments`, `PlayerEffect.costAdjustments` (Task 4); `Cards.thaliaPrinting` (Task 4).
- Produces: `Cards.sapphireMedallionPrinting :: Cards -> Printing`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/PlayerEffectSpec.hs`, add a group and register it in `tests`:

```haskell
-- Sapphire Medallion {2} Artifact: "Blue spells you cast cost {1} less to cast."
medallionTests :: Cards.Cards -> Tasty.TestTree
medallionTests cards =
  let -- alice controls a Sapphire Medallion, `n` untapped Islands, and a Piker
      -- for Unsummon to target; her hand holds Unsummon ({U} instant),
      -- Divination ({2}{U} sorcery) and Lightning Bolt ({R} instant).
      board n =
        let base = S.landsInPlay (Cards.islandPrinting cards) n
            (_, gs1) = S.addCreature (Cards.sapphireMedallionPrinting cards) S.alice base
            (_, gs2) = S.addPiker cards S.bob gs1
            (unsummon, gs3) = S.addHandCard (Cards.unsummonPrinting cards) S.alice gs2
            (divination, gs4) = S.addHandCard (Cards.divinationPrinting cards) S.alice gs3
            (bolt, gs5) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs4
         in ( unsummon,
              divination,
              bolt,
              gs5
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- Thalia AND the Medallion, and one Island. The CR 601.2f order test.
      bothBoard =
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, gs1) = S.addCreature (Cards.sapphireMedallionPrinting cards) S.alice base
            (_, gs2) = S.addCreature (Cards.thaliaPrinting cards) S.alice gs1
            (unsummon, gs3) = S.addHandCard (Cards.unsummonPrinting cards) S.alice gs2
         in ( unsummon,
              gs3
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
   in Tasty.testGroup
        "SapphireMedallion"
        [ -- Ruling: "The ability can't reduce the amount of colored mana you pay
          -- for a spell. It reduces only the generic mana component of that
          -- cost." THE HEADLINE FALSIFIER: subtracting from the mana value would
          -- make this spell free.
          HU.testCase "CR 118.7a a {U} spell still costs {U}" $
            let (unsummon, _, _, gs) = board 2
             in HU.assertEqual
                  "unchanged"
                  (ManaCost.MkManaCost [blue])
                  (Cost.total S.alice unsummon (ManaCost.MkManaCost [blue]) gs),
          HU.testCase "CR 118.7a a {2}{U} spell costs {1}{U}" $
            let (_, divination, _, gs) = board 2
             in HU.assertEqual
                  "one generic off"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])
                  (Cost.total S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) gs),
          HU.testCase "CR 613.11 a red spell is unaffected" $
            let (_, _, bolt, gs) = board 2
             in HU.assertEqual
                  "unchanged"
                  (ManaCost.MkManaCost [red])
                  (Cost.total S.alice bolt (ManaCost.MkManaCost [red]) gs),
          -- Divination is {2}{U}: three mana printed, two after the discount. Two
          -- Islands is exactly the amount that tells the two apart.
          HU.testCase "CR 601.2f the discount is observable at the castability gate" $
            let (_, divination, _, withMedallion) = board 2
                bareBoard =
                  let base = S.landsInPlay (Cards.islandPrinting cards) 2
                      (d, gs1) = S.addHandCard (Cards.divinationPrinting cards) S.alice base
                   in ( d,
                        gs1
                          { GameState.phase = Phase.PrecombatMain,
                            GameState.activePlayer = S.alice,
                            GameState.priority = Just S.alice
                          }
                      )
                (bareDivination, bare) = bareBoard
             in do
                  HU.assertBool "castable for {1}{U} with two Islands" (Cast.castable S.alice divination withMedallion)
                  HU.assertBool "and not castable for {2}{U} without the Medallion" (not (Cast.castable S.alice bareDivination bare)),
          -- CR 611.1 / 109.5: the You scope is the effect's controller.
          HU.testCase "CR 109.5 the You scope does not discount an opponent's spell" $
            let (_, _, _, gs) = board 2
                (bobDivination, withBob) = S.addHandCard (Cards.divinationPrinting cards) S.bob gs
             in HU.assertEqual
                  "bob pays full price"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue])
                  (Cost.total S.bob bobDivination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) withBob),
          -- Ruling: "If there are additional costs to cast a spell, or if the
          -- cost to cast a spell is increased by an effect (such as the one
          -- created by Thalia, Guardian of Thraben's ability), apply those
          -- increases before applying cost reductions." THE ORDER TEST, and it
          -- names a cost with NO generic component on purpose: the two orders
          -- agree wherever the CR 601.2f floor does not bind.
          HU.testCase "CR 601.2f Thalia then the Medallion leaves a {U} spell at exactly {U}" $
            let (unsummon, gs) = bothBoard
             in do
                  HU.assertEqual
                    "increase first, then reduce"
                    (ManaCost.MkManaCost [blue])
                    (Cost.total S.alice unsummon (ManaCost.MkManaCost [blue]) gs)
                  HU.assertBool "so one Island is enough" (Cast.castable S.alice unsummon gs)
        ]
```

In `source/test-suite/Pawl/CardSpec.hs`, add to `m45p7CardTests`:

```haskell
      HU.testCase "Sapphire Medallion is a {2} artifact with one You ReduceSpellCost Blue ability" $
        let c = Printing.card (Cards.sapphireMedallionPrinting cards)
         in do
              HU.assertEqual "name" (Text.pack "Sapphire Medallion") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) (Card.Type.manaCost c)
              HU.assertEqual "types" (Set.singleton CardType.Artifact) (TypeLine.types (Card.Type.typeLine c))
              HU.assertEqual
                "one player ability"
                [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (SpellCriterion.SpellOfColor Color.Blue) 1)]
                (Card.Type.playerAbilities c),
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Variable not in scope: Cards.sapphireMedallionPrinting`.

- [ ] **Step 3: Write the card file**

Create `data/cards/sapphire-medallion.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Sapphire Medallion","manaCost":[{"type":"Generic","value":2}],"typeLine":{"supertypes":[],"types":[{"type":"Artifact"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"playerAbilities":[{"scope":{"type":"You"},"effect":{"type":"ReduceSpellCost","value":[{"type":"SpellOfColor","value":{"type":"Blue"}},1]}}]}
```

Run the `regen` recipe from Task 3 Step 5 against it; expect no output.

- [ ] **Step 4: Register the printing**

In `source/test-suite/Pawl/Cards.hs`, add `sapphireMedallionPrinting` (record field, `loadCards` with slug `"sapphire-medallion"`, `MkCards`, `allPrintings`). Do **not** add it to any deck.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `SapphireMedallion` green (all six cases, the order test included).

If the order test reports `{1}{U}`, the reduction is being applied before the increase — fix `Cost.applyAdjustments`, never the test. If it reports `{0}` or an empty cost for the `{U}` case, the reduction is coming off the coloured component and CR 118.7a is being violated.

- [ ] **Step 6: Commit**

```bash
git add data/cards/sapphire-medallion.json source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): Sapphire Medallion, CR 118.7a and the 601.2f order

A reduction takes only the generic component, so {1} off a {U} spell
leaves {U} -- the falsifier a mana-value implementation makes free. With
Thalia out too, a {U} blue noncreature spell still costs exactly {U},
which is the one shape that can tell 601.2f's order from its reverse.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: `maximumHandSize`, `Engine.discardToHandSize`, and Reliquary Tower

The value-override arm, read off the casting path entirely — and the separation of two constants the rules keep apart that `Engine.discardToHandSize` currently conflates: CR 103.5's starting hand size and CR 402.2's maximum hand size, both "normally seven".

**Files:**
- Create: `data/cards/reliquary-tower.json`
- Modify: `source/library/Pawl/PlayerEffect.hs` (`defaultMaximumHandSize`, `maximumHandSize`)
- Modify: `source/library/Pawl/Engine.hs:128-138` (`discardToHandSize`)
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `PlayerEffect.applying` (Task 3).
- Produces: `PlayerEffect.defaultMaximumHandSize :: Natural`; `PlayerEffect.maximumHandSize :: PlayerId -> GameState -> Maybe Natural`; `Cards.reliquaryTowerPrinting :: Cards -> Printing`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/PlayerEffectSpec.hs`, add a group and register it in `tests`:

```haskell
-- Reliquary Tower, a Land: "You have no maximum hand size. / {T}: Add {C}."
reliquaryTowerTests :: Cards.Cards -> Tasty.TestTree
reliquaryTowerTests cards =
  let -- alice holds nine Plains cards; the board is otherwise empty unless a
      -- printing is named.
      handOfNine extra =
        let gs0 = Setup.emptyGame S.bothPlayers
            put g printing = snd (S.addCreature printing S.alice g)
            withExtra = List.foldl' put gs0 extra
            add g _ = snd (S.addHandCard (Cards.plainsPrinting cards) S.alice g)
         in List.foldl' add withExtra [1 .. 9 :: Int]
      cleanup gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
   in Tasty.testGroup
        "ReliquaryTower"
        [ HU.testCase "CR 402.2 the maximum hand size is normally seven" $
            HU.assertEqual "seven" (Just 7) (PlayerEffect.maximumHandSize S.alice (handOfNine [])),
          HU.testCase "CR 514.1 nine cards at cleanup discards down to seven" $
            let after = cleanup (handOfNine [])
             in do
                  HU.assertEqual "hand" 7 (S.handSize S.alice after)
                  HU.assertEqual "two discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
          HU.testCase "CR 402.2 Reliquary Tower removes the maximum entirely" $
            HU.assertEqual
              "no maximum"
              Nothing
              (PlayerEffect.maximumHandSize S.alice (handOfNine [Cards.reliquaryTowerPrinting cards])),
          HU.testCase "CR 514.1 with Reliquary Tower nothing is discarded and nothing is asked" $
            let after = cleanup (handOfNine [Cards.reliquaryTowerPrinting cards])
             in do
                  HU.assertEqual "hand keeps nine" 9 (S.handSize S.alice after)
                  HU.assertEqual "nothing discarded" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
          -- CR 109.5: the You scope. bob does not share alice's Tower.
          HU.testCase "CR 109.5 the opponent still has a maximum hand size" $
            HU.assertEqual
              "seven"
              (Just 7)
              (PlayerEffect.maximumHandSize S.bob (handOfNine [Cards.reliquaryTowerPrinting cards])),
          -- CR 305.7: a land whose subtype is SET to a basic type loses its
          -- rules-text abilities. Reliquary Tower is nonbasic, and Blood Moon is
          -- in the pool -- so this axis composes with the layer system without
          -- being part of it.
          HU.testCase "CR 305.7 Blood Moon strips the ability off the Tower" $
            let board = handOfNine [Cards.reliquaryTowerPrinting cards, Cards.bloodMoonPrinting cards]
             in HU.assertEqual "seven again" (Just 7) (PlayerEffect.maximumHandSize S.alice board)
        ]
```

In `source/test-suite/Pawl/CardSpec.hs`, add to `m45p7CardTests`:

```haskell
      HU.testCase "Reliquary Tower is a land with a You NoMaximumHandSize ability and a {T} colorless mana ability" $
        let c = Printing.card (Cards.reliquaryTowerPrinting cards)
         in do
              HU.assertEqual "name" (Text.pack "Reliquary Tower") (Card.Type.name c)
              HU.assertEqual "no mana cost" Nothing (Card.Type.manaCost c)
              HU.assertEqual "types" (Set.singleton CardType.Land) (TypeLine.types (Card.Type.typeLine c))
              HU.assertEqual "not basic" Set.empty (TypeLine.supertypes (Card.Type.typeLine c))
              HU.assertEqual
                "one player ability"
                [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]
                (Card.Type.playerAbilities c)
              case Card.Type.activatedAbilities c of
                [ab] -> do
                  HU.assertEqual "tap cost only" [AdditionalCost.TapSelf] (AbilityCost.additional (ActivatedAbility.cost ab))
                  HU.assertEqual "no mana cost" Nothing (AbilityCost.mana (ActivatedAbility.cost ab))
                  HU.assertEqual "adds colorless" [Effect.AddMana ManaType.Colorless] (Modal.allEffects (ActivatedAbility.modal ab))
                _ -> HU.assertFailure "expected exactly one activated ability",
```

`Modal.allEffects` here is `Pawl.Modal`'s; if `CardSpec` imports only `Pawl.Type.Modal`, use `Foldable.toList (Mode.effects m)` over the single mode instead, exactly as the P6 Master Thief assertion does.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Variable not in scope: PlayerEffect.maximumHandSize`, `Cards.reliquaryTowerPrinting`.

- [ ] **Step 3: Add `maximumHandSize`**

In `source/library/Pawl/PlayerEffect.hs`:

```haskell
-- CR 402.2: "each player has a maximum hand size, which is normally seven
-- cards." This is NOT CR 103.5's starting hand size, which is a different seven
-- (Setup.openingHand) that this constant deliberately does not share -- the
-- rules keep them apart, and Reliquary Tower changes only one of them.
defaultMaximumHandSize :: Natural
defaultMaximumHandSize = 7

-- CR 402.2 / 613.11: this player's maximum hand size. Nothing IS "no maximum
-- hand size" (Reliquary Tower) -- never a sentinel, and never a very large
-- number.
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
maximumHandSize pid gs =
  let removes effect = case effect of
        PlayerEffect.NoMaximumHandSize -> True
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
   in if any removes (applying pid gs)
        then Nothing
        else Just defaultMaximumHandSize
```

- [ ] **Step 4: Read it at the cleanup step**

In `source/library/Pawl/Engine.hs`, `discardToHandSize` becomes:

```haskell
discardToHandSize :: PlayerId -> Game ()
discardToHandSize pid = do
  gs <- State.get
  -- CR 402.2, not CR 103.5: the maximum hand size is its own rule and its own
  -- seven, and an effect may remove it entirely (Reliquary Tower). A player with
  -- no maximum discards nothing and is never asked.
  case PlayerEffect.maximumHandSize pid gs of
    Nothing -> pure ()
    Just limit -> do
      let held = Game.zoneMembers Zone.Hand pid gs
          excess = length held - fromIntegral limit
      Monad.when (excess > 0) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider pid held (fromIntegral excess)))
        let inHand oid = List.elem oid held
            toDiscard = take excess (filter inHand chosen)
        Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toDiscard
```

Add `import qualified Pawl.PlayerEffect as PlayerEffect`. Drop the `Pawl.Setup` import **only** if nothing else in `Engine` uses it.

- [ ] **Step 5: Write the card file and register the printing**

Create `data/cards/reliquary-tower.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Reliquary Tower","manaCost":null,"typeLine":{"supertypes":[],"types":[{"type":"Land"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[{"cost":{"mana":null,"additional":[{"type":"TapSelf"}]},"modal":{"modes":[{"effects":[{"type":"AddMana","value":{"type":"Colorless"}}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"playerAbilities":[{"scope":{"type":"You"},"effect":{"type":"NoMaximumHandSize"}}]}
```

Run the `regen` recipe from Task 3 Step 5 against it; expect no output. Register `reliquaryTowerPrinting` in `source/test-suite/Pawl/Cards.hs` (slug `"reliquary-tower"`). Do **not** add it to any deck.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `ReliquaryTower` green (all six cases), the `CardSpec` shape assertion green, and **every pre-existing cleanup test still green**: `defaultMaximumHandSize` is 7, the same number `Setup.openingHand` was, so no existing behaviour changes.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl data/cards/reliquary-tower.json source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): Reliquary Tower, and CR 402.2 split from CR 103.5

The value-override arm, read off the casting path entirely. Nothing IS
'no maximum hand size' -- never a sentinel -- so the cleanup prompt is
asked less often, not differently. Blood Moon still strips the ability
off the Tower (CR 305.7), which is this axis composing with the layer
system without being part of it.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: The stored carrier, and `Pawl.Expiry`'s third list

`ActivePlayerEffect` on `GameState`, gathered by the same `applying`, expiring through P6's three sweeps. No opcode produces one yet; the tests hand-build them, exactly as `ExpirySpec` does for the other two carriers.

**Files:**
- Create: `source/library/Pawl/Type/ActivePlayerEffect.hs`
- Modify: `source/library/Pawl/Type/GameState.hs` (new `playerEffects` field)
- Modify: `source/library/Pawl/Setup.hs:57` and `source/test-suite/Pawl/Support.hs:840` (the two `MkGameState` sites)
- Modify: `source/library/Pawl/PlayerEffect.hs` (`applying` grows its second carrier)
- Modify: `source/library/Pawl/Expiry.hs` (all three sweeps grow a third list)
- Modify: `source/test-suite/Pawl/Support.hs` (an `addPlayerEffect` fixture)
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`

**Interfaces:**
- Consumes: `PlayerEffect.applying`, `PlayerEffect.prohibitsCasting` (Task 3); `Expiry.Expiry`, `Expiry.dropAtCleanup`, `Expiry.dropAtHandoff`, `Expiry.sweepConditional` (P6).
- Produces: `ActivePlayerEffect.MkActivePlayerEffect {source :: ObjectId, controller :: PlayerId, timestamp :: Timestamp, expiry :: Expiry, scope :: PlayerScope, effect :: PlayerEffect}`; `GameState.playerEffects :: [ActivePlayerEffect]`; `S.addPlayerEffect :: Expiry -> PlayerScope -> PlayerEffect -> PlayerId -> GameState -> GameState`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/Support.hs`, add the fixture:

```haskell
-- Seed a stored player effect directly into GameState (bypasses resolving the
-- spell that would install it; use when a test needs one active without a
-- resolution). Object id 998 is the stand-in source, the withEffectAt posture --
-- nothing here reads the source's own characteristics.
addPlayerEffect ::
  Expiry.Expiry ->
  PlayerScope.PlayerScope ->
  PlayerEffect.PlayerEffect ->
  PlayerId.PlayerId ->
  GameState.GameState ->
  GameState.GameState
addPlayerEffect expiry scope effect controller gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActivePlayerEffect.MkActivePlayerEffect
          { ActivePlayerEffect.source = ObjectId.MkObjectId 998,
            ActivePlayerEffect.controller = controller,
            ActivePlayerEffect.timestamp = ts,
            ActivePlayerEffect.expiry = expiry,
            ActivePlayerEffect.scope = scope,
            ActivePlayerEffect.effect = effect
          }
   in gs1 {GameState.playerEffects = active : GameState.playerEffects gs1}
```

In `source/test-suite/Pawl/PlayerEffectSpec.hs`, add a group and register it in `tests`:

```haskell
-- The STORED carrier: a player effect that outlives the object that made it.
-- Hand-built here, exactly as ExpirySpec hand-builds a ContinuousEffect, so the
-- carrier and its sweeps are proven before an opcode can produce one.
storedTests :: Tasty.TestTree
storedTests =
  let base = Setup.emptyGame S.bothPlayers
      silenced =
        S.addPlayerEffect
          Expiry.Type.AtCleanup
          PlayerScope.Opponents
          PlayerEffect.Type.CantCastSpells
          S.alice
          base
   in Tasty.testGroup
        "Stored"
        [ HU.testCase "CR 611.1 a stored effect applies through its scope" $
            do
              HU.assertBool "bob is prohibited" (PlayerEffect.prohibitsCasting S.bob silenced)
              HU.assertBool "alice is not" (not (PlayerEffect.prohibitsCasting S.alice silenced)),
          -- CR 611.2c: the CONTROLLER is baked in at creation, so the scope is
          -- still answerable once the source has left the battlefield -- which
          -- for an instant it always has.
          HU.testCase "CR 611.2c the scope resolves without the source existing" $
            HU.assertEqual "the source id names nothing" Nothing (Game.lookupObject (ObjectId.MkObjectId 998) silenced),
          HU.testCase "CR 514.2 the cleanup sweep drops an AtCleanup player effect" $
            let after = Expiry.dropAtCleanup silenced
             in do
                  HU.assertEqual "one stored before" 1 (length (GameState.playerEffects silenced))
                  HU.assertEqual "none after" [] (GameState.playerEffects after)
                  HU.assertBool "and bob may cast again" (not (PlayerEffect.prohibitsCasting S.bob after)),
          HU.testCase "CR 514.2 the cleanup sweep keeps a Never player effect" $
            let forever = S.addPlayerEffect Expiry.Type.Never PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
             in HU.assertEqual "survives" 1 (length (GameState.playerEffects (Expiry.dropAtCleanup forever))),
          HU.testCase "CR 611.2a the handoff sweep drops an AtTurnOf player effect for the player whose turn began" $
            let armed = S.addPlayerEffect (Expiry.Type.AtTurnOf S.bob) PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
                bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
             in do
                  HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
                  HU.assertEqual "and it ended" [] (GameState.playerEffects bobsTurn),
          HU.testCase "CR 611.2b the conditional sweep deletes a player effect whose condition has failed" $
            let armed =
                  S.addPlayerEffect
                    (Expiry.Type.While S.alice StateCondition.YouControlSource)
                    PlayerScope.Opponents
                    PlayerEffect.Type.CantCastSpells
                    S.alice
                    base
                (changed, swept) = Engine.runGamePure S.identityAnswer armed Expiry.sweepConditional
             in do
                  -- Object 998 is not on the battlefield, so YouControlSource is
                  -- false the moment it is checked.
                  HU.assertBool "the sweep reports a change" changed
                  HU.assertEqual "deleted, not masked" [] (GameState.playerEffects swept)
        ]
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Type.ActivePlayerEffect'`, `GameState.playerEffects` not in scope.

- [ ] **Step 3: Create `Pawl.Type.ActivePlayerEffect`**

```haskell
module Pawl.Type.ActivePlayerEffect where

import Pawl.Type.Expiry (Expiry)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerEffect (PlayerEffect)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.PlayerScope (PlayerScope)
import Pawl.Type.Timestamp (Timestamp)

-- CR 611.1 / 613.11: a stored, resolution-generated player or rules-modifying
-- continuous effect, held in GameState.playerEffects. The player-axis analogue of
-- ActiveReplacement and ContinuousEffect: the printed carrier
-- (Pawl.Type.PlayerStaticAbility) is re-derived live from the battlefield, while
-- these are stored because the object that made them may be long gone.
--
-- `controller` is STORED, where ContinuousEffect stores none. It has to be. A
-- stored Modification re-reads its source's PROJECTED controller (CR 613.1b),
-- which works because the source is a permanent; Silence is an INSTANT, so by the
-- time its effect is live the source is in a graveyard with no controller to
-- project and "your opponents" would be unanswerable. The controller is baked in
-- at creation -- the same treatment Expiry.While already gives CR 109.5's "you".
--
-- `scope`, by contrast, stays DYNAMIC. CR 611.2c's first sentence freezes a
-- stored effect's object set, but its third carves out exactly this axis: such an
-- effect "modifies the rules of the game, so it can affect objects that weren't
-- affected when that continuous effect began". There is no stored-set analogue of
-- Affected.TheseObjects here, and PlayerScope is the same type on both carriers.
--
-- `expiry` decides when a Pawl.Expiry sweep drops it (CR 514.2, 611.2a, 611.2b).
--
-- `timestamp` is stored even though nothing observes it yet: CR 613.10 and 613.11
-- both order by timestamp (CR 613.7), and no two of P7's constructors conflict,
-- so the order is unobservable in this pool. Stamping at creation is free;
-- retrofitting an order onto effects already stored is not (#N).
--
-- Runtime-only, like Expiry and ActiveReplacement: it never appears in card JSON
-- and has no codec, which is what keeps a stored value out of a card file and a
-- printed value out of the store.
data ActivePlayerEffect = MkActivePlayerEffect
  { source :: ObjectId,
    controller :: PlayerId,
    timestamp :: Timestamp,
    expiry :: Expiry,
    scope :: PlayerScope,
    effect :: PlayerEffect
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the `GameState` field and both construction sites**

In `source/library/Pawl/Type/GameState.hs`, add the import and this field beside `replacements`:

```haskell
    -- CR 611.1 / 613.11: stored PLAYER and RULES-modifying continuous effects
    -- from resolutions (Silence), each with an expiry the Pawl.Expiry sweeps
    -- consult. The third carrier sharing that vocabulary. A permanent's printed
    -- player abilities are NOT here -- Pawl.PlayerEffect re-derives those live.
    playerEffects :: [ActivePlayerEffect],
```

Add `GameState.playerEffects = [],` to `Setup.emptyGame`'s record (beside `GameState.replacements = []`) and to `Support.hs`'s `MkGameState` at line 840.

- [ ] **Step 5: Grow `applying`'s second carrier**

In `source/library/Pawl/PlayerEffect.hs`, `applying` gains the stored half. The `printed` binding is unchanged; add:

```haskell
      -- CR 611.2c: the stored carrier. Its controller is read off the record and
      -- never re-derived -- see Pawl.Type.ActivePlayerEffect -- while its scope is
      -- resolved live, exactly as the printed carrier's is.
      storedOne active =
        ( ActivePlayerEffect.controller active,
          ActivePlayerEffect.scope active,
          ActivePlayerEffect.effect active
        )
      stored = map storedOne (GameState.playerEffects gs)
   in map effectOf (filter keep (printed ++ stored))
```

- [ ] **Step 6: Grow `Pawl.Expiry`'s three sweeps**

In `source/library/Pawl/Expiry.hs`, each of `dropAtCleanup`, `dropAtHandoff` and `sweepConditional` gains a `keepPlayerEffect` and a third field update. The module's own comment already records why the sweeps are shared — "the two lists lived in two modules, not because they differed" — so extend it to say **three** carriers.

`dropAtCleanup`:

```haskell
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
        }
```

`dropAtHandoff`: identical addition, same two lines.

`sweepConditional`:

```haskell
      keepPlayerEffect active = survives (ActivePlayerEffect.source active) (ActivePlayerEffect.expiry active)
      keptEffects = filter keepEffect (GameState.continuousEffects gs)
      keptReplacements = filter keepReplacement (GameState.replacements gs)
      keptPlayerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
      changed =
        length keptEffects /= length (GameState.continuousEffects gs)
          || length keptReplacements /= length (GameState.replacements gs)
          || length keptPlayerEffects /= length (GameState.playerEffects gs)
  Monad.when changed $
    State.put
      gs
        { GameState.continuousEffects = keptEffects,
          GameState.replacements = keptReplacements,
          GameState.playerEffects = keptPlayerEffects
        }
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `Stored` green (all six cases), everything else unchanged. No existing test creates a stored player effect, so every sweep's behaviour on the other two carriers is untouched.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl source/test-suite pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): ActivePlayerEffect, the stored carrier and Expiry's third list

The controller is baked in at creation (Silence is an instant, so its
source has no controller left to project) while the SCOPE stays
dynamic, per CR 611.2c's third sentence. P6's three sweeps grow a third
list because it shares their expiry vocabulary.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 8: `Effect.AffectPlayers` and Silence

The opcode that fills the stored carrier, and the last gate card — the one that proves the axis works from a resolution and not only from the battlefield.

**Files:**
- Create: `data/cards/silence.json`
- Modify: `source/library/Pawl/Type/Effect.hs` (new constructor)
- Modify: `source/library/Pawl/Resolve.hs` (five exhaustive matches plus the executor arm)
- Modify: `source/library/Pawl/Codec.hs:992-1063` (`effectToJson`/`jsonToEffect`)
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `ActivePlayerEffect`, `GameState.playerEffects` (Task 7); `Expiry.arm` (P6).
- Produces: `Effect.AffectPlayers :: Duration -> PlayerScope -> PlayerEffect -> Effect card`; `Cards.silencePrinting :: Cards -> Printing`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CodecSpec.hs`, add to the `"effect"` group:

```haskell
          HU.testCase "AffectPlayers round-trips" $
            roundTrip
              "e6"
              Codec.effectToJson
              Codec.jsonToEffect
              (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells),
```

In `source/test-suite/Pawl/PlayerEffectSpec.hs`, add a group and register it in `tests`:

```haskell
-- Silence {W} Instant: "Your opponents can't cast spells this turn."
--
-- The board is BOB's turn on purpose: the "only casting is stopped" ruling names
-- playing a land and activating an ability, and both are only available to the
-- active player or need his own permanents. alice casts Silence at instant speed
-- during his main phase, which is the card's real use.
silenceTests :: Cards.Cards -> Tasty.TestTree
silenceTests cards =
  let resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      board =
        let gs0 = Setup.emptyGame S.bothPlayers
            -- alice: two Plains, two Silences in hand.
            (_, gs1) = S.addCreature (Cards.plainsPrinting cards) S.alice gs0
            (_, gs2) = S.addCreature (Cards.plainsPrinting cards) S.alice gs1
            (silence, gs3) = S.addHandCard (Cards.silencePrinting cards) S.alice gs2
            (silence2, gs4) = S.addHandCard (Cards.silencePrinting cards) S.alice gs3
            -- bob: two Mountains, a Prodigal Sorcerer (a NON-mana activated
            -- ability), a Goblin Piker in hand to cast and a Mountain in hand to
            -- play.
            (_, gs5) = S.addCreature (Cards.mountainPrinting cards) S.bob gs4
            (_, gs6) = S.addCreature (Cards.mountainPrinting cards) S.bob gs5
            (_, gs7) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.bob gs6
            (piker, gs8) = S.addHandCard (Cards.pikerPrinting cards) S.bob gs7
            (land, gs9) = S.addHandCard (Cards.mountainPrinting cards) S.bob gs8
         in ( silence,
              silence2,
              piker,
              land,
              gs9
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.bob,
                  GameState.priority = Just S.bob
                }
            )
      (silenceId, silence2Id, pikerId, landId, before) = board
      after = resolveAll (S.runPure S.identityAnswer before (Cast.castSpell S.alice silenceId))
      isActivate action = case action of
        Action.Type.Activate _ _ -> True
        Action.Type.Cast _ -> False
        Action.Type.Play _ -> False
        Action.Type.Pass -> False
   in Tasty.testGroup
        "Silence"
        [ HU.testCase "before Silence resolves, bob may cast his creature" $
            HU.assertBool "offered" (elem (Action.Type.Cast pikerId) (Action.legalActions S.bob before)),
          -- CR 611.2c, THE FALSIFIER: nothing bob owns is a spell when Silence
          -- resolves -- the stack holds only Silence itself. Freeze the affected
          -- set and this card does literally nothing.
          HU.testCase "CR 611.2c the effect reaches a spell that did not exist when it began" $
            do
              HU.assertEqual "one stored effect" 1 (length (GameState.playerEffects after))
              HU.assertBool "bob is prohibited" (PlayerEffect.prohibitsCasting S.bob after)
              HU.assertEqual
                "and no cast is offered"
                []
                (filter isCast (Action.legalActions S.bob after)),
          -- CR 109.5: "your opponents" is scoped off Silence's controller, which
          -- is baked into the stored effect because its source is in a graveyard.
          HU.testCase "CR 109.5 the Opponents scope spares the caster" $
            do
              HU.assertBool "alice is not prohibited" (not (PlayerEffect.prohibitsCasting S.alice after))
              HU.assertBool "and may cast her second Silence" (Cast.castable S.alice silence2Id after),
          -- Ruling: "The only thing Silence stops is casting spells. Your
          -- opponents can still activate abilities ... they can still play lands,
          -- and so on."
          HU.testCase "CR 601.3 only casting is stopped" $
            do
              HU.assertBool "bob may still play a land" (elem (Action.Type.Play landId) (Action.legalActions S.bob after))
              HU.assertBool "and still activate an ability" (any isActivate (Action.legalActions S.bob after)),
          HU.testCase "CR 514.2 the prohibition ends at cleanup" $
            let ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
             in do
                  HU.assertEqual "nothing stored" [] (GameState.playerEffects ended)
                  HU.assertBool "bob may cast again" (not (PlayerEffect.prohibitsCasting S.bob ended))
        ]
```

In `source/test-suite/Pawl/CardSpec.hs`, add to `m45p7CardTests`:

```haskell
      HU.testCase "Silence is a {W} instant whose one effect is AffectPlayers UntilEndOfTurn Opponents CantCastSpells" $
        let c = Printing.card (Cards.silencePrinting cards)
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
         in do
              HU.assertEqual "name" (Text.pack "Silence") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [white])) (Card.Type.manaCost c)
              HU.assertBool "an instant" (Card.isInstant c)
              HU.assertEqual "no player abilities: it is not a permanent" [] (Card.Type.playerAbilities c)
              HU.assertEqual
                "one targetless opcode"
                [Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells]
                (Card.allEffects c)
              HU.assertEqual "no target slots" Map.empty (Card.allTargetSpecs c)
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Data constructor not in scope: Effect.AffectPlayers`, `Variable not in scope: Cards.silencePrinting`.

- [ ] **Step 3: Add the opcode**

In `source/library/Pawl/Type/Effect.hs`, add the two imports (`Pawl.Type.PlayerEffect (PlayerEffect)`, `Pawl.Type.PlayerScope (PlayerScope)`) and append:

```haskell
  | -- CR 611.1 / 613.11: install a stored PLAYER or RULES-modifying continuous
    -- effect on a class of players for a duration. Silence is
    -- `AffectPlayers UntilEndOfTurn Opponents CantCastSpells`.
    --
    -- Targetless, mirroring Replace: a rules-modifying effect watches a CLASS,
    -- not a chosen object, so there is nothing to target and nothing to prompt.
    -- Resolve stores it into GameState.playerEffects with this effect's source,
    -- its controller (CR 109.5, baked in -- the source may be in a graveyard by
    -- the time anyone asks), a fresh timestamp, and Expiry.arm's answer.
    AffectPlayers Duration PlayerScope PlayerEffect
```

- [ ] **Step 4: Add the six `Pawl.Resolve` arms**

In `source/library/Pawl/Resolve.hs`, add to each existing exhaustive match:

- `slotsOf`: `Effect.AffectPlayers {} -> Set.empty`
- `readsX`: `Effect.AffectPlayers {} -> False`
- `manaProduced`: `Effect.AffectPlayers {} -> Nothing`
- `searchesLibrary`: `Effect.AffectPlayers {} -> False`
- `rewriteEffect`: `Effect.AffectPlayers {} -> effect` (a player effect carries no basic-land-type word for CR 612 to rewrite)

and the executor arm in `applyEffect`, beside `Effect.Replace`:

```haskell
  Effect.AffectPlayers duration scope playerEffect ->
    -- CR 611.1 / 613.11: install the stored player effect. Targetless and
    -- unprompted. CR 109.5: the CONTROLLER is baked in now, because the source
    -- will not have one to project once it leaves the stack (Silence is an
    -- instant). The SCOPE is not: CR 611.2c makes a rules-modifying effect one
    -- that "can affect objects that weren't affected when that continuous effect
    -- began", so it is re-resolved on every read.
    State.modify' $ \gs -> case Expiry.arm controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActivePlayerEffect.MkActivePlayerEffect
                { ActivePlayerEffect.source = source,
                  ActivePlayerEffect.controller = controller,
                  ActivePlayerEffect.timestamp = ts,
                  ActivePlayerEffect.expiry = expiry,
                  ActivePlayerEffect.scope = scope,
                  ActivePlayerEffect.effect = playerEffect
                }
         in gs1 {GameState.playerEffects = active : GameState.playerEffects gs1}
```

- [ ] **Step 5: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add to `effectToJson`:

```haskell
  Effect.AffectPlayers d s pe -> Json.tagged (Text.pack "AffectPlayers") (Just (Array [durationToJson d, playerScopeToJson s, playerEffectToJson pe]))
```

and to `jsonToEffect`:

```haskell
    "AffectPlayers" -> case mv of
      Just (Array [d, s, pe]) -> Effect.AffectPlayers <$> jsonToDuration d <*> jsonToPlayerScope s <*> jsonToPlayerEffect pe
      _ -> Left (Text.pack "AffectPlayers expects [Duration, PlayerScope, PlayerEffect]")
```

- [ ] **Step 6: Write the card file and register the printing**

Create `data/cards/silence.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Silence","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[{"type":"AffectPlayers","value":[{"type":"UntilEndOfTurn"},{"type":"Opponents"},{"type":"CantCastSpells"}]}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[]}
```

Run the `regen` recipe from Task 3 Step 5 against it; expect no output. Register `silencePrinting` in `source/test-suite/Pawl/Cards.hs` (slug `"silence"`). Do **not** add it to any deck.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `Silence` green (all five cases), the codec round-trip green, the `CardSpec` shape assertion green.

If the "reaches a spell that did not exist when it began" case fails with bob still able to cast, the scope is being frozen rather than re-resolved — fix `applyEffect`, never the test. If the "only casting is stopped" case fails, the prohibition has leaked past `Cast.castable` into `Action.legalActions`' land or activation branches.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl data/cards/silence.json source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p7): Effect.AffectPlayers and Silence

The targetless opcode that fills the stored carrier. CR 611.2c is the
whole test: Silence resolves with no opponent spell in existence, so a
frozen affected set would make the card do literally nothing. Its
ruling is the other test -- lands and activated abilities are untouched.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 9: Close — issues filed and cited, docs updated, exit criterion verified

Every `(#N)` placeholder the earlier tasks left in the code is replaced here with a real issue number. **`grep -rn '(#N)' source/` must return nothing when this task is done.**

**Files:**
- Modify: every source file carrying a `(#N)` placeholder
- Modify: `source/library/Pawl/Type/SpellCriterion.hs` (confirm the P9 citation), `source/library/Pawl/Cast.hs`, `source/library/Pawl/Cost.hs`, `source/library/Pawl/PlayerEffect.hs`, `source/library/Pawl/Type/ActivePlayerEffect.hs`
- Modify: `docs/progress.md`, `CLAUDE.md`, `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`

- [ ] **Step 1: File the eleven new issues**

Run each `gh issue create` and record the number it prints. Labels come from CLAUDE.md's set: `elision`, `gap`, `rules-correctness`, `bug`, `expires:milestone`, `expires:card-driven`. The eleven rows are the spec's §8 table, verbatim in intent.

| # | Title | Labels | Body must carry |
|---|---|---|---|
| A | Activated-ability cost modification has no producer | `gap`, `expires:card-driven` | `AbilityCost` is untouched by P7; `Pawl.Cost.total` is asked only about spells. The census's `ReduceManaCostOfActivatedAbilities` class has no producer. Expires on a card that taxes or discounts an activation (Rings of Brighthearth-adjacent). |
| B | CR 118.7e's "apply multiple cost reductions in any order" is elided | `elision`, `expires:card-driven` | Every reduction P7 can express is an amount of generic mana routed to one component by CR 118.7a, so summing them is equivalent to every order and the prompt is unobservable. `Pawl.Cost.applyAdjustments` sums. Expires on a coloured or hybrid reduction, which CR 118.7e explicitly prompts. |
| C | CR 118.7b–g non-generic cost reductions have no producer | `gap`, `expires:card-driven` | Colored, colorless, hybrid, Phyrexian and snow reductions are unrepresentable: `ReduceSpellCost` carries a bare `Natural` of generic mana. Expires on a card whose reduction names a mana type. |
| D | CR 613.10's player-affecting tier has no producer | `gap`, `expires:card-driven` | P7 implements CR 613.11's rules tier only. 613.10 ("an effect might give a player protection from red") is a genuinely distinct tier applied in timestamp order after object characteristics are determined, and nothing produces one. Expires on a card granting a player protection or shroud. |
| E | CR 613.10/613.11 timestamp ordering is not implemented | `rules-correctness`, `expires:card-driven` | Both rules order by timestamp (CR 613.7). `ActivePlayerEffect` stores one and the printed carrier could take its permanent's, but `Pawl.PlayerEffect.applying` returns effects unsorted. None of P7's five constructors conflicts with another, so the order is unobservable; the fix is a sort, not a migration. Expires on **Null Profusion** + Reliquary Tower, the pair Reliquary Tower's own ruling names. |
| F | CR 601.2f's "locked in" total cost is recomputed rather than stored | `rules-correctness`, `expires:card-driven` | CR 601.2f locks the total cost in once it is determined; pawl recomputes it on demand at castability and again at payment. Nothing in the pool can change a cost between announcement and payment, so the two always agree. Expires on an effect that changes a cost mid-announcement. |
| G | CR 601.3a's quality-changing prohibitions are unrepresentable | `gap`, `expires:card-driven` | `PlayerEffect.prohibitsCasting` takes no `ObjectId`, because both of P7's prohibitions are quality-free. CR 601.3a's worked example is **Void Winnower**; the fix widens the signature rather than adding a constructor. Expires on Void Winnower. |
| H | Player-scoped casting and land-play permissions have no producer | `gap`, `expires:card-driven` | `Card.castingPermissions` is object-scoped (Panglacial Wurm). The player-scoped sibling — extra land drops, cast-from-graveyard — has no carrier and no producer. Expires on Exploration or Yawgmoth's Will. |
| I | No card produces a conditional or turn-relative stored player effect | `gap`, `expires:card-driven` | `Pawl.Expiry`'s three sweeps handle `While` and `AtTurnOf` on `GameState.playerEffects` by construction, but every card in the pool arms `AtCleanup`, so those paths are covered only by hand-built fixtures. The sibling of #84 on the third carrier. Expires on a conditional-duration player effect. |
| J | `Cast.castSpell` computes the total cost against an object still in hand | `rules-correctness`, `expires:card-driven` | CR 601.2f runs after 601.2a has moved the spell to the stack; pawl pays first, so `SpellCriterion` is evaluated against the object's *hand* projection. Newly load-bearing, since the total cost now depends on the spell's projected characteristics. Unreached today — nothing in the pool projects differently in hand than on the stack. Adjacent to #56. Expires on a card whose type or colour differs between the two zones. |
| K | Turn-structure skips are replacement effects and have no producer | `gap`, `expires:card-driven` | The gap census (`docs/mtgish-gap-census.md` §3.2) lists `SkipsUntapStep`/`SkipsDrawStep`/`SkipsMainPhase` under `PlayerEffect`; **CR 614.1b is explicit that "effects that use the word 'skip' are replacement effects"**, so they belong on P5's `ReplacementEffect`, not this axis. Filed so the census's misplacement is not followed by a later phase. `Engine.skipsDraw`'s CR 103.7a first-turn skip is a turn-based rule, not an effect, and stays where it is. Expires on a skip card, built on P5. |

- [ ] **Step 2: Sweep the `(#N)` placeholders and confirm the cited ones**

Run: `grep -rn '(#N)' source/`

Replace each hit with the matching issue number, keeping the comment to what is *not* implemented and **never writing an expiry trigger into the comment** (that lives in the issue):

| File | Placeholder | Issue |
|---|---|---|
| `Pawl/PlayerEffect.hs`, `prohibitsCasting`'s comment | the missing `ObjectId` parameter | G |
| `Pawl/Cost.hs`, `applyAdjustments`'s comment | the summed reductions | B |
| `Pawl/Cast.hs`, `castSpell`'s `paidCost` comment | the hand-zone projection | J |
| `Pawl/Type/ActivePlayerEffect.hs`, the `timestamp` comment | the unimplemented order | E |

Add citations at three more sites that this plan does not pre-place — write them now:

- `Pawl/Type/PlayerEffect.hs`, on `ReduceSpellCost`: reductions are an amount of generic mana only (C).
- `Pawl/Type/ActivePlayerEffect.hs`, on `expiry`: no card arms anything but `AtCleanup` on this carrier (I).
- `Pawl/Cost.hs`, on `total`: additional and alternative costs are not part of the total (A is the ability half; the spell half is **#4**, P8 — cite `#4`, do not file a duplicate).

Confirm (do not re-file): `#38`/`#39`/`#40` are the P9 filter-language deferral and `Pawl.Type.SpellCriterion`'s module comment already names them as its siblings; **`SpellCriterion` joins that list as a fourth member** — say so in `SpellCriterion`'s comment and note it in the completion entry, but do **not** close any of them.

Run: `grep -rn '(#N)' source/`
Expected: no output.

- [ ] **Step 3: Verify the exit criterion mechanically**

Run each and confirm the expected result:

```bash
# The FIRST INVARIANT's audit: Pawl.PlayerEffect is the sole rules home of casing
# on this axis. Pawl.Type.* modules only declare, Pawl.Codec only encodes, and
# Pawl.Resolve names AffectPlayers as an opcode without inspecting its payload.
grep -rln 'PlayerEffect\.\(CantCastSpells\|CantCastMoreThan\|IncreaseSpellCost\|ReduceSpellCost\|NoMaximumHandSize\)\|PlayerScope\.\(You\|Opponents\|EachPlayer\)\|SpellCriterion\.\(NoncreatureSpell\|SpellOfColor\)' source/library/ \
  | grep -v 'Pawl/PlayerEffect.hs\|Pawl/Codec.hs\|Pawl/Type/'          # no output

# The layer system is untouched: this axis is a sibling tier, not a layer.
# HEAD~8 is the commit before Task 1 (eight task commits are in at this point).
git diff --stat HEAD~8 -- source/library/Pawl/Projection.hs source/library/Pawl/Type/Layer.hs \
  source/library/Pawl/Type/Modification.hs source/library/Pawl/Type/Affected.hs \
  source/library/Pawl/Type/ContinuousEffect.hs source/library/Pawl/Type/StaticAbility.hs \
  source/library/Pawl/Type/Player.hs                                    # no output

# The stored/printed split's audit from the other side: no card data may name a
# stored player effect.
grep -rn 'ActivePlayerEffect' data/                                     # no output

# P8's surface is untouched.
git diff --stat HEAD~8 -- source/library/Pawl/Type/AdditionalCost.hs source/library/Pawl/Type/AbilityCost.hs   # no output

grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p7-player-effects.md   # counts down to 0
cabal clean && cabal build all --enable-tests --enable-benchmarks       # warning-free
cabal test                                                              # all green
git add -A && hooky run                                                 # passes
cabal bench                                                             # three timings, no large regression
```

`HEAD~8` assumes the eight task commits above are the only commits since Task 1 began; if a fix-up commit landed in between, adjust the ref to the commit before Task 1.

**Watch the benchmark.** `Cast.castable` now runs `PlayerEffect.applying` twice per card in hand per `legalActions` call, each walking the battlefield. `Projection.controllerOf` is a lean fold and the CR 305.7 check is skipped for a permanent with no player abilities, so the added cost should be small — but if `cabal bench` shows a move beyond the suite's own run-to-run noise (~800 µs stddev on a ~12 ms mean), say so plainly in the completion note rather than rounding it away. Note that `#66` still makes all three benchmarks execute the identical game, so the aggregate is the only honest reading.

- [ ] **Step 4: Append the `docs/progress.md` completion entry**

One entry, in the file's established voice, recording what P7 *established* — not what is left. It must state:

- **the five gate cards and what each falsified** — Rule of Law: the count is a fold over P4's whole turn log, because the spell that used up the allowance is Rule of Law itself, cast before the effect existed, and the counted event is the *cast* (a countered spell still counts); Thalia: the tax lands on both castability and payment, since taxing one underpays and taxing the other wedges a game with no rewind (#56); Sapphire Medallion: CR 118.7a, a reduction takes only the generic component — `{1}` off `{U}` leaves `{U}` — and, crossed with Thalia on a cost with *no* generic component, the one shape that can tell CR 601.2f's order from its reverse; Reliquary Tower: `Nothing` is "no maximum hand size", and CR 402.2 is now its own rule and its own seven, separate from CR 103.5's; Silence: CR 611.2c's third sentence — a stored rules-modifying effect's scope is never frozen, or the card does literally nothing;
- **the structural fact the phase rests on** — CR 613.10/613.11 put this axis *outside* the layer system. There is no new `Layer`, no new `Modification`, and `Pawl.Projection` was not edited. Name the mechanical audit in Step 3 that proves it;
- **the census correction** — `SkipsUntapStep`/`SkipsDrawStep`/`SkipsMainPhase` are **not** on this axis: CR 614.1b makes a skip a replacement effect, so they belong on P5's carrier. Filed as issue K so a later phase does not follow §3.2's placement;
- **what was added** — `Pawl.Type.PlayerEffect`, `PlayerScope`, `SpellCriterion`, `PlayerStaticAbility`, `ActivePlayerEffect`; `Pawl.PlayerEffect` (`applying`, `inScope`, `prohibitsCasting`, `castsThisTurn`, `costAdjustments`, `matchesSpell`, `maximumHandSize`, `defaultMaximumHandSize`) and `Pawl.Cost` (`total`, `applyAdjustments`); `Card.playerAbilities`, `GameState.playerEffects`, `Effect.AffectPlayers`, `GameEvent.SpellCast`, `Event.castOf`, `Subtype.Soldier`; six read sites; five card files;
- **the four departures from the spec** listed at the top of this plan, with their reasons;
- **no prompt was added, and one was asked less often** — `Prompt.ChooseDiscard` is skipped entirely for a player with no maximum hand size, which is the absence of a choice rather than the making of one. The one elision is CR 118.7e's reduction order (issue B), unobservable while every reduction is generic;
- **tracking** — closes **#3** (M4.5 P7) and with it git-bug `c5a985d` (GAP-P), and the *modification* half of GAP-Co; **#4** (P8, the payment half) stays open; #38/#39/#40 are cited and **not** retired, with `SpellCriterion` joining that list as a fourth member; eleven new issues filed (A–K above) with their real numbers;
- the final suite count, that the build is warning-clean on a from-scratch `cabal clean` build, and the benchmark comparison with `#66` noted;
- the spec and plan paths, kept as reference.

- [ ] **Step 5: Replace the `CLAUDE.md` status bullet**

**Replace, never append** — milestone history goes in `progress.md`. The new bullet says M0–M4h plus M4.5 P1–P7 are complete, that P7 closed **GAP-P** and the *modification* half of **GAP-Co** and with it the whole of Cluster 3, and that **P8 (costs) and P9 (filters) float freely**, with P10 (player counters) and P11 (Command zone) remaining. Keep it to the same length as the bullet it replaces.

- [ ] **Step 6: Update the umbrella spec**

`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`:
- §3's P7 row (line 109): mark it landed, with a pointer to `docs/superpowers/specs/2026-07-22-p7-player-effects-design.md`. **Correct the row's own description** — it says the axis is "resolved by the projection over players"; it is not, and the reason is CR 613.10/613.11.
- §4's ordering paragraph (line 200): P7 landed, **Cluster 3 closed**; P8 and P9 float, P10 and P11 remain.
- §6's mapping (line 245): `c5a985d` (GAP-P) → P7, **discharged**.
- Record the **CR 614.1b skips correction** where the census's §3.2 placement is referenced, so a later phase does not build a `SkipsDrawStep` on the wrong carrier.

- [ ] **Step 7: Commit**

```bash
git add docs/progress.md CLAUDE.md docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md source/library/Pawl
hooky fix
git add -u
hooky run
git commit -m "docs(m4.5-p7): completion note, umbrella tick, CLAUDE.md status

Eleven deferrals filed as issues and cited at their code sites; #3 and
GAP-P closed, #4 (P8) left open, #38/#39/#40 cited not retired with
SpellCriterion joining them. The umbrella's 'resolved by the projection
over players' claim is corrected -- CR 613.10/613.11 put this axis
outside the layer system -- and the census's placement of turn-structure
skips under PlayerEffect is corrected to CR 614.1b's replacement
effects. Cluster 3 is closed.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 8: Close the milestone issue**

```bash
gh issue close 3 --comment "Landed. See docs/progress.md for the completion entry and docs/superpowers/plans/2026-07-22-p7-player-effects.md for the executed plan."
```

- [ ] **Step 9: Confirm the plan is complete**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p7-player-effects.md`
Expected: `0`.

---

## Spec coverage map

| Spec section | Where it lands |
|---|---|
| §0 the seven falsifiers | 1 → Task 3; 2 → Task 3 (both the `Cast.castable` gate and the destroy case); 3 and 4 → Task 8; 5 → Task 4; 6 → Task 5; 7 → Task 6 |
| §2.1 `PlayerEffect`, and why increase and reduce are separate | Task 1 (the type), Task 4 (`costAdjustments` keeps them apart), Task 5 (the order test) |
| §2.2 `PlayerScope`, resolved against the controller, never frozen | Task 1 (the type), Task 3 (`inScope`), Task 7 (the stored carrier keeps it dynamic), Task 8 (the CR 611.2c falsifier) |
| §2.3 `SpellCriterion`, the third criterion sibling reading the projection | Task 1 (the type), Task 4 (`matchesSpell`) |
| §2.4 the printed carrier | Task 1 |
| §2.4 the stored carrier, its baked controller and its stamped timestamp | Task 7 |
| §2.5 `Pawl.PlayerEffect` as the sole casing home; `prohibitsCasting` | Task 3 |
| §2.5 `costAdjustments` | Task 4 |
| §2.5 `maximumHandSize` | Task 6 |
| §2.6 `Pawl.Cost` and CR 601.2f's three steps | Task 4 (steps 1–3, plus the CR 118.7a floor), Task 5 (the order, end to end) |
| §2.7 `GameEvent.SpellCast`, and why the count is free | Task 2 (the event), Task 3 (`castsThisTurn` and the turn-handoff test) |
| §2.8 the six read sites | `Cast.castable` and `castableWhileSearching`: Tasks 3 and 4; `Cast.castSpell`: Tasks 2 and 4; `Engine.discardToHandSize`: Task 6; `Pawl.Expiry`: Task 7; `Pawl.Resolve`: Task 8 |
| §2.9 the `AffectPlayers` opcode | Task 8 |
| §2.10 `Pawl.Expiry`'s third carrier | Task 7 |
| §2.11 serialization: codecs for the card data, none for the stored record | Tasks 1 and 8 (codecs), Task 7 (`ActivePlayerEffect` deliberately gets none), Task 9 Step 3 (the `grep -rn 'ActivePlayerEffect' data/` audit) |
| §3 the two invariants | Task 9 Step 3's first two greps (the casing surface, the untouched layer system); the no-choice half is recorded in Task 9 Step 4 |
| §4 what the phase does not touch | Task 9 Step 3's second and fourth greps |
| §5 Rule of Law's four tests | Task 3 (six cases, covering all four) |
| §5 Silence's three tests | Task 8 |
| §5 Thalia's three tests | Task 4 |
| §5 Sapphire Medallion's four tests | Task 5 |
| §5 Reliquary Tower's two tests | Task 6 (plus the CR 305.7 case, departure 3) |
| §5 the codec round-trips | Tasks 1, 2 and 8 (unit arms); the five card files via `CardsSpec.checkFile` as soon as each is registered |
| §6 module and type inventory | Tasks 1, 3, 4, 7, 8 |
| §7 the phase's own ordering | Tasks 1–8, in that order, with §7's step 1–2 merged into Task 1 and its step 7 split into Tasks 7 and 8 |
| §8 the eleven deferrals with named expiries | Task 9 Steps 1–2 |
| §9 tracking | Task 9 Steps 4–6 and 8 |
| §10 exit criterion | Task 9 Step 3 |
