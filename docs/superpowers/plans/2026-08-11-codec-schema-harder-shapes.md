# Harder Codec Shapes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the codec slice so every shape the first one left out — multi-payload arms, all five collections, a record-level decode check, parametric codecs and real mutual recursion — is exercised by at least one converted type.

**Architecture:** Three pieces of new machinery (`Schema`'s array vocabulary, `Common`'s `Codec`-shaped collection combinators, a validation hook on `Fields.object`), then sixteen modules converted in dependency order, ending with the `Filter`/`Keyword` knot that is the first real test of `define`'s cycle breaking.

**Tech Stack:** As the first slice. No new packages.

The design is `docs/superpowers/specs/2026-08-11-codec-schema-harder-shapes-design.md`; read it once before Task 1. It extends `2026-08-11-codec-schema-slice-design.md`, which is still accurate.

**Branch:** `1225-schema-harder-shapes`, stacked on `1225-schema-vertical-slice` (PR #1266). After #1266 squash-merges, rebase with `git rebase --onto main 1225-schema-vertical-slice 1225-schema-harder-shapes`.

## Global Constraints

Identical to the first slice's, and they are not repeated at length here because they have not changed. In brief: `-Weverything` plus `-Werror` including `-Wunused-imports`, `-Wunused-packages`, `-Wredundant-constraints` and `-Wincomplete-patterns`; extensions only from `.hlint.yaml`'s allowlist; `fromIntegral`/`fromInteger`/`realToFrac`/`toEnum` banned, conversions live in `Pawl.Extra.<SourceType>`; `"Avoid lambda"` and `"Use >=>"` are disabled because the project prefers explicit over point-free; `exposed-modules` is `cabal-gild`-generated; `Mk` prefixes wrappers and bare names mark sum alternatives; comments terse and accurate; no compat shims.

Operationally: run `cabal` in the **foreground**, never piped; `cabal test --test-options '--timeout 5s --hide-successes'`; another session shares the GHC semaphore, so `--no-semaphore -j1` on `semWait: invalid argument` and never `pkill -f cabal`; `sed` here is GNU, not BSD; `ormolu --mode check $(git ls-files '*.hs')` before pushing; `git add` explicit paths.

**Starting test count: 4185.**

---

## On transcription

The first slice's plan transcribed every module body, and three of those transcriptions did not compile — a banned function, a monomorphism-restriction trip, and a missing instance. This plan does not repeat that. Seven converted modules already exist in the tree, reviewed and green, and they are a more reliable specification than a paraphrase:

| Shape | Copy the pattern from |
| --- | --- |
| Nullary enum | `source/libraries/codec/Pawl/Codec/BeginningStep.hs` |
| Sum with payload and nullary arms | `source/libraries/codec/Pawl/Codec/Phase.hs` |
| Scalar newtype | `source/libraries/codec/Pawl/Codec/PlayerId.hs` |
| Record with required and defaulted fields | `source/libraries/codec/Pawl/Codec/PhasePattern.hs` |
| Converted spec | any of `*Spec.hs` beside those |

Where this plan gives code, it is because the shape is **new** and has no example. Where it names a pattern, read the example.

---

## File Structure

**New machinery** — `source/libraries/json-schema/Pawl/JsonSchema/Schema.hs` and `SchemaSpec.hs`; `source/libraries/json-codec/Pawl/JsonCodec/Common.hs` and `CommonSpec.hs`; `source/libraries/json-codec/Pawl/JsonCodec/Fields.hs` and `FieldsSpec.hs`.

**Converted, in dependency order** — `Color`, `CardType`, `Supertype`, `PlayerRelation`, `MorphVariant`, `KeywordFamily`, `Subtype`, `ManaType`, `ManaSymbol`, `ManaCost`, `TypeLine`, `CounterKind`, `Filter`, `CostComponent`, `Cost`, `Keyword`, each with its `*Spec.hs`.

**Consumers** — about 31 modules outside the closure, plus `source/test-suite/Pawl/CardSpec.hs`. Each is a mechanical `X.toJson` → `Codec.encode X.codec` swap; find them per task with `grep -n 'X\.\(toJson\|fromJson\)' source/`.

---

### Task 1: The array vocabulary in `Schema`

**Files:** modify `source/libraries/json-schema/Pawl/JsonSchema/Schema.hs`, `SchemaSpec.hs`.

**Interfaces produced:**
- `boolean :: Schema`
- `array :: Schema -> Schema` — `{"type":"array","items":…}`
- `uniqueArray :: Schema -> Schema` — as `array`, plus `"uniqueItems": true`
- `tupleOf :: [Schema] -> Schema` — `{"type":"array","prefixItems":[…],"minItems":n,"maxItems":n}`

- [ ] **Step 1: Write the failing tests.** Add cases to `SchemaSpec` in the style already there — assert the built `Value` against an expected `Value` composed from `Schema.pair`/`obj`/`arr`. One case each for `boolean`, `array`, `uniqueArray`, and `tupleOf` with two elements. `tupleOf` must pin **both** `minItems` and `maxItems` to the list's length; a tagged arm's payload array is a fixed shape, not a list that happens to be short.
- [ ] **Step 2: Run and watch them fail.** `cabal test --test-options '--timeout 5s --hide-successes'` — expect "not in scope".
- [ ] **Step 3: Implement**, following the existing constructors' shape (`fromPairs`, `pair`, `text`, `integerValue`). `tupleOf`'s bounds come from `length`, which needs `Pawl.Extra.Int.toNatural`-style care only if the count reaches a `Natural` — it does not; `integerValue` takes an `Integer`, and `Int` → `Integer` conversion must not use `fromIntegral`. `toInteger` is total and unbanned.
- [ ] **Step 4: Run and watch them pass.**
- [ ] **Step 5: Mutate.** Drop `maxItems` from `tupleOf`; confirm its case fails; restore.
- [ ] **Step 6:** `ormolu --mode check $(git ls-files '*.hs')`, then commit.

---

### Task 2: Collection combinators in `Common`

**Files:** modify `source/libraries/json-codec/Pawl/JsonCodec/Common.hs`, `CommonSpec.hs`.

**Consumes:** Task 1's `array`, `uniqueArray`, `tupleOf`.

**Interfaces produced**, each wrapping the `encodeX`/`decodeX` pair `Common` already owns rather than reimplementing it:
- `tuple :: Codec.Codec a -> Codec.Codec b -> Codec.Codec (a, b)` — encodes to a two-element array via `Common.array`, schema `tupleOf [a, b]`
- `list :: Codec.Codec a -> Codec.Codec [a]`
- `set :: (Ord a) => Codec.Codec a -> Codec.Codec (Set.Set a)` — schema `uniqueArray`, the others `array`
- `seq :: Codec.Codec a -> Codec.Codec (Seq.Seq a)`
- `nonEmpty :: Codec.Codec a -> Codec.Codec (NonEmpty.NonEmpty a)`
- `multiset :: (Ord a) => Codec.Codec a -> Codec.Codec (Map.Map a Natural.Natural)`

`tuple` decodes a two-element array and **fails on any other length** — the existing `ManaSymbol` decoder pattern-matches `Value.Array (Array.MkArray [av, bv])`, so a three-element array is a decode error today and must stay one.

- [ ] **Step 1: Write the failing tests** in `CommonSpec`, which already has the house style. One round trip each through `Common.assertCodec` with a small element codec (`Common.scalar Schema.integer Common.integer Common.asInteger` is the one Tasks 7 and 8 of the first slice used). Include: `tuple` rejecting a one-element and a three-element array; `nonEmpty` rejecting `[]`; `multiset` recounting repeats and round-tripping to ascending order; `set` deduplicating.
- [ ] **Step 2: Run and watch them fail.**
- [ ] **Step 3: Implement.** Follow `Common.maybe`, which is the model: read the pair off the `Codec`, hand it to the existing `encodeX`/`decodeX`, and `fmap` the schema. Add a one-line comment on the group citing `(#1263)` — the function-shaped halves die when their last caller converts. Do not write an expiry.
- [ ] **Step 4: Run and watch them pass.**
- [ ] **Step 5: Mutate.** Change `set`'s schema from `uniqueArray` to `array`; confirm **nothing fails**, because schema content is unasserted by design; say so in the report rather than adding an assertion. Then change `nonEmpty` to accept `[]`; confirm its case fails; restore both.
- [ ] **Step 6:** format and commit.

---

### Task 3: The validation hook on `Fields`

**Files:** modify `source/libraries/json-codec/Pawl/JsonCodec/Fields.hs`, `FieldsSpec.hs`.

**Interfaces produced:**
```hs
objectWith :: forall o. (Typeable.Typeable o) => (o -> Either Text.Text o) -> Fields o o -> Codec.Codec o
object :: forall o. (Typeable.Typeable o) => Fields o o -> Codec.Codec o
object = objectWith pure
```

`objectWith`'s check runs **after** `decodeFields` assembles the record and on the decode side only — encoding cannot fail. `object` keeps its exact current behaviour and signature, so no existing caller changes.

**This must not become a `Monad`.** `Fields` is `Applicative` with no `Monad` deliberately, so a field cannot depend on an earlier field. What was missing is an escape hatch for validating the *whole assembled record*; that is a different thing from a bind, and the module comment should say which.

- [ ] **Step 1: Write the failing test.** `FieldsSpec` has an `Example` record already. Add a second codec over it built with `objectWith`, rejecting (say) a zero `size`, and assert both that a valid value round trips and that the invalid one decodes to `Left`. Keep the existing `codec` and its cases untouched — they are the evidence `object`'s behaviour did not change.
- [ ] **Step 2: Run and watch it fail.**
- [ ] **Step 3: Implement**, and update the module header to say what `objectWith` is for and why it is not a `Monad`.
- [ ] **Step 4: Run and watch it pass.**
- [ ] **Step 5: Mutate.** Make `objectWith` ignore its check; confirm the rejection case fails; restore. Then confirm `object = objectWith pure` really is behaviour-preserving by checking the pre-existing `Example` cases still pass untouched.
- [ ] **Step 6:** format and commit.

---

### Task 4: The six small enums

**Convert:** `Color`, `CardType`, `Supertype`, `PlayerRelation`, `MorphVariant`, `KeywordFamily`.

Five are nullary enums — copy `BeginningStep`. `KeywordFamily` (64 lines) has arms carrying payloads; copy `Phase`. Check each against its `Pawl.Types` module: every constructor present, every tag string identical, `encode` a total `case`.

**Consumers:** `grep -n '\(Color\|CardType\|Supertype\|PlayerRelation\|MorphVariant\|KeywordFamily\)\.\(toJson\|fromJson\)' source/`. Several are modules this plan converts later — update their call sites now and convert them in their own task.

- [ ] **Step 1:** convert all six, copying the named patterns.
- [ ] **Step 2:** update every consumer call site.
- [ ] **Step 3:** update the six specs — swap the assertion helper, keep **every existing value and JSON literal byte for byte**, add one `"has a schema"` case each.
- [ ] **Step 4:** `cabal test --test-options '--timeout 5s --hide-successes'`; expect +6 cases.
- [ ] **Step 5: Mutate.** Change one tag string in `Color`; confirm a decode case fails; restore.
- [ ] **Step 6:** format and commit.

---

### Task 5: `Subtype`

544 nullary constructors, 1103 lines, and no difficulty — the module already writes the table twice (a `case` in `toJson`, a table in `fromJson`), and conversion writes it twice again (a `case` and an arm list). Script the transformation rather than typing it, then **read the diff stat and confirm the shape before staging**, per CLAUDE.md's rule on scripted edits.

- [ ] **Step 1:** convert, by script.
- [ ] **Step 2: Verify the blast radius.** The new file should have 544 `Arm.nullary` lines and 544 `case` arms. `grep -c 'Arm.nullary' ` and a constructor count from `Pawl.Types.Subtype` must agree; if they do not, stop.
- [ ] **Step 3:** update consumers (`Filter`, `ProjectedCharacteristics`, `Modification`, `Effect`, `TypeLine`, and `source/test-suite/Pawl/CardSpec.hs`).
- [ ] **Step 4:** update `SubtypeSpec` as in Task 4.
- [ ] **Step 5:** run the suite; expect +1 case and no other movement.
- [ ] **Step 6: Mutate.** Delete one arm from the list; confirm a decode fails; restore.
- [ ] **Step 7:** format and commit.

---

### Task 6: The mana cluster — the multi-payload arm

**Convert:** `ManaType`, `ManaSymbol`, `ManaCost`.

This is the first **multi-payload arm** and the first **newtype over a list**.

`ManaSymbol.Hybrid a b` uses no new `Arm` constructor. It composes Task 2's `tuple` with the existing single-payload arm:

```hs
Arm.payload "Hybrid" (Common.tuple ManaType.codec ManaType.codec) (uncurry ManaSymbol.Hybrid)
```

and its `encode` case keeps writing `Common.array [ … , … ]` exactly as today, so the JSON does not move.

`ManaCost` is a newtype over `[ManaSymbol]`. It is **not** a `Common.scalar` — its schema is `Common.list ManaSymbol.codec`'s, so build it as a plain `Codec.MkCodec` wrapping `Common.list`, or via `Common.scalar` only if the schema comes out identical. State which you chose and why.

- [ ] **Step 1:** convert the three, in that order.
- [ ] **Step 2:** update consumers (`ManaProduction`, `ManaFilter`, `Face`, `Cost`, `PlayerEffect`, `AttackCost`).
- [ ] **Step 3:** update the three specs as in Task 4. `ManaSymbolSpec` must keep its `Hybrid` case's literal exactly — it is the proof the tuple encoding is unchanged.
- [ ] **Step 4:** run the suite; expect +3 cases.
- [ ] **Step 5: Mutate.** Swap `Hybrid`'s two `ManaType` arguments in the arm's injection; confirm a round trip fails; restore. If the type checker catches it instead, say so — both arguments have the same type, so it should genuinely run.
- [ ] **Step 6:** format and commit.

---

### Task 7: `TypeLine` — sets and the record-level check

**Convert:** `TypeLine`, using Task 2's `Common.set` and Task 3's `Fields.objectWith`.

The check is CR 205.1: a type line with no card types is a malformed file, not a card with no types. It moves from a `Monad.when (Set.null tys) . Left` between two field reads into `objectWith`'s hook, and the existing comment explaining it must come with it.

Keep the schema silent about it. JSON Schema *could* say `minItems: 1` on `types`, but the rule is CR 205.1 and the schema describes the wire format.

- [ ] **Step 1:** convert.
- [ ] **Step 2:** update its one consumer, `Face`.
- [ ] **Step 3:** update `TypeLineSpec` as in Task 4. It already has cases for the empty-`types` rejection and the absent-`types` rejection — **those must still pass, unchanged**, and they are the proof the check survived the move.
- [ ] **Step 4:** run the suite; expect +1 case.
- [ ] **Step 5: Mutate.** Make `objectWith`'s check always succeed; confirm the two rejection cases fail; restore.
- [ ] **Step 6:** format and commit.

---

### Task 8: `CounterKind` — the first parametric codec

**Convert:** `CounterKind`, whose codec becomes
`codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (CounterKind.CounterKind keyword)`.

It depends on nothing else in the closure, which is why it comes before the knot: it proves the parametric shape in isolation, so that if the knot misbehaves the cause is the recursion and not the parameter.

Its consumers pass a keyword codec. `Keyword` is not converted yet, so at this point its callers must still bridge — **if that cannot be done honestly, stop and report**: it may be that `CounterKind` has to move into Task 9's commit with the rest of the knot. Check before writing code, and say which you found.

- [ ] **Step 1:** check whether `CounterKind`'s consumers can call it without a `Codec Keyword`. Report the answer before proceeding.
- [ ] **Step 2:** convert, or fold into Task 9 and say so.
- [ ] **Step 3–6:** consumers, spec, suite, mutate, format, commit — as in Task 4.

---

### Task 9: The knot — `Filter`, `CostComponent`, `Cost`, `Keyword`

**These four convert in one commit.** `Keyword` → `Cost` → `CostComponent` → `Filter` → `Keyword`. There is no ordering that lands them separately, and an adapter wrapping an unconverted `Keyword` would have to fabricate a schema.

This is the task the whole extension exists for: the first time `define` breaks real mutual recursion, and the first `Codec keyword -> Codec (Filter keyword)`.

Signatures:
```hs
Filter.codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (Filter.Filter keyword)
CostComponent.codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (CostComponent.CostComponent keyword)
Cost.codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (Cost.Cost keyword)
Keyword.codec :: Codec.Codec Keyword.Keyword
```

`Keyword.codec` is **self-referential** — defined partly as `Filter.codec Keyword.codec`. That is a legal top-level binding; Haskell's laziness ties it at the value level and `define` breaks it at the schema level. No fixpoint combinator is needed. If it diverges, the fault is in `define`'s registration order, not in the binding.

`Filter.And`/`Or` carry `[Filter keyword]` — Task 2's `Common.list` applied to the codec being defined, another self-reference.

Three multi-payload arms in `CostComponent` and three in `Keyword` use `Common.tuple`.

**Two behaviour changes to expect and report, not to suppress:**
- `Filter`'s unknown-tag error becomes `unknown Filter_Keyword: X` rather than `unknown Filter: X`, because the name is `Typeable`-derived and now names the instantiation. Nothing asserts it.
- A known tag missing its `value` reports `"missing tagged value"` rather than falling through to the unknown-tag message, as in the first slice.

- [ ] **Step 1:** convert all four together.
- [ ] **Step 2:** update every consumer — about 31 modules. `grep -n '\(Filter\|Cost\|CostComponent\|Keyword\)\.\(toJson\|fromJson\)' source/`. Verify repo-wide that none is missed.
- [ ] **Step 3:** update the four specs as in Task 4.
- [ ] **Step 4:** run the suite. Expect +4 cases. **If it hangs rather than fails, the recursion did not terminate** — that is the interesting failure and it should be reported in full, not worked around.
- [ ] **Step 5: Render `Keyword`'s schema and read it.** It is the first schema in the project produced by real mutual recursion. Confirm `$defs` contains both `Keyword` and `Filter_Keyword`, that each references the other, and that the document is finite. Paste it into the report.
- [ ] **Step 6: Mutate.** In `Pawl.JsonSchema.Define`, move `define`'s registration after its body; confirm rendering `Keyword`'s schema now hangs and the suite fails on its timeout; restore. This is the proof the first slice could only make synthetically.
- [ ] **Step 7:** format and commit.

---

### Task 10: Close out

- [ ] **Step 1: Render all sixteen schemas** and read them. The five checks from the first slice apply: correct `required` lists, nullable-and-defaulted fields shaped right, no `additionalProperties` anywhere, every `$ref` resolving within `$defs`, and enums faithful to their constructors. Add two: every array-valued schema has `items` or `prefixItems`, and every `Set` field says `uniqueItems`. Paste all sixteen into the report. **If a check fails, stop and report** — this is still the only check on schema content.
- [ ] **Step 2: Update #1263** with what remains: `tuple3`/`tuple4` (9 arms, all in `GameEvent`), the `Common` drain, and the ~230 modules still unconverted. Note that the parametric and recursive risks it carried are now retired.
- [ ] **Step 3: Self-review the branch.** Re-read every comment the change touched. Grep for `{}` and `_` patterns over `Value.Value` and `Arm` and record which you read.
- [ ] **Step 4: Verify.** `cabal-gild pawl.cabal`, `ormolu --mode check $(git ls-files '*.hs')`, `cabal build`, `cabal test --test-options '--timeout 5s --hide-successes'`.
- [ ] **Step 5: Open a draft PR** against `1225-schema-vertical-slice`, not `main` — this branch is stacked. Body carries: what changed and why; that it extends #1266 and must merge after it; the two error-text changes; that schema content is still unasserted (#1264); the sixteen rendered schemas in `<details>`; and an explicit "no" on whether the rules core cases on an effect's identity. Do **not** mark it ready — a whole-branch review runs first.

---

## Self-Review

**Spec coverage.** Multi-payload arms → Task 6 (`ManaSymbol.Hybrid`) and Task 9. All five collections → Task 2, exercised by Tasks 6 (`list`), 7 (`set`) and 9 (`list`). Record-level validation → Tasks 3 and 7. Parametric codecs → Tasks 8 and 9. Real recursion → Task 9. Array schema vocabulary → Task 1.

**Ordering.** Every module's dependencies convert before it. The one forced grouping is Task 9's knot, and Task 8 explicitly checks whether `CounterKind` belongs in it rather than assuming.

**Known risk.** Task 9 is the largest single task in either plan — four modules and ~31 consumers in one commit. It cannot be split without leaving the tree uncompilable. If it needs a fix loop, the loop will be slow.
