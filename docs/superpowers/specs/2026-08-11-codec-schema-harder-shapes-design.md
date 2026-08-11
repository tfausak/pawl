# The shapes the first slice left out (#1225)

Extends `2026-08-11-codec-schema-slice-design.md`. That design is unchanged; this
one adds what its own final review found missing.

## The problem

The first slice converted seven types and they were all the easy shapes: nullary
enums, single-payload sums, a scalar newtype, and a flat two-field record. The
review that closed it named what that leaves unproven, and the numbers are the
argument:

- **Multi-payload arms.** 48 arms encode a multi-argument constructor as
  `Common.tagged "X" (Just (Common.array [...]))`. `Arm.payload` takes exactly
  one codec, so none of them can be expressed.
- **Collections.** 57 fields use `encodeList`, `encodeSet`, `encodeSeq`,
  `encodeNonEmpty` or `encodeMultiset`. `Common.maybe` is the only
  `Codec`-shaped lifting the slice built, and `Pawl.JsonSchema.Schema` has no
  array vocabulary at all -- no `array`, `items` or `prefixItems`, and no
  `boolean` either.
- **A decode that fails on the assembled record.** `Pawl.Codec.TypeLine`
  rejects an empty `types` set (CR 205.1) between two field reads.
  `Applicative`-only `Fields` has nowhere to put that.
- **Parametric codecs**, which the first slice explicitly did not retire.
- **Recursion**, which `define` handles structurally but which no real type has
  ever exercised.

Each gets at least one real type here. The point is not the sixteen modules; it
is that the next pass inherits a template with no unanswered shape in it.

## What the closure actually is

Converting a module requires its dependencies converted, so the seeds pull in a
closure of sixteen:

| Module | Lines | Why it is in |
| --- | --- | --- |
| `Color`, `CardType`, `Supertype`, `PlayerRelation`, `MorphVariant` | 19-45 | nullary enums the rest need |
| `KeywordFamily` | 64 | enum, some arms carrying payloads |
| `Subtype` | 1103 | 544 nullary constructors; `Filter` and `TypeLine` both need it |
| `ManaType` | 20 | sum, payload and nullary arms |
| `ManaSymbol` | 35 | **multi-payload arm** (`Hybrid a b`), plus a `Natural` payload |
| `ManaCost` | 13 | **newtype over a list** |
| `TypeLine` | 31 | **`Set` fields and the only record-level decode check in the tree** |
| `CounterKind` | 34 | **parametric** |
| `CostComponent` | 61 | parametric, multi-payload arms |
| `Cost` | 28 | parametric, `Maybe` and list fields |
| `Filter` | 100 | **parametric and self-recursive** (`And [Filter keyword]`) |
| `Keyword` | 175 | **ties the knot** `Filter`'s parameter opens |

`Subtype` is 61% of the line count and none of the difficulty: 544 nullary
constructors, already written twice today (a `case` and a table), converting to
a `case` and an arm list of the same size. Script it, then read the diff stat.

### The knot is indivisible

`Keyword` needs `Cost`, `Cost` needs `CostComponent`, `CostComponent` needs
`Filter`, and `Filter`'s only instantiation is `Filter Keyword` --
`Pawl.Types.Keyword`'s own header says it "TIES THE KNOT that
`Pawl.Types.Filter`'s keyword parameter opens". Those five convert in one commit
or not at all. There is no ordering that lands them separately, and an adapter
that wrapped an unconverted `Keyword` in a `Codec` would have to invent a schema
for it, which is a lie rather than a shim.

That knot is also the whole point: it is the first time `define`'s cycle
breaking runs against real mutual recursion rather than `DefineSpec`'s synthetic
case, and the first time a codec is `Codec keyword -> Codec (Filter keyword)`.

## New machinery

### Array vocabulary, in `Pawl.JsonSchema.Schema`

```hs
boolean :: Schema
array :: Schema -> Schema                    -- {"type":"array","items":…}
uniqueArray :: Schema -> Schema              -- … plus "uniqueItems": true
tupleOf :: [Schema] -> Schema                -- prefixItems, minItems, maxItems
```

`tupleOf` pins both bounds, because a tagged arm's payload array is a fixed
shape rather than a list that happens to have two elements.

### Collections, in `Pawl.JsonCodec.Common`

```hs
tuple :: Codec.Codec a -> Codec.Codec b -> Codec.Codec (a, b)
list :: Codec.Codec a -> Codec.Codec [a]
set :: (Ord a) => Codec.Codec a -> Codec.Codec (Set.Set a)
seq :: Codec.Codec a -> Codec.Codec (Seq.Seq a)
nonEmpty :: Codec.Codec a -> Codec.Codec (NonEmpty.NonEmpty a)
multiset :: (Ord a) => Codec.Codec a -> Codec.Codec (Map.Map a Natural.Natural)
```

Each wraps the `encodeX`/`decodeX` pair `Common` already owns rather than
reimplementing it; each is the `Codec`-shaped half of a pair that #1263 will
eventually delete. A collection's schema should be as expressive as the
constraint it names, and the decoder is tightened to guarantee what the
schema claims rather than the schema being trimmed to whatever the decoder
happened to accept: `set` emits `uniqueItems` and `decodeSet` rejects a
repeated element to match; `nonEmpty` emits `minItems: 1` and `decodeNonEmpty`
already rejected `[]`. `multiset` is deliberately NOT tightened the same
way -- a repeat there is the encoding of a count (CR 122.1's counters), not a
mistake, so its schema carries no uniqueness keyword and its decoder keeps
recounting repeats.

`tuple` is arity 2 and that is deliberate: all seven multi-payload arms in this
closure are arity 2. Repo-wide there are 39 at arity 2, 7 at arity 3 and 2 at
arity 4 -- every one of the higher arities in `Pawl.Codec.GameEvent`, which this
change does not touch. Building `tuple3` and `tuple4` with no caller here would
be capability nobody exercises; #1263 carries them.

`ManaSymbol`'s multi-payload arm then needs no new `Arm` constructor at all:

```hs
Arm.payload "Hybrid" (Common.tuple ManaType.codec ManaType.codec) (uncurry ManaSymbol.Hybrid)
```

That is why `Arm` gains nothing in this change. A dedicated `Arm.payload2`
would have been a second way to say the same thing.

### A validation hook, in `Pawl.JsonCodec.Fields`

```hs
objectWith :: (Typeable.Typeable o) => (o -> Either Text.Text o) -> Fields o o -> Codec.Codec o
object :: (Typeable.Typeable o) => Fields o o -> Codec.Codec o
object = objectWith pure
```

The check runs after `decodeFields` assembles the record, and only on decode --
encoding cannot fail. This is the narrowest thing that answers the review's
question: `Fields` stays `Applicative` with no `Monad`, so a field still cannot
depend on an earlier field, and what was missing was an escape hatch for
*validation*, not a bind.

`TypeLine` is its only caller and the only record-level check in the tree:

```hs
codec = Fields.objectWith nonEmptyTypes $ do
  supertypes <- Fields.defaulted "supertypes" Set.empty (Common.set Supertype.codec) TypeLine.supertypes
  types <- Fields.required "types" (Common.set CardType.codec) TypeLine.types
  subtypes <- Fields.defaulted "subtypes" Set.empty (Common.set Subtype.codec) TypeLine.subtypes
  pure TypeLine.MkTypeLine {…}
```

The check is invisible to the schema, which is honest: JSON Schema could say
`minItems: 1` on `types`, but the decoder's rule is CR 205.1 and the schema
describes the wire format, not the rulebook.

## Parametric codecs

`Filter.codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (Filter.Filter keyword)`.

The `Typeable` constraint is what `Name.typeName` needs, and it is what makes
`Filter Keyword` and a hypothetical `Filter Something` take separate `$defs`
entries -- `Filter_Keyword`, not `Filter`. Two consequences, both worth stating
before they surprise someone:

- **The unknown-tag error text changes.** `Filter`'s decoder reports
  `unknown Filter: X` today and will report `unknown Filter_Keyword: X`. Nothing
  asserts these strings. The new one is arguably better -- it names the
  instantiation actually being decoded -- but it is a change, and #1263 already
  carries a note about it.
- **`Keyword.codec` is a self-referential binding.** It is defined partly in
  terms of `Filter.codec Keyword.codec`. Haskell's laziness ties that knot at
  the value level and `define` breaks it at the schema level; neither needs a
  fixpoint combinator. The proof it works is that rendering `Keyword`'s schema
  terminates, which the suite's timeout enforces.

## Testing

Unchanged in kind from the first slice: every converted spec keeps its existing
cases and JSON literals verbatim, swaps its assertion helper, and gains one
`"has a schema"` case. Schema *content* stays unasserted -- that is still the
owner's decision, and #1264 still carries the fix.

Two additions where the machinery is new and a literal is cheap and stable:
`SchemaSpec` covers the array vocabulary, and `CommonSpec` covers `tuple` and
the five collection combinators, since those have definite answers that no
per-type spec pins down.

One thing this change *does* get for free that the first slice could not: the
`Keyword` schema is the first real evidence that recursion terminates. Rendering
it is a test that fails by timing out, not by asserting.

## What this still does not retire

- `tuple3` and `tuple4` -- 9 arms, all in `GameEvent`.
- The `Common` drain: every `encodeX`/`decodeX` pair now has a `Codec`-shaped
  sibling, and the function-shaped half stays until its last caller converts.
- Schema content is still asserted nowhere (#1264).
- The remaining ~230 modules (#1263).

## Alternatives rejected

- **`Arm.payload2`/`payload3` for multi-payload arms.** `Common.tuple` composes
  with the existing single-payload `Arm.payload` and produces the same JSON, so
  a second spelling would be redundant.
- **A `Monad` instance on `Fields` to host the validation.** It would let a
  field depend on an earlier field, which is the thing the type is deliberately
  built to forbid; the check needed the whole record, not a bind.
- **Converting `Filter` without `Keyword`.** Its consumers need a
  `Codec Keyword` to pass, and an adapter over the unconverted pair would have
  to fabricate a schema.
- **`minItems: 1` on `TypeLine`'s `types`.** Expressible, but it states a rules
  constraint in a document that describes the wire format.
