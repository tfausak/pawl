# A schema for every codec, proven on seven types (#1225)

## The problem

Nearly every Pawl type has an `encode`/`decode` pair in `pawl:codec` and no
machine-readable description of what it reads. A card author has the Haskell or
nothing. #1225 asks for a JSON schema alongside the pair, and for the three to
travel together as one value:

``` hs
data Codec a = MkCodec
  { encode :: a -> Value
  , decode :: Value -> Either Text a
  , schema :: Schema
  }
```

That reshapes every module in `pawl:codec`. This design does seven of them and
stops, so the shape is judged on real types before it is copied onto the rest.

## Scope

In: two new sublibraries, the `Codec` bundle, the combinators that derive a
schema from a record's fields and a sum's arms, and seven converted modules
covering every shape the rest will need -- scalar newtype, nullary enum, sum
with payload arms, and a record with both a required and a defaulted field.

Out: the rest of `pawl:codec`; schema validation in the test suite; any command
that emits a schema. Each gets an issue.

The slice is deliberately non-recursive, so `define`'s cycle-breaking is built
but not exercised on a real type. That is stated again under *What this does not
retire*, because it is the thing most likely to be misread as proven.

## The libraries

Two new sublibraries, inserted between `json` and `codec`:

```
spec
  json           Value, encode, decode
    json-schema  Schema, SchemaM, define, run
      json-codec Common, Codec, Fields, Arm
        codec    CardName, PhasePattern, ... and nothing else
```

`pawl:json-schema` is a peer of `pawl:json-pointer`: an RFC-adjacent format
modelled over `Pawl.Json.Value`, with no knowledge of Pawl's types or of the
codec's conventions.

`pawl:json-codec` holds the shape of a codec and the generic helpers.
`pawl:codec` is left holding codecs for `pawl:types` types, one module each,
which is what its name has always claimed.

Both stanzas need their scope comment in `pawl.cabal` and their row in
`CLAUDE.md`'s table.

### `Pawl.Codec.Common` moves

`Pawl.JsonCodec.Common` is `Pawl.Codec.Common` moved verbatim. It has to move:
`Fields.object` and `Arm.tagged` need `object`, `asObject`, `field`, `tagged`
and `asTagged`, so the module cannot sit above `json-codec`. Its own header
already says nothing in it names a `Pawl.Types` type, and its imports still bear
that out, so the move is a rename with no body changes.

253 files import it, every one as the identical line, and the alias stays
`Common`:

``` hs
-import qualified Pawl.Codec.Common as Common
+import qualified Pawl.JsonCodec.Common as Common
```

The only other mentions are five lines, in `Common.hs`, `CommonSpec.hs` and the
test suite's `Main.hs`. `git diff --stat` reading 253 files at one line each,
plus those three, is the blast-radius check CLAUDE.md asks for before staging a
scripted edit.

## The schema

``` hs
-- Pawl.JsonSchema.Schema
newtype Schema = MkSchema { unwrap :: Value.Value }

-- Pawl.JsonSchema.Define
type SchemaM = State.State (Map.Map String (Maybe Schema.Schema))

define :: String -> SchemaM Schema.Schema -> SchemaM Schema.Schema
run :: SchemaM Schema.Schema -> Value.Value
```

A JSON schema is modelled as a plain `Value`, per #1225. There is more structure
available, but nothing downstream would read it.

`define name body` registers `name`, returns `{"$ref": "#/$defs/<name>"}`, and
skips `body` entirely if `name` is already present or in progress -- which is
what breaks a cycle. `Nothing` in the map marks in-progress, `Just` a finished
definition. The map holds `Maybe Schema` rather than `Maybe Value`: the invariant
that a half-built definition is not a usable schema lives in exactly this map,
and the newtype is what keeps it from being confused with an arbitrary value.

`run` produces the document:

``` json
{ "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$ref": "#/$defs/PhasePattern",
  "$defs": { "PhasePattern": {...}, "PhaseSelector": {...}, ... } }
```

Every named `Pawl.Types` type gets a `$defs` entry, including scalars like
`PlayerId`. That is the documentation payload: a reader who sees
`{"$ref": "#/$defs/PlayerId"}` learns the name of the thing, which inlining
`{"type": "integer"}` would throw away. Only structural wrappers -- `Maybe`,
arrays -- inline.

`Pawl.JsonSchema.Schema` exports generic constructors only: `string`, `integer`,
`natural`, `object`, `oneOf`, `constant`, `nullable`, `withDefault`. Pawl's
tagged-object convention is not a JSON Schema concept and is composed from these
in `json-codec`, where it belongs.

## The bundle

``` hs
-- Pawl.JsonCodec.Codec
data Codec a = MkCodec
  { encode :: a -> Value.Value,
    decode :: Value.Value -> Either Text.Text a,
    schema :: Define.SchemaM Schema.Schema
  }

maybe :: Codec a -> Codec (Maybe a)
```

`schema` is `SchemaM Schema`, not `Schema`, from the start. The field type is
what a later change would be most expensive to alter, since every `codec` value
in the tree is written against it, and recursion is the known reason the plain
form fails.

`Codec`-shaped combinators live beside the type; the function-shaped ones stay in
`Common`. As the full pass proceeds, `Common` drains into `Codec` -- `encodeSeq`
and `decodeSeq` become one `Codec.seq`, and so on -- and `Common` emptying is the
signal that the pass is finished.

## Records: derived, through `ApplicativeDo`

``` hs
-- Pawl.JsonCodec.Fields
data Fields o a = MkFields
  { encodeFields :: o -> [Pair.Pair Value.Value],
    decodeFields :: [Pair.Pair Value.Value] -> Either Text.Text a,
    schemaFields :: Define.SchemaM ([Pair.Pair Value.Value], [String])
  }

required :: String -> Codec.Codec a -> (o -> a) -> Fields o a
defaulted :: (Eq a) => String -> a -> Codec.Codec a -> (o -> a) -> Fields o a
object :: String -> Fields o o -> Codec.Codec o
```

`Functor` maps `decodeFields` only; the other two fields are contravariant in
`o` and untouched. `Applicative` concatenates all three. There is no `Monad`
instance and there cannot be one, which is load-bearing below.

``` hs
-- Pawl.Codec.PhasePattern
codec :: Codec.Codec PhasePattern.PhasePattern
codec = Fields.object "PhasePattern" $ do
  p <- Fields.required "whichPhase" PhaseSelector.codec PhasePattern.whichPhase
  w <- Fields.defaulted "whosePhase" Nothing (Codec.maybe PlayerId.codec) PhasePattern.whosePhase
  pure
    PhasePattern.MkPhasePattern
      { PhasePattern.whichPhase = p,
        PhasePattern.whosePhase = w
      }
```

`encode`, `decode`, `properties`, `required` and `default` all fall out of that
one block. The default binding appears once: `Common`'s header currently
documents an invariant held by hand between `optionalPair` and `defaultedField`
-- that both must be passed the same binding or a round trip stops being the
identity -- and this collapses it to a single argument. The schema's `"default"`
is `Codec.encode` applied to that same argument, so it cannot disagree either.

`ApplicativeDo` gets the block `docs/style-guide.md` asks for under *Prefer
monads for building records*, without the `(<$>)`/`(<*>)` that section rejects.
It needs adding to `.hlint.yaml`'s allowlist with its reason. Because `Fields o`
has no `Monad` instance, a bind that depends on an earlier one fails to compile
rather than silently desugaring through `join`: the independence the encoder
relies on is checked, not assumed.

## Sums: schema derived, `encode` written out

``` hs
-- Pawl.JsonCodec.Arm
data Arm a where
  MkNullary :: String -> a -> Arm a
  MkPayload :: String -> Codec.Codec b -> (b -> a) -> Arm a

tagged :: String -> (a -> Value.Value) -> [Arm a] -> Codec.Codec a
```

`decode` and `schema` derive from the arm list. `encode` is passed in as a
hand-written total `case`, byte-identical to today's `toJson` body.

That asymmetry is deliberate. Deriving `encode` too needs an `a -> Maybe b`
projection per arm, and then a constructor added to `Pawl.Types.PhaseSelector`
compiles clean and silently stops being encodable. Today's `case` is
exhaustiveness-checked, and `-Wincomplete-patterns` catching a new constructor is
worth more than collapsing a table that is already written twice. CLAUDE.md's
*find the sites `-Werror` won't* section exists because this repository has lost
that check before. The tag stays written twice, exactly as today; the schema is
the thing gained.

The existential in `MkPayload` uses GADT syntax, which `.hlint.yaml` already
allows.

## Wire shapes

| Haskell | JSON | Schema |
| --- | --- | --- |
| `newtype PlayerId = MkPlayerId Natural` | `2` | `{"type":"integer","minimum":0}` |
| `data EndingStep = EndStep \| Cleanup` | `{"type":"EndStep"}` | `oneOf` of const-tagged objects |
| `Phase.Combat CombatStep` | `{"type":"Combat","value":{...}}` | arm object with a `$ref` value |
| `whosePhase :: Maybe PlayerId` | absent, `null`, or `2` | `oneOf [$ref, null]`, `"default": null`, not in `required` |

Two calls where the honest schema is the looser one:

- **No `additionalProperties: false`, anywhere.** `asObject` and `asTagged`
  ignore unknown keys, and nullary arms ignore a `"value"` key outright. A
  schema that forbade them would reject documents the decoder accepts, which is
  worse than saying less.

- **A defaulted field is nullable *and* optional.** `defaultedField` treats an
  absent key and an explicit `null` alike, so the schema has to permit both.

Neither is a limitation of the schema language; both are the decoder's actual
behaviour written down. A schema that is stricter than its decoder is a lie in
the direction that costs a card author an afternoon.

## The slice

The `PhasePattern` cluster, closed downward -- no module in it depends on an
unconverted one.

| Module | Shape | What it proves |
| --- | --- | --- |
| `BeginningStep` | nullary enum | `Arm.tagged`, all-nullary |
| `CombatStep` | nullary enum | same |
| `EndingStep` | nullary enum | same |
| `Phase` | sum, three payload arms | `$ref` composition, three ways |
| `PhaseSelector` | sum, payload and nullary arms | mixed arms in one `oneOf` |
| `PlayerId` | `newtype` over `Natural` | scalar, `minimum` |
| `PhasePattern` | record | `required` and `defaulted` together, `Maybe` |

Chosen over the mana cluster (`Color`/`ManaType`/`ManaSymbol`/`ManaCost`) and
over a minimal three-module set because it is the only small cluster carrying a
record with both a required and a defaulted field, and `Maybe` is where encode,
decode and schema most easily disagree.

Each converted module exports `codec` and drops `toJson`/`fromJson`; no aliases
are kept, per *No API stability obligations*.

## Consumers

These modules read the converted pairs and change mechanically -- `X.toJson`
becomes `Codec.encode X.codec`:

- `PhaseSelector`: `Effect`, `Expiry`, `ActivationRestriction`
- `PlayerId`: `DelayedTrigger`, `Expiry`, `Modification`, `Countering`,
  `DamageEvent`, `GameEvent`, `Recipient`
- `Phase`: `GameEvent`, `TriggerCondition`, `CastingRestriction`
- `PhasePattern`: `ReplacementEffect`

## Tests

The converted specs keep their existing cases, moved to a
`Common.assertCodec s codec x j` that reads `encode` and `decode` off the bundle.

Each converted spec gains exactly one schema case:

``` hs
Spec.it s "has a schema" $
  Spec.assertBool
    s
    (Either.isRight (Common.asObject (Define.run (Codec.schema PhasePattern.codec))))
    "expected an object"
```

It asserts nothing about content, so editing a schema never edits a test -- but
it forces the value, so a bottom fails, and a `define` that fails to terminate
fails on the suite's `--timeout 1s`. Schemas themselves are reviewed by eye from
the PR body, which carries all seven rendered.

`Pawl.JsonSchema.DefineSpec` is the one place a literal is cheap and stable: a
synthetic self-referential `define`, asserting the cycle breaks into a `$ref`
instead of diverging. That tests the machinery, not any type's schema.

Golden schemas per type and a schema validator were both considered and
rejected for now -- see *Alternatives rejected*.

## Verification

- Suite green, count reported before and after in the PR body.
- Mutation, per CLAUDE.md: break `define`'s already-present check so a cycle
  diverges, and confirm `DefineSpec` fails on the timeout; drop a field from
  `PhasePattern`'s block and confirm the round-trip case fails. Put both back.
- `git diff --stat` confirming the `Common` move is 253 files at one line each,
  plus `Common.hs`, `CommonSpec.hs` and `Main.hs`.
- `ormolu --mode check $(git ls-files '*.hs')` repo-wide, since `hooky fix` only
  reaches staged files.
- `cabal-gild pawl.cabal` directly, since modules are added and moved.
- `-Werror` blind spots to grep and report on: the codec's `Common` fallthroughs
  and any `{}`/`_` pattern over `Value.Value` that a new schema constructor
  could slip past.

## What this does not retire

Two risks survive the slice, and the PR says so rather than implying coverage:

- **Recursion is built but unexercised.** `Card`/`Face`, `Filter` and `Effect`
  are the recursive knots, and none is in the slice. `define` breaking a real
  cycle is proven only by `DefineSpec`'s synthetic case.
- **Parametric codecs are untouched.** `Face`, `Mode` and `Cost` take a `card`
  encoder today and would become `Codec card -> Codec (Face card)`. Their
  `define` name would have to vary with the parameter, or collide. Nothing in
  the slice says how.

## Deferred, each with an issue

- The rest of `pawl:codec`, including the two risks above and the `Common`
  drain.
- Schema validation in the test suite -- ajv, or a Haskell validator over the
  keyword subset -- so a schema that disagrees with its encoder fails.
- Emitting a schema: a `pawl` subcommand, a checked-in schema file, or both.
  This is what #1225 is ultimately for; the slice only makes it possible.

## Alternatives rejected

- **A `ToSchema` class derived through `GHC.Generics`**, as Scrod does. Neither
  `DeriveGeneric` nor `DataKinds` is on `.hlint.yaml`'s allowlist, and the whole
  codec is hand-written per module by design. Pawl writes schemas the way it
  writes codecs.
- **`schema :: Schema`, deferring `SchemaM`.** Smaller now; changes the field
  type of every codec in the tree later.
- **Golden schema literals per type.** Brittle in proportion to how much they
  would cover, and they prove only that a schema did not change, never that it
  agrees with its encoder.
- **A validator, now.** It is the check that would actually make schemas
  load-bearing, and it is a sublibrary's worth of work with its own test burden.
  Deferred rather than dismissed.
- **Projections per sum arm, to derive `encode`.** Costs the
  `-Wincomplete-patterns` check on every sum in the codec.
- **Splitting `Common` rather than moving it.** Nearly every codec module uses
  helpers from both halves, so most would import both modules: a larger diff and
  a worse resting place.
