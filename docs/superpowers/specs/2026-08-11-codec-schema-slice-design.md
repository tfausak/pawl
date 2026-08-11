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
  json             Value, encode, decode
    json-pointer   Pointer, Token, encode, encodeFragment
      json-schema  Schema, Name, SchemaM, define, run
        json-codec Common, Codec, Fields, Arm
          codec    CardName, PhasePattern, ... and nothing else
```

`pawl:json-schema` sits on `pawl:json-pointer` rather than beside it: a `$ref` is
a pointer, and the library that already models one correctly is the one that
should build it. Like its neighbour it is an RFC-adjacent format over
`Pawl.Json.Value`, with no knowledge of Pawl's types or the codec's conventions.

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

-- Pawl.JsonSchema.Name
newtype Name = MkName { unwrap :: String }

typeName :: (Typeable.Typeable a) => Proxy.Proxy a -> Name

-- Pawl.JsonSchema.Define
type SchemaM = State.State (Map.Map Name.Name (Maybe Schema.Schema))

define :: Name.Name -> SchemaM Schema.Schema -> SchemaM Schema.Schema
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

### Names are derived, not written

A definition's name comes from `Typeable`, never from a string literal. GHC has
auto-derived `Typeable` for every type since 7.10, so this costs `pawl:types`
nothing -- no `deriving` clause, no edit.

`typeName` renders the `TypeRep` structurally -- `tyConName`, then arguments
joined with `_` -- rather than `show`ing it. `show` writes an application with a
space, `Face Card`, and a `$ref` is a URI-reference where that would have to be
percent-encoded. Structural rendering gives `Face_Card`.

Names are unqualified. `Pawl.Types` is one type per module with the module named
for the type, so base names are unique by construction, and qualification would
lengthen every `$ref` in the document to buy nothing. `tyConModule` is there if
that convention ever breaks.

Parameterised types then name themselves: `Face Card` and `Face Token` are
distinct `TypeRep`s, so they take distinct `$defs` entries with no further
design. The cost is a `(Typeable card)` constraint on the parametric codecs,
which the slice does not reach.

### A `$defs` key and a `$ref` are not the same string

`tyConName` returns whatever the type constructor is called, verbatim. Haskell
admits operator type constructors, so `(:~/)` is a nameable type and `":~/"` is a
reachable definition name -- carrying both characters RFC 6901 reserves. Pawl
defines no such type today and has no plans to, but `Name` is derived from the
whole language, not from Pawl's habits.

So the two positions are written differently, and `Define` keeps them apart:

- **The `$defs` key is the raw name.** It is a JSON object key; nothing is
  escaped here.
- **The `$ref` is built as a `Pointer`**, never as a string:

  ``` hs
  reference :: Name.Name -> Schema.Schema
  reference name =
    schemaOf . Pointer.encodeFragment . Pointer.MkPointer $
      [Token.MkToken (Text.pack "$defs"), Token.MkToken (Name.unwrap name)]
  ```

  A `Token` stores unescaped text and `Pointer.encode` escapes on the way out,
  `~` before `/`, which is the ordering a hand-rolled escape gets wrong -- a `/`
  rewritten to `~1` otherwise has its own `~` rewritten again. That is already
  written, documented and tested in `pawl:json-pointer`, and this is what that
  library is for.

`Face Card` reaches none of it: `typeName` joins arguments with `_` first. The
`_` is a rendering choice; the escaping is correctness.

### One addition to `pawl:json-pointer`

`encodeFragment` is new. RFC 6901 section 6 defines a second representation of a
pointer -- the URI fragment identifier -- which prefixes `#` and percent-encodes
what RFC 3986 forbids in a fragment: `#`, `%`, `<`, `>`, `\`, `^` and `|`, every
one a legal Haskell operator character. `$ref` takes that form, not the plain
one.

It belongs in `json-pointer`, not here. The library claims RFC 6901, section 6 is
part of RFC 6901, and a schema library has no business knowing about URIs. It is
a short function beside `encode`, with its own case in `PointerSpec`, and it
leaves `json-pointer` covering the whole RFC rather than most of it.

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

`Pawl.JsonCodec.Codec` holds the type and nothing else. Combinators of both
shapes live in `Common`, which imports it: `Codec.maybe` wraps the
`encodeMaybe`/`decodeMaybe` pair rather than reimplementing it, and
`assertCodec` needs the record to read `encode` and `decode` off. Putting the
`Codec`-shaped ones beside the type instead would make `Codec` and `Common`
mutually recursive, or duplicate the element logic.

As the full pass proceeds, each function-shaped pair in `Common` collapses into
its `Codec`-shaped replacement -- `encodeSeq` and `decodeSeq` become one
`Codec.seq` -- and the last of them going is the signal that the pass is
finished.

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
object :: (Typeable.Typeable o) => Fields o o -> Codec.Codec o
```

`object` takes no name: `o` is fixed by its return type, so `typeName` supplies
one. A field's key is still a string, because a JSON key is not a Haskell name.

`Functor` maps `decodeFields` only; the other two fields are contravariant in
`o` and untouched. `Applicative` concatenates all three. There is no `Monad`
instance and there cannot be one, which is load-bearing below.

``` hs
-- Pawl.Codec.PhasePattern
codec :: Codec.Codec PhasePattern.PhasePattern
codec = Fields.object $ do
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
It needs adding to `.hlint.yaml`'s allowlist with its reason, alongside
`ScopedTypeVariables`, which `object` and `tagged` need to reach their own type
variable for `typeName`. Because `Fields o`
has no `Monad` instance, a bind that depends on an earlier one fails to compile
rather than silently desugaring through `join`: the independence the encoder
relies on is checked, not assumed.

## Sums: schema derived, `encode` written out

``` hs
-- Pawl.JsonCodec.Arm
data Arm a where
  MkNullary :: String -> a -> Arm a
  MkPayload :: String -> Codec.Codec b -> (b -> a) -> Arm a

tagged :: (Typeable.Typeable a) => (a -> Value.Value) -> [Arm a] -> Codec.Codec a
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
`Pawl.JsonSchema.NameSpec` pins the rendering rule the same way: a bare type
gives `PhasePattern`, an applied one `Face_Card` rather than a name with a space
in it. Escaping itself is `pawl:json-pointer`'s, already covered by `TokenSpec`,
and is not re-tested here; `encodeFragment` gets its own case in `PointerSpec`
beside `encode`. What `DefineSpec` adds is that the two are composed correctly --
a `MkName ":~/"` giving the `$defs` key `:~/` and the `$ref` `#/$defs/:~0~1`.
That uses a hand-built `Name` rather than a real operator-named type: that a
`TypeRep` renders its operator verbatim is GHC's contract, and asserting it would
cost `TypeOperators` to say nothing about the code under test.

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

One risk survives the slice, and the PR says so rather than implying coverage:
**parametric codecs are untouched.** `Face`, `Mode` and `Cost` take a `card`
encoder today and would become `Codec card -> Codec (Face card)` under a
`(Typeable card)` constraint. Naming them is handled -- `typeName` keys each
instantiation separately -- but nothing in the slice exercises one, so that the
rest of the shape survives the extra parameter is argued, not shown.

Recursion is not on that list. `define` registers its name before evaluating its
body, so a re-entrant call returns a `$ref` against an in-progress entry and the
cycle terminates structurally, whatever the type. `DefineSpec`'s synthetic case
covers it.

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
- **Hand-written definition names.** A string per `define` is a second place the
  type's name lives, free to drift when the type is renamed, and it leaves
  parameterised types with no answer. `Typeable` costs one extension and removes
  the argument entirely.
- **Fully qualified names.** Collision-proof without leaning on one-type-per-
  module, but `#/$defs/Pawl.Types.PhasePattern.PhasePattern` in every reference,
  with the module name repeating the type name, for a collision the convention
  already prevents.
- **Escaping `$ref` inside `json-schema`.** `pawl:json-pointer` was written
  before the schema for this, and gets the `~`-before-`/` ordering right. A
  second implementation would be a second thing to get wrong.
