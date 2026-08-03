# An omitted field means its default

## The problem

The codec writes every field of every record, whether or not the field says
anything. `data/cards/goblin-piker.json` is 51 lines, of which nine keys hold `[]` and one
holds the `ModeSelection` that means "not modal". Dropping those, and the `spell`
stanza left empty behind them, takes the file to 35 lines:

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "keywords": [],
  "replacementEffects": [],
  "spell": {
    "modes": [{ "effects": [], "targetSpecs": [] }],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "triggeredAbilities": [],
  "typeLine": { "subtypes": [...], "supertypes": [], "types": [...] }
}
```

Across the 226 committed cards: **2,364 keys hold `null`, `[]`, or `{}`**, out of
13,451 JSON paths, and **`"selection": {"type":"ChooseExactly","value":1}` appears
325 times**. Simulating the rule below over the corpus takes it from **20,804
lines to 16,228** — and that simulation is conservative, since it does not know
the enum defaults in R2 below.

The same noise fills the specs. 512 `Common.assertJsonCodec` sites carry a JSON
literal, and most of them spell out fields that say nothing.

`Pawl.Codec.Card` already shows where this ends up. Its `toJson` is in two tiers:
**12 keys written unconditionally**, and **16 hand-rolled `if null then [] else
[...]` blocks** for the ones added later. The comments admit the split is
historical rather than principled:

> Those two are required keys spelled `null` on every noncreature because they
> predate the pool; a required loyalty key would have meant editing every other
> card file to say nothing.

The work is to make the second tier the only tier.

## The rules

Each is stated so a reviewer can check a module against it.

**R1 — a field is omissible iff some value of it means "nothing to say here."**
This is deliberately narrower than "the type has a zero". `Maybe a` defaults to
`Nothing` and every container (`Set`, `Seq`, `[]`, `Map`) to empty, because for
those, absence *is* the meaning. A `Bool` field naming a property the thing
lacks defaults to `False` for the same reason: `DamageEvent.dealtByDeathtouch`,
`dealtByInfect`, `dealtByToxic`, `dealtByLifelink`, and `EntryRiders.attacking`.

**R2 — an enum is omissible only when one constructor means "no restriction."**
Five qualify, and two of them already have hand-rolled decode halves
(`Counterability.fromJsonDefault`, `Optionality.fromJsonDefault`):

| Type | Default | Read as |
|---|---|---|
| `Counterability` | `Counterable` | no restriction |
| `Optionality` | `Mandatory` | no rider |
| `SourceRelation` | `AnySource` | unfiltered |
| `TapState` | `Untapped` | no rider |
| `ControllerRelation` | `Anyones` | unfiltered |

`ModeSelection` joins them at `Modal.selection`, defaulting to `ChooseExactly 1`.
That is not a new judgment: `Pawl.Types.Modal`'s header already reads "A non-modal
payload is one Mode with ChooseExactly 1."

**R3 — identity fields are never omissible.** `ObjectId`, `PlayerId`, `CardName`,
`SlotName`, `AbilityName`. An omitted `Countering.source` silently meaning object
`0` is a bug wearing a default's clothes. Concretely this keeps `Countering`'s
three fields, `ZoneChange`'s `departed` and `object`, `ManaCount.player`, and the
identity fields of `DelayedTrigger` and `DamageEvent` required. Together with R4,
which covers `ZoneChange.from`/`to` and `ManaCount.filter`, it leaves `Countering`,
`ZoneChange` and `ManaCount` written exactly as they are today.

**R4 — a field whose constructors are all equally meaningful is never
omissible**, because privileging one is a lie about the data. This keeps
`Condition.measured`/`comparison`/`threshold`, `ZoneChange.from`/`to`,
`PhasePattern.whichPhase`, `Count.scope`/`filter`/`aggregation`,
`CounterPattern.onWhat`, `PlayerStaticAbility.scope`/`effect`, and
`StaticAbility.affected`/`modifications` as required keys.

**R5 — a numeric payload where `0` is a real value is never omissible.**
`EntryOption.power` and `EntryOption.toughness` describe a token, and a 0/0 token
is legal; an absent power must not read as "0/0" when it means "the file forgot".
This is the rule R1 would otherwise trample, since `Natural` has a zero.

**R6 — `Card` requires `name` and `typeLine`, and `TypeLine` requires a non-empty
`types`.** `name` has no default. `typeLine` guards against a truncated file, but
only if the guard reaches the content: under R1 alone, `"typeLine": {}` would be
legal and a typeless card would load. So `TypeLine.types` decodes through a
non-empty check (the shape `Common.decodeNonEmpty` and `Modal.fromJson` already
use), while `supertypes` and `subtypes` default to empty. Every other `Card` field
becomes omissible, including `spell`, whose default is one empty `Mode` with
`ChooseExactly 1`.

**R7 — the verbose form stays legal.** Omission becomes *permitted*, never
*required*, on input. Every file and literal that loads today still loads
afterward. This is what makes the corpus migration a rewrite rather than a flag
day, and it falls out of keeping `Common.decodeMaybe`: an explicit `null` decodes
to `Nothing` exactly as an absent key does.

## The mechanism

Three functions in `Pawl.Codec.Common`:

```haskell
-- | A field that is always written and must be present.
requiredPair :: String -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]

-- | A field written only when it differs from the default an absent key means.
optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]

-- | Reads a field that may be absent, supplying the default 'optionalPair' omits.
defaultedField ::
  String ->
  a ->
  (Value.Value -> Either Text.Text a) ->
  [Pair.Pair Value.Value] ->
  Either Text.Text a
```

`toJson` becomes `Common.object . concat $ [...]`, one line per field, so whether
a field is required reads down the left edge instead of being split across two
tiers a hundred lines apart. `Pawl.Codec.Modal` in full:

```haskell
defaultSelection :: ModeSelection.ModeSelection
defaultSelection = ModeSelection.ChooseExactly 1

toJson :: (card -> Value.Value) -> Modal.Modal card -> Value.Value
toJson codec m =
  Common.object . concat $
    [ Common.requiredPair "modes" (Common.encodeSeq (Mode.toJson codec)) (Modal.modes m),
      Common.optionalPair "selection" defaultSelection ModeSelection.toJson (Modal.selection m)
    ]
```

Each default gets a top-level binding in its codec module, named `default<Field>`,
referenced by both halves. That is what keeps the two halves from drifting on the
*value*; the key name is still written twice, as it is today, and a typo there
fails loudly under the round-trip cases in §Verification.

**Seven deletions**, all subsumed. From `Common`: `nullableField`,
`decodeListDefault`, `decodeSetDefault`, `decodeMapDefault`,
`decodeBooleanDefault`. Plus `Counterability.fromJsonDefault` and
`Optionality.fromJsonDefault`, whose per-type existence was only ever a
workaround for `defaultedField` not taking the default as an argument.

`decodeMaybe` stays, per R7, and composes:
`Common.defaultedField "power" Nothing (Common.decodeMaybe Power.fromJson)` accepts
an absent key, an explicit `null`, and a value alike.

## Scope

29 codec modules build a record with `Common.object`: `ActivatedAbility`,
`AttackCost`, `AttackRequirement`, `Binding`, `BlockRequirement`, `Card`,
`CombatRestriction`, `Condition`, `Cost`, `Count`, `CounterPattern`, `Countering`,
`DamageEvent`, `DamagePattern`, `DelayedTrigger`, `EntryOption`, `EntryRiders`,
`ManaCount`, `Modal`, `Mode`, `PhasePattern`, `PlayerStaticAbility`,
`ProjectedCharacteristics`, `StaticAbility`, `TokenPattern`, `TriggeredAbility`,
`TypeLine`, `ZoneChange`, `ZoneChangePattern`.

All 29 are in scope. No boundary needs drawing around the engine-internal event
codecs by hand: R3, R4 and R5 already leave `Countering`, `ZoneChange` and
`ManaCount` byte-identical, and reduce `DamageEvent` to losing its four `dealtBy*`
flags while every identity field stays required.

Tagged `"value"` payloads are out of scope: `Common.tagged` already omits `value`
when there is none, and a constructor's payload is not a defaulted record field.

Nothing under `Pawl.Engine` or `Pawl.Types` changes. The diff does not make the
rules core case on an effect's identity; it does not touch the rules core at all.

## Migration

**The corpus.** Regenerate all 226 files from the encoder, then
`script/format-json.sh fix` for the canonical `jq --sort-keys` form. The proof
that nothing was lost is not P3 — which compares a file against the encoder and
would pass by construction — but a before/after check: load every card from the
old corpus, load every card from the new one, and assert the `Card` values are
equal. That check is temporary scaffolding, run once during the migration and
reported in the PR body rather than committed.

**The specs.** Regenerate the 512 literals from `toJson`. What this costs is worth
stating plainly, because it is a real reduction in test strength:
`Common.assertJsonCodec` runs both directions against the same literal, so
`assertFromJson` survives as `fromJson . toJson == Right` — a round-trip property
that still fails if the encoder omits a key it should not have. What is lost is
the literal's role as an independent statement of the *wire format*: after
regeneration, a renamed JSON key would round-trip happily. The regeneration diff
is therefore the one place that can catch it, and wants reading rather than
skimming.

## Verification

- **One all-defaults case per record codec.** The record with every field at its
  default, asserted against its minimal JSON. This is what closes the loop
  between the two halves: if `toJson` omits a field `fromJson` requires, the
  minimal form fails to decode; if the halves disagree on *what* the default is,
  the round-trip lands on a different value. For `Modal` that case reads
  `Common.assertJsonCodec s toJson fromJson defaultModal """ {"modes":[{}]} """`.
- **Verbose-form cases.** A handful asserting that the pre-migration shape still
  decodes, since R7 makes it a supported input with no other coverage once the
  corpus and the literals have both been regenerated.
- **P1, P2 and P3 unchanged.** They keep meaning what they meant.
- `cabal build all` warning-free, `hooky run` clean, suite count reported before
  and after.

## Risks

**The 512-literal regeneration is the one irreversible-feeling step.** It is not
actually irreversible — git — but a wrong encoder change baked into the literals
would look correct forever after. Mitigated by converting one codec module at a
time, so each commit carries one module's codec change, only that module's
literals, and the corpus delta that module caused. That is a smaller and more
attributable diff to read than one global regeneration at the end, and it keeps
the suite green at every commit rather than red across the middle of the work.

**Per-field defaults are judgment calls, and R2 is where they concentrate.**
Naming `ControllerRelation.Anyones` the default is a claim that "anyone's" is the
unmarked reading; if that is wrong for some field, the file gets quieter and less
clear rather than incorrect. Every one is reversible by moving one `optionalPair`
to `requiredPair`.

**`Card.spell` becoming omissible is the largest single behavior change**, since
it removes a stanza from every vanilla card. R6 keeps `TypeLine.types` non-empty
precisely so a file that loses more than intended fails rather than loading as a
blank card.

## Not in this work

No issue exists for this yet; filing one is the first task of the plan, and the
PR closes it. The 24 `Common.assertJsonCodec` sites that build their JSON by
`<>` concatenation are left alone — they are not literals, and shrinking them is
a separate cleanup with its own hazard (`init baseCardJson <> ...` depends on the
last character being `}`).
