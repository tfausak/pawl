# Effect payload records

One record per constructor, so that every arm's payload is a named object on the
wire and a named type in Haskell.

## Target shape

`docs/style-guide.md`'s "Avoid mixing ADTs and records" already prints the
destination as its *good* example:

``` hs
data T = C1 T1 | C2 T2
```

Effect (and GameEvent, PlayerEffect) currently sit at `C1 A B C` -- a plain
positional multi-arg constructor. That is NOT what the guide forbids; the guide's
bad example is record *syntax* inside an ADT, and its motivation is partial field
selectors. So this is a consistent extension of the guide's shape, not something
the guide already mandates. The motivation here is the wire format: a payload
with a real type is a payload `Fields.object` can name.

## What it settles

- **#1302 (`tupleN`) closes as wontfix.** Every arm carries exactly one payload,
  so `Arm.payload` covers all of them and no fixed-length array codec is needed
  at any arity.
- **#1305 dissolves.** Its open question was an unnamed `Fields` variant vs. real
  record types. Real records, so `Fields.object` derives every `$defs` name from
  `Typeable` as it already does. No new `Fields` combinator.
- **The payload-level untagged unions go with it.** `moveTail`, `Create`'s
  riders, `Destroy`'s bound slot, `Mill`'s tally, `CreateCopy`'s count,
  `PreventNextDamage`'s rider, `ArmDelayedTrigger`'s onset/duration,
  `ShuffleIntoLibrary`'s naming PlayerRef and `OfferCast`'s offer are all
  elided trailing elements recovered by JSON type or by length. As named keys
  they are `Fields.defaulted`, and the discrimination problem does not arise.

## Naming

The constructor's own name, per the guide's `C1 T1` convention --
`Effect.MoveToZone` carries `Pawl.Types.MoveToZone`. Nothing is invented, the
verb-phrase arms (`PlayerSacrifices`, `DealDamage`) need no coined noun, and the
mapping is mechanical rather than a matter of taste.

NOT a `*Spec` suffix. `Pawl.Types.TargetSpec` is the outlier in the tree, not the
pattern; the established payload records are plain nouns (`EntryRiders`,
`CastOffer`, `MillTally`, `ManaProduction`, `ExchangeSides`).

Constructors take `Mk` per CLAUDE.md: `data MoveToZone = MkMoveToZone {...}`.

## Sharing a record

Two constructors MAY share a record where their shape coincides -- `Draw`,
`Scry`, `Surveil` and `Fateseal` are all "this player, this many".

**That sharing is expediency, never a claim of equality.** The invariant, and the
reason it is written here rather than left to judgment:

> When one sharer needs a field the others do not, SPIN OUT a separate type for
> it. Never bolt an optional field onto the shared record for one constructor's
> sake.

A shared record that grows a `Maybe` for exactly one user has silently become the
untagged union this whole effort is removing -- the field's absence would once
again be how a reader tells the constructors apart.

## Order

Arm by arm, not in one bang. The constructor-match sites are spread over 18
files but partition cleanly: `MoveToZone` alone is ~37 sites over 9 files, which
is one ordinary PR. Heaviest matchers are `Pawl.CardSpec`, `Pawl.Engine.Resolve`
and `Pawl.Engine.PlayerEffect`.

Two populations owe records, and the second is easy to miss:

- Arms still writing a POSITIONAL ARRAY in a function-shaped module ---
  `Pawl.Codec.Effect` holds most of them, with a few in `GameEvent` and
  `ProjectedCharacteristics`.
- Arms already on `Common.tuple` inside converted bundles. A tuple is still a
  positional payload with the arity pinned; converting one to a record is the
  same change, just already `Codec`-shaped. `grep -rn "Common.tuple"
  source/libraries/codec` enumerates them.

PILOT: `Condition.Compares`. Small (24 match sites, mostly specs), it is the
first instance of the naming convention, and it finishes a thread --- `Condition`
is the last module whose ONLY blocker is this unit, so the pilot converts it to a
bundle as its proof.

Prerequisites, each its own unit, all now landed:

1. ~~#1303 --- map-as-entry-array~~ (#1372, and the maps became objects rather
   than gaining a combinator).
2. ~~`PlayerRef` -> `ObjectRef`~~ (#1369).
3. ~~`Aggregation` + `Count` + `Quantity`~~ (#1394), which also took `EventShape`,
   `ManaCount` and `Scope`.

## Migration

The #1365 mechanism, which worked: retain the legacy decoder as a fallback behind
the new one, rewrite `data/cards/*.json` through a throwaway executable that
decodes and re-encodes, reformat with `script/format-json.sh fix`, then delete
the fallback and the executable. `Pawl.CardsSpec` asserts every file re-encodes
to its own meaning, so a missed file fails loudly, and it having passed on the
pre-migration corpus is what proves the decoder drops nothing.

Per arm, so the corpus is rewritten many times rather than once. That is fine --
the rewrite is mechanical and verified each time.

## Carried over

`Pawl.Types.Effect`'s per-constructor documentation is extensive and is the real
content of the change. It moves to the new record module, one type per module per
CLAUDE.md, and the constructor keeps only what distinguishes it from its
siblings. Re-check every CR citation as it moves.

`ObjectRef.EachCardInGraveyard` got a two-element array payload in #1365 and
wants a record here like any other multi-payload arm; that is a deliberate re-do,
not a miss.
