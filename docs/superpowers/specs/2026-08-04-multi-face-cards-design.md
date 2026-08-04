# Multi-face cards: the face/layout split, and CR 709 split cards

Issue: #648. Follow-ups on naming: #649 (registry lookup), #650 (plural names).

## 1. The problem

`Pawl.Types.Card` is one flat record of 28 printed characteristics — one name,
one mana cost, one type line, one text. Every card layout the comprehensive
rules enumerate that prints more than one set of characteristics is therefore
unrepresentable: CR 709 split, CR 710 flip, CR 712 double-faced, CR 715
adventurer, CR 720 omen, CR 722 preparation.

`docs/design.md` §2.11 already fixes the shape — `faces :: NonEmpty Face` plus a
`Layout` tag, never `left`/`right` — and
`docs/superpowers/specs/2026-07-15-m0-core-types-design.md:126` records that it
was deliberately deferred out of M0. This spec cashes that in and lands one
layout end-to-end.

Nothing in `source/` mentions a face, a layout, or any of those six layouts
today. `Subtype.Adventure` exists as a subtype name with no mechanism behind it.

## 2. Scope

**In:** the face/layout restructure, and CR 709 split cards, proven by
Wax // Wane.

**Out, each already or newly tracked:** CR 710 flip, CR 715 adventurer, CR 720
omen, CR 712 double-faced (which is what unblocks #70), CR 712.4 meld (#369),
B.F.M., CR 702.102 fuse, and CR 709.5 Room cards with their shared type lines
and unlock designations.

Meld and B.F.M. are a different problem in kind — one permanent represented by
two *cards* (CR 712.4a) rather than one card with several faces — and nothing
here is shaped for them.

## 3. Types

```haskell
-- Pawl.Types.Face
data Face card = MkFace { name, manaCost, typeLine, power, …, openingHandAction }

-- Pawl.Types.Layout
data Layout = Normal | Split

-- Pawl.Types.Card
data Card = MkCard { layout :: Layout, faces :: NonEmpty (Face Card) }
```

`Face` takes today's 28 `Card` fields verbatim, with their comments. `Card`
shrinks to two.

### 3.1 The recursive knot stays tied at `Card`

Six of the 28 fields are parameterized at `Card` itself: `spell :: Modal card`,
`activatedAbilities`, `triggeredAbilities`, `delayedAbilities`,
`mulliganAction`, `openingHandAction`. Those knots stay at `Card`, so `Modal
Card`, `Effect Card` and `TriggeredAbility Card` are unchanged across the
codebase, and a token-defining effect keeps naming a whole card. `Face` is
parametric in `card` and `Card` instantiates it at itself, which is the
established way this repo breaks such a cycle — no `hs-boot`, no narrowed
stand-in type.

Tying at `Face` instead would have made a double-faced token (CR 707.8a)
unrepresentable and churned every one of those type applications for nothing.

### 3.2 `Layout` is a rulebook enumeration

Casing on `Layout` in the closed half is the same act as casing on `Phase` or on
a `Keyword`: CR 709/710/712/715/720/722 are numbered sections of the rulebook.
The invariant in `design.md` §1 forbids casing on an *effect's identity*, which
this is not. `Layout` gets one arm per layout that has actually landed, so it is
`Normal | Split` here and grows.

### 3.3 `Normal` is the identity

Under `Normal` the card has exactly one face, `combined` returns it and
`castableFaces` offers it alone. Every card in the pool today is `Normal`, so
the restructure is behaviour-preserving by construction. That is the property
the whole change rests on, and it is what makes the 227-file migration safe.

## 4. Which face is live

Two closed-half functions in `Pawl.Engine.Card`, both casing on `Layout`:

- **`combined :: Card -> Face Card`** — CR 709.4, the characteristics a card has
  in every zone except the stack. Under `Split`: the union of the faces' card
  types and abilities (CR 709.4c), of their supertypes and subtypes (CR 709.4
  itself — a supertype and a subtype are characteristics under CR 109.3, and
  709.4c narrows only the card types), of their keywords, and the concatenation
  of their mana costs (CR 709.4b), from which colours and mana value fall out.
  The name is the face names joined by `//` — see §4.1. Under `Normal`: the sole
  face.
- **`castableFaces :: Card -> [Face Card]`** — CR 709.3a, the faces a player may
  propose to cast. A `Face` already carries its own `name`, so the pair the first
  draft returned was redundant; `Cast.castableSpells` reads `Face.name` off it to
  build each `Action.Cast`.

`Object` gains `face :: Maybe CardName` — the face this object is on the stack
with (CR 709.3b, "while on the stack, only the characteristics of the half being
cast exist"). `Nothing` in every other zone, where the layout decides. That field
is also where CR 712.8f's "the face that's up" will live when double-faced cards
land.

The two seams where a card becomes characteristics — `Projection.baseCharacteristics`
and `Projection.viewOfCard` — take the face from the object rather than the card,
and fall back to `combined` when the object names none.

### 4.1 The combined name is the face names joined by `//`

The comprehensive rules never state this, but they do it: every Example in
`docs/rules.txt` that names a split card writes the halves joined **without
spaces** — `Fire//Ice` at lines 3882 and 5747, `Assault//Battery` at 5746. That
is four occurrences across three Examples, and one of the three sits under a
flashback rule rather than under CR 709 at all; CR 709.4a itself carries no
example. Every external card source does the same, modulo spacing.

So `combined` joins. It beats taking the first face's name, which would privilege
one half for no rules reason, and it is what a log or a player view should show.

Three properties worth stating, because they are what keep the join cheap:

- **It is derived, never stored.** A card file carries per-face names only; there
  is nowhere to write a combined name, so the joined string and the filename slug
  cannot drift apart. It also means the separator is a one-line decision, not a
  data migration.
- **The slug is indifferent to it.** `Pawl.Slug.fromText` maps `/` to a space and
  splits on words, so `"Wax//Wane"` and `"Wax // Wane"` both slugify to
  `wax-wane`. Sourcing a card from Scryfall, which spaces its separator, cannot
  produce a different filename.
- **It does not reach the stack.** CR 709.3b gives a spell only the
  characteristics of the half being cast, so a Wax on the stack is named "Wax".
  The join lives inside `combined` and nowhere else.

What the join does *not* do is satisfy CR 709.4a, which says the card has two
names and that an object has a chosen name if *one of its names* is the chosen
name. "Wax//Wane" is not one of them. A single string cannot answer that
question, and the joined one is wrong in a more symmetric and more legible way
than the alternative rather than being right. #650 carries the plural-names gap,
and is deliberately not #649: this is a characteristic, that is a lookup.

## 5. Faces are referenced by name, not by index

`faces` is a `NonEmpty` and keeps printed order, because several layouts have
positional rules: CR 709.5's "left half"/"right half", CR 710.1a's "top half" and
CR 710.1b's bottom, CR 712.8a's front face. Combining a mana cost needs a
deterministic order too. But the *reference* to a face is its `CardName`.

Order belongs to the container; identity belongs to the name. CR 709.4a says the
card has those names, and in paper a player casts a half by naming it. A
name-keyed `Action.Cast` in a `DecisionLog` — the canonical replayable artifact,
`design.md` §2.10 — replays correctly as long as the name still names a face,
where an index silently replays as the wrong half if the card data is ever
reordered. It does not fail loudly on a name that names none: `Game.resolveFace`
falls back to CR 709.4's combined view, which the lint below is what keeps
unreachable.

Three consequences:

1. **A new `CardSpec` lint: a card's face names are pairwise distinct.** This is
   what makes the reference well-defined, and it joins the existing biconditional
   lint family.

2. **Prototype (CR 718) is not a layout.** CR 718.2's alternative set is mana
   cost, power and toughness only, with no name of its own — a prototyped Rust
   Goliath is still named Rust Goliath — so two faces would collide with the lint
   above.

   CR 702.160a is the positive account: "Prototype is a static ability that
   appears on prototype cards that have a secondary set of power, toughness, and
   mana cost characteristics. A player who casts a spell with prototype can
   choose to cast that card 'prototyped.'" That is an alternative cost with a P/T
   rider — the shape `Card.alternativeCosts` (CR 118.9) already has — rather than
   a second set of characteristics the way an Adventure is. It stays a
   single-face card, and is out of scope here either way.

3. **The reference resolves against the object's stored card, never a projected
   one.** `Projection.rewriteCard` does rename a card under CR 612.2a, but only
   the token-definition card a `Create` effect names, and it returns a derived
   value; it never touches the `Source.OfCard` an object embeds. So no in-game
   rename can dangle a stored face name. `rewriteCard` walks into faces.

## 6. The half-choice is not a prompt

CR 709.3 puts the choice *before* the card is put onto the stack, and CR 601.2b
calls such a thing a previously made choice that may restrict later ones. CR
709.3a: "Only the chosen half is evaluated to see if it can be cast."

So `Action.Cast` gains the face — `Cast ObjectId CardName` — and
`Cast.castableSpells` offers **casting Wax and casting Wane as two separate
legal actions**, each priced from its own face by `Cost.costsFor`. No new prompt
and no engine choice: the engine never picks a half, it offers both. This also
puts the choice ahead of CR 601.2a's move to the stack, which matters because
53e02c37 made that move happen first.

An index would have been defensible — the `Activate` precedent carries an ability
*value* rather than an index precisely because abilities can be granted, and
faces cannot — but §5 settles it.

`Action.Play` is unchanged and carries no face. A modal double-faced land (CR
712.12) needs one; that arrives with CR 712.

## 7. Data and the registry

Every card file becomes `{"faces": [ <today's object> ]}`. `layout` defaults to
`Normal` and the codec already omits defaults on encode, so a single-face card
gains exactly one level of nesting and nothing else. The migration of all 227
files is one mechanical `jq` pass, and `CardsSpec`'s re-encode-equals-the-file
check stays green.

A flat encoding retained for single-face cards was rejected: it would give one
type two encodings and force the codec to decide which to emit.

`wax-wane.json` holds both faces. `Registry.parseCard`'s identity check is
unchanged in form — the card's combined name must slugify back to the filename it
was filed under — because §4.1 makes that combined name the joined one. Nothing
in the check special-cases arity. A lookup that misses lists
directory *filenames* and parses only those whose slug contains the requested
slug as a whole hyphen-separated run, confirming the hit by face name rather than
by filename guessing — so `named registry "Wane"` resolves `wax-wane.json`.

This is a stopgap and #649 says so at length: the fallback re-lists on every
miss, the joined filename has no standing in the rules, and CR 709.4a's real
consumer is a card that instructs a player to choose a card name.

## 8. Deferred within CR 709

- **CR 709.4a's two names.** `ProjectedCharacteristics.name` stays a single
  `CardName` — the joined one of §4.1 — where the rule gives the card two.
  Nothing in the pool asks a player to choose a card name and no multi-face
  permanent exists, so the difference is unobservable today. Tracked by #650.
- **CR 709.5 Room cards** — shared type lines, unlock designations, unlock costs
  as special actions. Its own issue.
- **CR 702.102 fuse**, and CR 709.4d's fused-spell characteristics. Its own issue.
- **CR 709.3c**, an effect that copies a split card and lets the copy be cast.

Each gets an issue carrying its status, rationale and expiry trigger, and a
comment at the code site naming only what is not implemented, plus `(#N)`.

## 9. Proof

Wax // Wane, added to `data/cards/`, verified against Scryfall rather than
recalled: Wax is `{G}` Instant "Target creature gets +2/+2 until end of turn";
Wane is `{W}` Instant "Destroy target enchantment". Both effects are already in
the vocabulary — `ModifyTarget` with `ModifyPowerToughness`, and `Destroy` — so
the card is pure data and exercises no new opcode.

Gameplay-level tests, per `design.md` §4:

1. Casting Wax gives the targeted creature +2/+2 until end of turn, and the spell
   on the stack has only Wax's characteristics (CR 709.3b).
2. Casting Wane destroys the targeted enchantment.
3. Both halves are offered as legal actions from one card in hand, and each is
   priced from its own face (CR 709.3a).
4. In hand and in the graveyard the card is both green and white with mana value
   2 (CR 709.4b) and is named "Wax//Wane" — the combined view — while the Wax on
   the stack is named "Wax" (CR 709.3b). This card does NOT prove CR 709.4c's
   ability half: its two halves are vanilla instants whose only text is a spell
   payload, and `Face.spell` is deliberately not merged (only the chosen half
   ever resolves). The union of card types, subtypes, keywords and the three
   printed ability lists is proven instead against `Pawl.CardSpec`'s hand-built
   `splitCard`, whose halves differ on each of those axes.
5. `named registry "Wax"` and `named registry "Wane"` both resolve the one card.
6. The corpus lint holds that every card's face names are pairwise distinct.
