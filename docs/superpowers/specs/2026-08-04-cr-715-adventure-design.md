# CR 715 adventurer cards, and playing a card from exile

Issue: #667. Unblocks the cast-from-exile half of #48.

## 1. The problem

Two problems that are one unit, because the second is the reason to pick the
first.

**Adventurer cards are unrepresentable.** `Pawl.Types.Layout` carries `Normal`
and `Split`. `Pawl.Engine.Card.combined`'s header already names CR 715.4 as one
of the three rules that disagree about which face a card shows, and says the
layouts that make them differ "get their arm when they land". `Subtype.Adventure`
(CR 205.3k) has been in the enum since M0 with no mechanism behind it.

**Nothing can be played from exile.** `Pawl.Engine.Cast.castZones` is
`[Zone.Hand, Zone.Graveyard]`. Every casting permission pawl has is read off the
CARD — `Face.castingPermissions` (Panglacial Wurm) or what rule 702 gives a
keyword (flashback) — so a permission that a RULE grants to one exiled OBJECT
for one PLAYER has no carrier at all.

CR 715.3d is the shortest path to that carrier:

> Instead of putting a spell that was cast as an Adventure into its owner's
> graveyard as it resolves, its controller exiles it. For as long as that card
> remains exiled, that player may play it. It can't be cast as an Adventure this
> way, although other effects that allow a player to cast it may allow a player
> to cast it as an Adventure.

Every other printed producer of a play-from-exile permission is a "you may cast
that card for as long as it remains exiled" grant from a spell's own text
(Psychic Theft, Elkin Lair, Grinning Totem, Mnemonic Betrayal), each of which
needs a reveal-and-choose prompt or a mass exile before its text can be authored
at all. An adventurer card needs neither: both halves are already in pawl's
vocabulary.

## 2. Scope

**In:** `Layout.Adventure`; CR 715.4's combined view and CR 715.3's two castable
faces; CR 715.3d's exile-instead-of-graveyard and the permission it leaves on the
exiled card; `Zone.Exile` as a castable zone gated on that permission; and
Embereth Shieldbreaker // Battle Display as the proving card.

**Out:**

- CR 715.5, naming an adventurer card's alternative name — that rides on #650's
  singular-name axis.
- CR 715.2a's "has an Adventure" qualifier: no card in the pool asks it.
- CR 715.3c, a copy of an Adventure spell: no card in the pool copies one.
- The other missing layouts (CR 710 flip, CR 712 double-faced, CR 720 omen, CR
  722 preparation), unchanged from #648's own out-list.

## 3. The card

**Embereth Shieldbreaker // Battle Display** (ELD; reprinted CLB). Checked
against Scryfall 2026-08-04:

| Face | Cost | Type line | Text |
|---|---|---|---|
| Embereth Shieldbreaker | `{1}{R}` | Creature — Human Knight | 2/1 vanilla |
| Battle Display | `{R}` | Sorcery — Adventure | Destroy target artifact. |

A **sorcery**, not an instant — the reminder text's parenthetical is CR 715.3d
itself and is not printed data. The pool already holds artifacts for the
Adventure half to destroy (Bonesplitter, Basilisk Collar, Braidwood Sextant,
Darksteel Myr).

Face order in the card file is printed order, and for this layout the FIRST face
is the normal one. CR 715.2 puts the alternative characteristics in the inset
frame and the normal ones in the card's own frame; `combined` reads the first
face, exactly as CR 712.8a's front face will when double-faced cards land.

## 4. What each rule becomes

### 4.1 CR 715.4 — the combined view is the normal face

```haskell
combined card = case Card.layout card of
  Layout.Normal    -> NonEmpty.head (Card.faces card)
  Layout.Split     -> foldSplit (Card.faces card)
  Layout.Adventure -> NonEmpty.head (Card.faces card)
```

Same expression as `Normal`, different rule, and written as its own arm rather
than shared: CR 715.4 ("in every zone except the stack, and while on the stack
not as an Adventure, an adventurer card has only its normal characteristics") is
a claim about a card with TWO faces, where `Normal`'s is the trivial one about a
card with one.

### 4.2 CR 715.3 — either half may be played

```haskell
castableFaces card = case Card.layout card of
  Layout.Adventure -> NonEmpty.toList (Card.faces card)
  …
```

CR 715.3: "As a player plays an adventurer card, the player chooses whether they
play the card normally or as an Adventure." The same shape `Split` has, and for
the same reason — a list of options, never a choice the engine makes.

### 4.3 CR 205.3k — which face IS the Adventure

A face is an Adventure when its printed type line carries `Subtype.Adventure`,
read the way `Card.isAura` reads CR 205.3h. A subtype test and not a positional
one: the position decides which face is NORMAL (§3), and the subtype is what the
rules themselves key "cast as an Adventure" on.

### 4.4 CR 715.3d — exile instead of the graveyard

At `Resolve.resolveSpellWith`'s final `Event.changeZone oid Zone.Graveyard`, and
only there. The FIZZLE path a few lines above keeps the graveyard: a spell whose
targets are all illegal doesn't resolve at all (CR 608.2b), and CR 715.3d's
"instead of putting [it] into its owner's graveyard **as it resolves**" does not
reach it. The mechanic's own ruling (2019-10-04, checked on Scryfall
2026-08-04) names that exact case: "If an Adventure spell leaves the stack in
any way other than resolving (most likely by being countered or by failing to
resolve because its targets have all become illegal), that card won't be exiled
and the spell's controller won't be able to cast it as a permanent later."

The permission that survives the move is a new per-incarnation `Object` field:

```haskell
-- CR 715.3d
playableFromExileBy :: Maybe PlayerId.PlayerId
```

Written immediately after the exile move, cleared by `changeZone` like every
other per-incarnation field — which is exactly CR 715.3d's "for as long as that
card remains exiled", with no sweep to run and nothing to unwind.

State on the OBJECT rather than a `CastingPermission` arm: every existing
permission is a fact about a CARD, true of every copy of it in every game, and
`Cast.permissionsOf` reads them off the face. This one is a fact about one
incarnation of one card and names a player, so putting it in that enum would
make `permissionsOf` return something it cannot compute from a face.

### 4.5 CR 601.3 — exile as a castable zone

`castZones` gains `Zone.Exile`. `castableZones` currently maps a FACE to zones;
it grows the object and the game state, because this permission is not on the
face:

- `Zone.Hand` — no permission needed (CR 304.1 / 307.1).
- `Zone.Graveyard` — flashback, unchanged.
- `Zone.Exile` — `Object.playableFromExileBy` names this player, AND the
  proposed face is not an Adventure (CR 715.3d's "It can't be cast as an
  Adventure this way").

The second conjunct is what makes the exiled card offer exactly one action where
the same card in hand offers two.

### 4.6 Where the card's file lives

`Pawl.Registry.parseCard` files a card under its face names JOINED — a filing
convention rather than a name the card has (#649), and one the registry must
compute without the engine, since `registry` sits above `engine` in the
sublibrary table. So the file is `embereth-shieldbreaker-battle-display.json`
even though CR 715.4 makes the card's name "Embereth Shieldbreaker".

`Pawl.CardsSpec.slugOf` read that location off `Card.combined`'s name instead,
which agreed with the join for every card that had ever existed: one face makes
the join that face's name, and `merge2` joins a split card's two. Adventure is
the first layout where the combined view and the join differ, so `slugOf` moves
onto `CardName.join` — the same side of the distinction the registry is on.

## 5. Known limits, each an issue rather than a comment

- #668: the permission names the Adventure's CONTROLLER, but `Game.zoneMembers
  Zone.Exile` filters exile by OWNER, so a stolen Adventure spell exiles under a
  controller who then cannot find it. No card in the pool casts an opponent's
  adventurer card.
- #669: CR 715.3d's closing "although other effects ... may allow a player to
  cast it as an Adventure" has neither a second permission to be the other
  effect nor a place to put the exclusion other than the exile arm itself.
- #670: CR 715.3d's "play" covers a land, and pawl's land drop is a separate
  action that does not consult this field. No adventurer card's normal face is a
  land.

## 6. Tasks

1. `Layout.Adventure` + codec + `Pawl.Codec.LayoutSpec`.
2. `Engine.Card`: the `combined` and `castableFaces` arms, and `isAdventure`.
3. `Object.playableFromExileBy`, and CR 715.3d in `Resolve`.
4. `Cast`: `Zone.Exile` in `castZones`, and the gate in `castableZones`.
5. The card file, and `Pawl.AdventureSpec` end to end: cast the Adventure half
   from hand, the artifact dies and the card lands in exile; the creature half
   is castable from exile and the Adventure half is not; the creature resolves
   onto the battlefield with its normal characteristics.
