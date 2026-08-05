# The registry loads its root once, and keys every name a card has

Issue: #649. Siblings: #650 (an object's plural names), #648 (the face/layout
split this builds on).

## 1. The problem

`Pawl.Registry` resolves a name by slugifying it and reading `<slug>.json`. That
is one name per card, and CR 709.4a gives a split card two: "Each split card has
two names. If an effect instructs a player to choose a card name and the player
wants to choose a split card's name, the player must choose one of those names
and not both." CR 715.5 and CR 720.5 say the same of an adventurer card's and an
omen card's alternative name. There is no combined name in the rules — "Wax //
Wane" is a printing and deck-list convention.

#648's stopgap files a multi-face card under its face names joined, and a lookup
that misses falls back to `byFaceName`: list the root, keep the filenames whose
slug contains the requested one as a whole hyphen-separated run, read those in
order, confirm by face name. So `named registry "Wane"` resolves, but every miss
— including a plain typo — pays for a directory listing, and the filename
convention stands in for the rules.

## 2. Scope

**In:** the lookup path, its failure semantics, and the collapse of the
enumerate-and-parse duplication between `Pawl.Registry` and `Pawl.Corpus`.

**Out:** printings (the tier above cards — `Pawl.Types.Printing` is already a
hollow `newtype` over `Card` and the engine already speaks it, so the seam is
waiting); matching a *chosen* name against an object, which is a projection
question (#650); meld (#369).

**No data migration.** All 232 card files stay exactly where they are.

## 3. The mechanism

`fileRegistry` parses its whole root at construction and builds a `Map` keyed by
slug, inserting each card **once per face name**. A lookup is then a map hit:

```haskell
fetchCard name = pure (Map.lookup (Slug.fromText (CardName.unwrap name)) cards)
```

`named registry "Wax"`, `"Wane"` and a typo are all one lookup with no directory
listing. Keys stay `Slug`, so `named` keeps accepting a name or a slug for free —
slugify is idempotent, so both remain the same lookup.

The filename becomes **only a stem**: nothing in the lookup path reads it, and a
file could be named anything. That the corpus files itself by the joined slug
survives as a lint in the test suite, which is where a claim about what pawl
ships belongs — a new `Pawl.CardSpec` sweep derives the path from the card and
reads it, so the convention is enforced there.

### 3.1 The cost

Startup becomes O(pool) rather than O(cards actually used): a caller who wants
one card pays to parse all of them. At 232 files and 372 KB that is a cost the
test suite already pays many times over per run — `Pawl.Support.allPrintings`
alone is unmemoized and reads the pool whole, called repeatedly from `CardSpec`,
`CardsSpec` and `CodecIntegrationSpec`. The pool is hand-authored and
card-driven, so it grows by single cards rather than by sets.

## 4. Why not a registry of faces

The owner's first direction on #649 was a registry of faces — `data/faces/wax.json`
and `data/faces/wane.json`, with `data/cards/wax-wane.json` reduced to
`{"faces": ["Wax", "Wane"], "layout": "Split"}`. Three things argue against it:

1. **A face file cannot answer the question.** `named "Wane"` must return the
   whole card — both faces and the layout, since CR 709.4 combines them in every
   zone except the stack — so a face registry needs either a back-pointer from
   each face to its card (a hand-authored double-linked association, kept honest
   by a lint) or an index built over card pointer files. Both are machinery whose
   only job is routing the lookup.
2. **The printings tier does not need it.** A printing names a *card*, so
   printings sit above cards. Faces being separately addressable buys that layer
   nothing.
3. **Normalization would not fix a shared face anyway.** If two cards shared a
   face named X, the ambiguity lives in the *name → card* map, not in storage: a
   `faces/x.json` back-pointer would be one-to-many in exactly the same way. Face
   normalization would save authoring duplication and nothing else.

Splitting the pool would also roughly double the file count, since every
single-face card would need a pointer file beside its face file.

## 5. What deletes

- `byFaceName`, `containsRun`, `fileSlug` — the fallback scan and its narrowing.
- `memoized` and the `MVar` cache. An immutable map makes "each file is parsed at
  most once" true by construction rather than by holding a lock across a read
  (#265), and the `Registry` header's carve-out about the file registry listing
  its own root goes with it.
- `parseCard`'s filename identity check and its `name`/`slug` parameters; it
  becomes `ByteString -> Either Text Card`, since `loadRoot` pairs each result
  with the path it came from.
- **`Pawl.Types.CardError`.** `fetchCard` becomes `CardName -> m (Maybe Card)`.
  `Invalid` becomes unreachable (§6), which leaves one constructor carrying one
  bit plus the name the caller already passed in — and both consumers,
  `fetchOrThrow` in the benchmark and `Support.cardOf`, only ever `show` it and
  abort. #167's two-constructor rationale, that a caller wanting "unknown card X,
  did you mean…?" should not have to string-match a `show`, has no such caller
  today. Messages improve: `no such card: Fog` rather than
  `Missing (MkCardName "Fog")`.
- **`Pawl.Corpus` and `Pawl.CorpusSpec`.** Reading and parsing every file in a
  root *is* the registry's construction now, so the lint's enumerate-and-parse
  moves into `Pawl.Registry` and `Pawl.Support` calls it directly. Deleting a
  module needs `cabal-gild pawl.cabal` run directly; `hooky fix` acts on staged
  files and skips it.

The filing convention itself survives as `Registry.filedAs :: Card -> Slug` —
the face names joined, slugified — because the corpus lint still needs to say
where a file belongs. What ends is its role in a *lookup*. So `CardName.join`
keeps two consumers rather than one, and the point of its comment changes rather
than disappearing: `Engine.Card.combined` and `Registry.filedAs` ask for the same
string for unrelated reasons and no longer have to agree.

`named`, `defaultRoot`, `cardPath` and the `Registry` record are unchanged.
Enumeration still is not part of the *interface*: a caller holding a `Registry`
can ask for a named card and nothing else.

## 6. Failure at construction

A new `Pawl.Exceptions.InvalidCorpus { root :: FilePath, problems :: [String] }`,
base-only like `MissingRoot` — the `exceptions` sublibrary sits above `types` and
cannot speak `CardName`. It is thrown once, listing **every** unparseable file
with its reason and **every** ambiguous name with the paths claiming it, sorted
so the report is a fact about the pool rather than about enumeration order.

Throwing rather than reporting per lookup is forced rather than chosen: an
unparseable file has no face names, so there is no key it could be filed under
and no lookup that could report it. Construction is the only place it can
surface. This is also #167's rationale — one clear failure at startup rather than
N identical ones — and `Corpus.loadAll`'s "a sweep should name every bad file in
one run" carried over to the thing that now does the sweeping.

Two names colliding is likewise fatal. Under the old filename keying it was
impossible, since filenames are unique; keying by face name makes it
representable, and a silent last-one-wins would be a lookup quietly serving a
card it was not asked for. A collision is fatal wherever it comes from — two
cards claiming one name, or one card repeating its own face name.

This is a new check rather than a moved one. `Pawl.CardSpec`'s existing "a card's
face names are pairwise distinct" lint is **intra**-card, and is what makes
`Engine.Card.faceNamed` well-defined; the check here is **inter**-card, and no
lint holds it today. It also lowers what #166 (closed) left standing: slugs are
unique across the committed corpus but not across the full ~34,660-name pool —
`Glimpse the Unthinkable` and `Glimpse, the Unthinkable` reduce to one key, and
joke-set names collide with the real cards they parody. #166's guard was that "a
file name holds one card"; keying by face name gives that up, and replaces it
with a loud failure at construction rather than a wrong answer at lookup.

**A real regression, stated plainly:** a broken corpus can no longer be repaired
mid-process, because it can no longer be *constructed* over. `RegistrySpec`'s "a
failed load is not memoized: fixing the file fixes the lookup" case is deleted
rather than reworded.

## 7. The joined name

`named registry "Wax // Wane"` becomes a miss. It resolves today only because the
joined string happens to be the filename, and CR 709.4a gives a split card two
names and no combined one. Removing it is the point of the issue: nothing in the
lookup path should depend on a joined name.

Real deck exports do write the joined form. Normalizing it to one of the card's
names is a deck-list parser's job, and pawl has no deck-list format today (the
benchmark's decks are built in Haskell). Filed as a follow-up rather than
answered here.

## 8. Tests

Every case runs against a throwaway corpus in a temp directory, as
`Pawl.RegistrySpec` already does — the committed `data/cards` is read-only there,
and these failure modes have no representative in it by construction.

The proving test is a **polarity flip of an existing case**, which is what makes
it discriminate rather than pass vacuously:

| Case | Today | After |
|---|---|---|
| a file whose card is named something else | `bird-maiden.json` holding Goblin Piker is `Invalid` | **`named "Goblin Piker"` resolves and `named "Bird Maiden"` misses** — both directions, proving the filename is not load-bearing either way |
| `named "Wax // Wane"` resolves | asserted | flipped to a miss, CR 709.4a |
| a filename that merely contains the name asked for | pins `containsRun` | deleted with its subject |
| a malformed file, and one with invalid UTF-8 | two lookup-time `Invalid` cases | `InvalidCorpus` thrown at construction, naming both paths |
| — | — | **new:** two files declaring a face named "Wax" → `InvalidCorpus` naming both paths |
| a split card is found by either of its names | passes via the fallback scan | passes via a map hit |
| a root that does not exist | `MissingRoot` at construction | unchanged |

## 9. Follow-ups

- **Deck lists.** A deck-list parser is where a printed `"Wax // Wane"` gets
  normalized to one of the card's names. No issue exists yet; file one.
- **Meld (#369).** CR 712.4a melds a pair into one permanent represented by two
  cards, CR 712.4b makes those back faces meaningful only while melded, and CR
  201.4e lets a player choose the *combined* back face's name. One name mapping
  to two cards would be a construction failure under §6. Nothing here makes that
  worse — meld is unrepresentable today either way, and #648's spec already
  records it as a different problem in kind.
- **Printings.** Unchanged and still unfiled as its own issue.

#650 is unaffected. An object's projected *name set* is a projection question;
this changes only how a card is found.
