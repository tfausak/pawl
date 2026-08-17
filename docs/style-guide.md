# Style guide

Preferences for writing Haskell here --- style and practice more than layout,
which Ormolu owns. Recommendations, not laws: deviate when you can defend it
in review. Above all, prefer clarity over cleverness.

`hooky fix` runs Ormolu and HLint; `.hlint.yaml` enforces the rules marked
*(hlint)* below and the extension allowlist, and is configured not to suggest
against the rest. Code compiles under `-Weverything -Werror` (`pedantic`),
less the `-Wno-*` list in `pawl.cabal`'s `common library` stanza.

## Modules and imports

- Prefer qualified imports, aliased to the last component: `import qualified
  Data.Text as Text`. Import operators unqualified (`import Data.Aeson ((.=))`)
  rather than writing `Aeson..=`.
- Group all imports together; no blank-line groups.
- Avoid importing parents: `A.B.C` imports nothing from `A.B` or `A`.
- Avoid explicit export lists.
- A module that re-exports others does only that; new declarations go in a
  module the re-exporter pulls in.
- Prefer short identifiers and let the module qualify them: `User.name`, not
  `userName`. Design for qualified imports.

## Language

- Avoid language extensions beyond the allowlist in `.hlint.yaml`; adding one
  means adding it there with the reason. No `LambdaCase` or `TupleSections`.
- Prefer ASCII in source, comments included.
- Prefer line comments to block comments.

## Names

- Camel case; no underscores between words (`quietSnake`, `LoudSnake`).
- Avoid primes (`users'`). Find a better name; failing that a single `_` or a
  number suffix. Never stack underscores (`users__`) --- number instead.
- Conversions: `unwrap` for a newtype's field; otherwise `Foo.toText` /
  `Text.fromFoo`, not `fooToText`.

## Types

- Use `newtype` liberally; a `type` alias adds no safety.
- Use smart constructors: `newtype Email = UnsafeEmail Text` with `fromText ::
  Text -> Maybe Email`, rather than a record that is correct by construction.
- Expose record constructors and fields; hiding them is boilerplate.
- Avoid mixing ADTs and records: `data T = C1 T1 | C2 T2` with a record per
  arm, never `data T = C1 { f1 :: Int } | C2 { f2 :: Double }`.
- Derive at least `Eq` and `Show` (except for secrets, which get a redacting
  `Show`).
- Avoid boolean blindness: `data State = Active | Inactive`, not `Bool`.
- Avoid big tuples --- past three or four elements, a record.
- Avoid `String`; use `Text`.
- Avoid `Int`; prefer arbitrary precision (`Integer`, `Natural`) unless a wire
  or storage format forces a width. Convert with the named total functions in
  `Pawl.Extra.*`, never `fromIntegral` *(hlint)*.
- Avoid `List` for anything but a stack or a once-iterated stream: `Seq` when
  order matters, `Set` when it doesn't.

## Expressions

- Prefer `case` to `maybe`/`either`/`bool`-style combinators, and to multiple
  function declarations: `factorial n = case n of 0 -> 1; _ -> ...`, not two
  clauses.
- Avoid using partial functions (`head`, `fromJust`) and writing them
  (non-exhaustive matches, `undefined`, `error`); return `Maybe`/`Either`.
- Prefer `do` notation to `>>=` chains, and monadic `do` (bind each field,
  then `pure Record { title = title, ... }`) to `<$>`/`<*>` for building
  records --- the applicative form silently swaps same-typed fields. Never
  `do` in pure code: use `let ... in`.
- Prefer `let` to `where`. Bind in reading order, and one `let` block per
  group rather than a `let` per line.
- Avoid unnecessary eta reduction; write the lambda unless the point-free form
  shows the shape better (`foldr (+) 0` is fine, `((. f) . compare .)` is not).
- Prefer functions to operators, and never backticks: `elem x xs`, not ``x
  `elem` xs``.
- Avoid list comprehensions; `map`/`filter`, or `do` with `guard`.
- Avoid multi-layered nesting of `case`: pull one value out at a time.
- Avoid duplicate guards: `y | p y -> a | q y -> b`, not two `y | ...` arms.
- `g $ f x` over `g (f x)`; `h . g $ f x` over `h $ g $ f x` *(hlint)*.
