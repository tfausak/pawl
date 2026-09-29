# Scenario migration, piece 2: convert by recording, not by rewriting

Related to #146. Piece 1 (#4411) landed the format. This plan migrates the
bulk of the gameplay tier in one unit, and its cost does not scale with the
number of tests.

## Census (origin/main @ 097d9d0, grep-level, per `Spec.it` block)

`pawl:test` holds 7726 `Spec.it` blocks; the remaining ~3.8k tasty cases are
codec specs in other libraries, which never move.

| Bucket | Blocks | Moves? |
| --- | ---: | --- |
| Already on the harness (`S.play`, `S.runScriptOrFail`) | 63 | yes; ~20 are the harness's own specs, which stay |
| Engine loop, observable-only setup, stock answerer | 713 | yes |
| Engine loop, observable-only setup, custom answerer | 813 | yes, if every answer has a move |
| Engine loop, injected machinery (effects, replacements, events) | 270 | no: #146 decision 5 |
| Subsystem entry point called directly (`S.runPure` on one function) | 2581 | no: unit tests |
| Pure queries over a hand-built state | 3224 | no: unit tests |
| Whole games from decks | 40 | no, not in this unit |

So the realistic target is about 1,600 blocks, a fifth of `pawl:test`. The
buckets are grep heuristics; step 1 replaces them with exact numbers.

What those 1,600 need beyond piece 1's vocabulary:

- Board zones: library (528 blocks), graveyard (100), exile (14),
  attachments (43), player counters (19). Three- and four-player boards (242)
  already work.
- Checks, by how often a reader appears: power/toughness (~1,300 counting
  `Projection.powerOf`), whether an object is on the battlefield (582), hand
  size (518), counters on an object (393), the stack (948, mostly "is empty"),
  controller (286). Life, count, damage and tapped exist already.

## The idea

Don't translate Haskell into JSON; run the Haskell and record it. Every
candidate test already builds a state, hands it to an engine entry point under
an answerer, and asserts on the result. A recorder captures three things at
that entry point:

1. **The board.** Decompile the starting `GameState` to a `Board`. Fail the
   decompile, and so skip the test, if the state holds anything a board cannot
   express: continuous effects, replacements, delayed triggers, a non-empty
   stack, event history. That is decision 5, enforced mechanically.
2. **The timeline.** Wrap the test's answerer: log every non-pass answer with
   its (turn, step, decider) key and convert it to a `Move`. A cast's or
   activation's own prompts fold into its `Choices`. An answer with no `Move`
   (a "may" choice, trigger ordering, ...) marks the test blocked and counts
   the prompt kind.
3. **The end.** The turn and step the run stopped at, and the final state.

Assertions are the one part that needs the source. A script maps the common
readers to checks: `S.lifeOf` -> Life, `countOnBattlefieldByName` /
`creaturesInPlay` -> Count, `damageOf` -> Damage, `tappedCount` -> Tapped,
`powerToughnessOf` -> PowerToughness, and so on. It handles only assertions on
the run's final state. An assertion it cannot map, or one on an intermediate
state or on `GameState` internals (phase, priority, combat), flags the test,
and a flagged test stays in Haskell.

Because the answers are recorded rather than understood, the 813
custom-answerer tests cost the same as the stock ones.

## Steps

1. **Recorder, dry run.** A throwaway harness, not committed, that runs each
   candidate block alone (`-p` on its full name, `-j1`) with Support's runners
   instrumented, and writes one record per block. Its output is the exact
   census:
   - blocks that decompile;
   - a histogram of prompt kinds with no `Move`;
   - a histogram of assertions with no `Check`;
   - blocks that are fully convertible today.

   Report these numbers before building anything else. Budget: a few hundred
   lines, and one test-binary run per block (about 1,600 runs of a second or
   two each).
2. **Vocabulary, driven by those histograms.** Add the board zones and the
   `Check` nouns and `Move` verbs the histograms rank highest, stopping where
   the tail stops paying: each addition should unlock tens of tests, not ones.
   Each needs a type, a codec with a round-trip spec, a runner arm, and a
   README line, as piece 1's did. Every addition must be observable state
   (decision 5).
3. **Generator.** From each record plus its mapped assertions, emit
   `data/scenarios/<spec>/<slug>.json`:
   - labels for objects the test's assertions name, card-name references for
     the rest;
   - checks keyed at the first priority moment at or after the recorded end,
     falling back to `final`;
   - the Haskell test's name as the description.
4. **Gate every generated file automatically.** The unit only keeps a file
   that passes all of these:
   - it matches the schema and runs clean;
   - each check reads the value the Haskell assertion expected (true by
     construction once it passes);
   - with each check's expected value perturbed in turn, it FAILS on that
     check. This is the mutation step, automated per check, and it replaces
     hand mutation for generated files.

   A file that fails any gate is dropped, and its test stays in Haskell.
5. **Delete the converted Haskell blocks mechanically**, by the source ranges
   the census recorded. Then remove top-level helpers nothing references any
   more; there are no export lists, so `-Werror` will not find them and a
   grep script has to.
6. **Review by sampling, not reading everything.** An agent reads every
   flagged or dropped test's reason, plus a random sample of ~50 generated
   pairs (Haskell before, JSON after), checking that no assertion was
   weakened.

## One unit, two PRs

The recorder and generator are tools; their output is the unit.

- **PR A, vocabulary only** (step 2): small and reviewable, and useful to
  hand-written scenarios regardless.
- **PR B, the bulk:** the generated JSON and the Haskell deletions, with no
  hand-written code.

The PR B body carries:
- suite count before -> after, which should net out to about zero per
  converted block;
- the census table as measured in step 1;
- converted, dropped at each gate, and flagged, per reason;
- the sample review;
- the compile-time delta for `pawl:test`, cold build before and after.

Scenario files go in one subdirectory per source spec, so the directory stays
navigable and a later split of the corpus is free.

## Risks

- **Entry-point mismatch.** A Haskell test that ran only the priority loop
  stops mid-step, while a scenario plays whole steps. Checks keyed at the
  recorded end position plus the run gate catch this, and a mismatch drops the
  file rather than weakening it.
- **Over-specified checks.** Emit only checks mapped from the test's own
  assertions, never a whole-state snapshot. Whole-state asserts are the Wagic
  failure #146 records.
- **Parallel work.** Deleting ~1,000 blocks collides with every in-flight
  card unit. Land PR B in a quiet window and merge `origin/main`
  immediately before, re-running the generator if needed.
- **The yield is unknown until step 1.** If fewer than a few hundred blocks
  convert, stop after step 1 and report. The histograms then say whether the
  format or the tests are the obstacle.

## Out of scope

- The 5,800 unit tests of engine functions: they stay in Haskell permanently.
- Whole-game tests.
- Tests on injected machinery.
- The XMage import.
- Making JSON the default for new gameplay tests: a one-line CLAUDE.md
  change, worth doing right after PR B.
