<!-- One PR per logical chunk of work. See docs/workflow.md. -->

## What and why

<!--
Closes #N — or "Part of #N" when a large issue is spanning several PRs.
Each PR must be independently mergeable and leave main green.
-->

## Rules basis

<!--
The CR citations behind this change, each checked against docs/rules.txt.
Never recalled from memory — recalled rules go stale, and have been wrong before.
Delete this section if the change touches no rules.
-->

## Approach

<!-- The design calls made, and the alternatives rejected. -->

## Verification

- [ ] `cabal build all --enable-tests --enable-benchmarks` is warning-clean
- [ ] `hooky fix` applied, `hooky run` passes
- [ ] Suite: 1162 -> 1162
- [ ] Proving test:

## Invariant check

<!--
Does this diff make the rules core case on an effect's *identity* rather than on a
classification? Fusing the closed and open halves is the project's single named
failure mode. An explicit "no" is cheap; say it.
-->

## Deferred

<!--
What is not implemented, with the issue filed and (#N) cited at the code site.
"Nothing" is a fine answer.
-->
