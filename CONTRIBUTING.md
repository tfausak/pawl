# Contributing

To set up the local development environment, use [Nix][1] (`nix develop`) or
[direnv][2] (`direnv allow`). Then run:

``` sh
cabal configure \
  --enable-benchmarks \
  --enable-tests \
  --flags=pedantic \
  --jobs \
  --semaphore
```

Use all the normal Cabal commands like `cabal build`.

For a fast edit-test loop, prefer the REPL over relinking. `cabal repl
pawl:test` takes about twenty seconds to load, after which a subset runs in
well under a second and `:r` reloads only what changed:

``` sh
ghci> System.Environment.withArgs ["-p", "Engine.Trigger"] Pawl.Test.main
```

`ghcid --command 'cabal repl pawl:test'` does the same on every save.

When you do want a binary, `cabal build pawl` links one executable instead of
three, and it reaches the other two: `cabal run pawl -- test -p Engine.Trigger`
and `cabal run pawl -- bench`.

Prefer clarity over cleverness: code that is easy to read, debug, and modify.
See the [style guide][3] for specifics.

All changes must start with an issue that clearly describes the problem or
enhancement. Then branch off the latest default branch, which is `main`.
Prefer a branch name like `issue-slug`; for example if issue 123 is about the
defender keyword, the branch might be called `123-implement-defender`.

Ideally changes follow TDD by writing a failing test first, seeing it fail, and
then writing code to make it pass.

Open PRs as drafts, and mark them ready for review once they're finished. A
draft means the work is still moving; leaving a finished one as a draft just
stalls it. Please do not force push to a branch after opening a PR. Merges are
squashed, so a branch's internal history is working state rather than a record.
Commit as often as is convenient.

Typically each PR will close one issue. However sometimes a single PR will
close multiple issues. Other times one issue may require multiple PRs, in
which case each PR should be independently mergeable and only the last one
should close the issue.

[1]: https://nixos.org
[2]: https://direnv.net
[3]: ./docs/style-guide.md
