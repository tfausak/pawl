# Contributing

To set up the local development environment, use [Nix][1] either with
[direnv][2] (`direnv allow`) or without it (`nix develop`). Then run:

``` sh
cabal configure \
  --enable-benchmarks \
  --enable-tests \
  --flags=pedantic \
  --jobs
```

Use all the normal Cabal commands like `cabal build`.

Prefer clarity over cleverness: code that is easy to read, debug, and modify.
See the [style guide](./docs/style-guide.md) for specifics.

All changes must start with an issue that clearly describes the problem or
enhancement. Then branch off the latest default branch, which is `main`.
Prefer a branch name like `issue-slug`; for example if issue 123 is about the
defender keyword, the branch might be called `123-implement-defender`.

Ideally changes follow TDD by writing a failing test first, seeing it fail, and
then writing code to make it pass.

Open PRs as drafts, and mark them ready for review once they're finished. A draft means
the work is still moving; leaving a finished one as a draft just stalls it.
Please do not force push to a branch after opening a PR. Merges are squashed,
so a branch's internal history is working state rather than a record. Commit as
often as is convenient.

Typically each PR will close one issue. However sometimes a single PR will
close multiple issues. Other times one issue may require multiple PRs, in
which case each PR should be independently mergeable and only the last one
should close the issue.

[1]: https://nixos.org
[2]: https://direnv.net
