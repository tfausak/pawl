#!/usr/bin/env bash
# Keep one worktree at origin/main with a finished build, as the donor for
# every fresh worktree's dist-newstyle -- and its dist-mutate, the -O0 build
# script/mutate.sh uses. Run after each merge; the builds are incremental, so
# they cost the merge's diff and nothing else. Usage:
#   script/warm-worktree.sh            # refresh the donor
#   script/warm-worktree.sh seed DIR   # clone its builds into DIR
# The donor lives at .claude/worktrees/warm under the primary checkout and is
# only ever reset to origin/main; nothing is edited or committed there.
set -euo pipefail
primary=$(git rev-parse --path-format=absolute --git-common-dir)
primary=${primary%/.git}
warm="$primary/.claude/worktrees/warm"
case "${1:-refresh}" in
  seed)
    dest="${2:?usage: warm-worktree.sh seed DIR}"
    [ -d "$warm/dist-newstyle" ] || { echo "no warm build at $warm" >&2; exit 1; }
    [ -e "$dest/dist-newstyle" ] && { echo "$dest/dist-newstyle exists" >&2; exit 1; }
    cp -a "$warm/dist-newstyle" "$dest/dist-newstyle"
    # Older donors have no -O0 build; mutate.sh then builds one cold.
    if [ -d "$warm/dist-mutate" ] && [ ! -e "$dest/dist-mutate" ]; then
      cp -a "$warm/dist-mutate" "$dest/dist-mutate"
    fi
    cp "$primary/cabal.project.local" "$dest/cabal.project.local"
    echo "seeded $dest from $(git -C "$warm" rev-parse --short HEAD)"
    ;;
  refresh)
    git -C "$primary" fetch -q origin
    if [ ! -d "$warm" ]; then
      git -C "$primary" worktree add -q --detach "$warm" origin/main
    else
      git -C "$warm" checkout -q --detach origin/main
    fi
    cp "$primary/cabal.project.local" "$warm/cabal.project.local"
    (cd "$warm" && "$primary/script/with-build-lock.sh" cabal build -v0 all)
    # The same flags as script/mutate.sh, or the seeded build is invalidated.
    (cd "$warm" && "$primary/script/with-build-lock.sh" cabal build -v0 pawl-test-suite --builddir=dist-mutate --ghc-options=-O0)
    echo "warm at $(git -C "$warm" rev-parse --short HEAD)"
    ;;
  *)
    echo "usage: warm-worktree.sh [refresh | seed DIR]" >&2
    exit 2
    ;;
esac
