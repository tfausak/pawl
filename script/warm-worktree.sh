#!/usr/bin/env bash
# Keep one worktree at origin/main with a finished build, as the donor for
# every fresh worktree's dist-newstyle. Run after each merge; the build is
# incremental, so it costs the merge's diff and nothing else. Usage:
#   script/warm-worktree.sh            # refresh the donor
#   script/warm-worktree.sh seed DIR   # clone its dist-newstyle into DIR
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
    echo "warm at $(git -C "$warm" rev-parse --short HEAD)"
    ;;
  *)
    echo "usage: warm-worktree.sh [refresh | seed DIR]" >&2
    exit 2
    ;;
esac
