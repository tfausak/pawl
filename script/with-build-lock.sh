#!/usr/bin/env bash
# Run one command under the machine-wide build lock, so only one cabal
# invocation runs at a time across every worktree. The GHC job semaphore only
# shares compile slots; it does not stop three worktrees building at once,
# which on an 8 GB machine is unusable. Usage:
#   script/with-build-lock.sh cabal test ...
# The lock is a hard link created with ln, which is atomic and fails when the
# target exists; it holds the owner's PID. Taking over a lock whose owner has
# died is an atomic rename, so of several waiters exactly one wins. The path
# is fixed rather than TMPDIR-relative so every shell locks the same file.
set -euo pipefail
lock="${PAWL_BUILD_LOCK:-/tmp/pawl-build.lock}"
mine="$lock.$$"
echo $$ > "$mine"
trap 'rm -f "$mine"' EXIT
while ! ln "$mine" "$lock" 2>/dev/null; do
  owner=$(cat "$lock" 2>/dev/null || true)
  if [ -n "$owner" ] && ! kill -0 "$owner" 2>/dev/null; then
    stale="$lock.stale.$$"
    if mv "$lock" "$stale" 2>/dev/null; then rm -f "$stale"; fi
    continue
  fi
  sleep 15
done
release() {
  if [ "$(cat "$lock" 2>/dev/null)" = "$$" ]; then rm -f "$lock"; fi
  rm -f "$mine"
}
trap 'release' EXIT
trap 'kill "$child" 2>/dev/null; release; exit 130' INT TERM
"$@" &
child=$!
wait "$child"
