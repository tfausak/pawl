#!/usr/bin/env bash
# Run one command under the machine-wide build lock, so only one cabal
# invocation runs at a time across every worktree. The GHC job semaphore only
# shares compile slots; it does not stop three worktrees building at once,
# which on an 8 GB machine is unusable. Usage:
#   script/with-build-lock.sh cabal test ...
# The lock is a directory (mkdir is atomic) holding the owner's PID; a lock
# whose PID is dead is stale and is taken over.
set -euo pipefail
lock="${PAWL_BUILD_LOCK:-${TMPDIR:-/tmp}/pawl-build.lock}"
lock="${lock%/}"
while ! mkdir "$lock" 2>/dev/null; do
  owner=$(cat "$lock/pid" 2>/dev/null || true)
  if [ -n "$owner" ] && ! kill -0 "$owner" 2>/dev/null; then
    rm -rf "$lock"
    continue
  fi
  sleep 15
done
echo $$ > "$lock/pid"
trap 'rm -rf "$lock"' EXIT INT TERM
"$@"
