#!/usr/bin/env sh

# Canonical form for the card corpus: `jq --sort-keys` output, which is a
# two-space indent with object keys sorted, plus exactly one trailing newline.
#
# jq has neither a check mode nor in-place editing, so this wrapper supplies
# both for the `json` hook in .hooky.kdl. Usage: `format-json.sh MODE FILE...`,
# where MODE is `check` (report unformatted files, exit 1) or `fix` (rewrite
# them in place).

set -o errexit
set -o nounset

self=$0
mode=$1
shift

case $mode in
  check | fix) ;;
  *)
    echo "$self: unknown mode: $mode" >&2
    exit 2
    ;;
esac

status=0
for file; do
  formatted=$(jq --sort-keys . "$file")
  if [ "$formatted" = "$(cat "$file")" ]; then
    continue
  fi
  case $mode in
    check)
      echo "not formatted: $file"
      status=1
      ;;
    fix)
      printf '%s\n' "$formatted" >"$file"
      ;;
  esac
done

exit $status
