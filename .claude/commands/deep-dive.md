---
description: Work a merged PR's follow-up issues together and close out the concern behind them
argument-hint: <pr-number>
---

Deep-dive PR #$1: work its follow-ups together and close out the concern.

1. Read the merged PR (`gh pr view $1`) and collect every issue it filed — check
   its body's own "issues filed" section AND `gh issue list` for issues created
   around its merge date. Read each one in full.

2. **Find the defect that is in NO issue.** Look for a correctness argument the
   PR left resting on a comment rather than a test — a "this is only safe
   because the one card in the pool happens to X" caveat. That is usually the
   real high-level concern, and the follow-up issues are its symptoms.

3. For each issue needing a card, survey Scryfall exhaustively (search by oracle
   text, never from memory) and **tabulate** the candidates against what each one
   drags in that pawl lacks. Prefer the card that forces the defect from step 2
   over the card the issue happens to name. Check each capability claim against
   the code before believing it.

4. **Then stop and show me:** the issues in scope, the unfiled defect, the
   candidate cards with their costs, and the scope fork you want me to decide.
   Do not start building until I answer.

5. Build it TDD, red first. For each falsifier, **prove it is red for a rules
   reason** and not merely a missing card — temporarily revert the fix, capture
   the failure output, restore it. Put that output in the PR body.

6. Self-review before opening the PR, and specifically: re-check every CR
   citation you added against `docs/rules.txt`; re-read every comment your rename
   or rewrite touched, **including module headers and neighbouring prose you did
   not edit**; and confirm no comment still cites an issue this branch closes.

7. After pushing, sweep the open issues once more for three things: bodies made
   wrong by your renames, issues your work **narrowed** rather than closed
   (comment on those), and deficiencies your own fix made conspicuous that
   nobody has filed (file those).

Closing the concern is the goal — filing issues for genuinely separate units is
expected, not a failure. Report the PR and stop; don't wait for CI.

`CLAUDE.md` already carries the standing rules (rules.txt over recalled Magic,
card-driven work means adding the card, draft-then-ready, what the PR body must
contain). Don't restate them; follow them.
