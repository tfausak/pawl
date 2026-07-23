# M5 — Player control, restart, subgames (umbrella spec)

*Design pass 2026-07-23, at M4.5 P11's completion (all of M4.5 done). This is an
**umbrella** spec: it fixes the milestone's shape — thesis, phase decomposition,
dependency ordering, per-phase gate card + axis + falsifier, deferred backlog,
exit criterion, and hand-off to M6. **Each phase gets its own detailed spec and
plan under `docs/superpowers/{specs,plans}/` when it is next up**, exactly as
M4a–M4h and M4.5 P1–P11 each did under their umbrella. Nothing here is
implementable as-is; it is the map the phase specs are derived from. Tracks
GitHub issue #8 (milestone M5).*

## 0. Why this milestone exists

M5 is design.md §3's **"nightmares, as rules sections"**: they are *numbered
sections of the closed half*, not exotic opcodes. Three sections, each a phase:

- **CR 723 Controlling Another Player** — Mindslaver.
- **CR 727 Restarting the Game** — Karn Liberated.
- **CR 729 Subgames** — Shahrazad.

Two of the three are the payoff of bets placed on **day one** and deliberately
retired now that the axes they sit on are complete (M4.5's hand-off):

- **723 is the `Decider` bet** (design.md §2.3, "Decider ≠ player"). The
  decider-vs-player split was built into the prompt type on day one and wired
  through M4a (Mindslaver's whole-turn control). 723 is mostly *closing out* an
  existing mechanism, not building a new axis.
- **729 is the suspended-continuation bet** (design.md §2.1, §3). "Yours nests a
  `Program Prompt` inside a `Program Prompt` — which is a function call." XMage
  lists Shahrazad unimplementable because its architecture can't nest a game in a
  game; mtg-pure's prompts are IO callbacks, so a game in flight can't be
  snapshotted or resumed and Mindslaver is a `-- TODO`. pawl's
  `StateT GameState (Program Prompt)` makes a subgame a recursive call.
- **727 sits between them** (design.md §3): "restart-in-place is tractable even
  in a mutable engine; nesting a subgame to completion and resuming the parent is
  the part that isn't." It is the lower-risk of the two game-lifecycle phases and
  introduces the primitive 729 reuses.

**This milestone is numbered a whole number** (unlike the `.5` interstitials M3.5
and M4.5): it is the design.md §3 milestone M5, cited by number throughout the
design doc, and it does not renumber M6–M7.

## 1. Scope

**In scope** — the three closed-half rules sections above, as three phases (§3),
in numbered order (§4). Each phase = one rules section closed to the depth its
one real gate card (or a sanctioned labeled-synthetic stand-in) demands.

**Out of scope, deferred to backlog** (§4) — CR 732 (Taking Shortcuts) and CR 733
(Handling Illegal Actions) per issue #8's owner comment; the card-driven and
subsystem-blocked slices of 723/727/729 that drag in machinery M5 does not build;
and the design.md §6 out-of-scope set (unchanged).

**Not in this milestone by construction** — the M4-tail *vocabulary* (new
opcodes, trigger conditions, counter names, cost instances). These are **VOCAB**
(census §5), grow forever on the seams M4/M4.5 built, and are tracked by opcode
breadth (design.md §4), never by this spec.

### The definition-of-done for each phase (design.md §4 corollary)

A phase is **not done** until its rules section has (a) a routine / axis in the
type system and (b) a **real, recognizable gate card** exercised in a
*gameplay-level* test — a scenario that plays through the driver loop and asserts
on game state, not a unit test of a routine in isolation. A **labeled synthetic
crutch with a documented expiry** naming the card that retires it is legitimate
*only* when a real card would drag in something not yet built (the `Landform`
precedent; M5b uses exactly this). The gate card is how each phase proves the
engine **does** the right thing.

## 2. The two invariants every phase honors

These outrank the phase plan (CLAUDE.md, "Executing a plan"):

1. **The rules core reads a *classification*, never an effect's identity.**
   - 723 stays a `Decider` swap at `Decide.deciderFor` — the engine never cases on
     "is this Mindslaver." Control is an indirection, not an opcode dispatch.
   - 729's outcome plumbing stays generic: a `PlaySubgame` opcode binds the
     subgame's result to a named slot, and "the loser loses half their life" is
     **ordinary follow-on card data** (existing life-loss vocabulary reading the
     slot). `Pawl.Resolve` never grows a `case … Shahrazad`.
   - 727's restart is a routine keyed off a generic "restart the game" effect, not
     off Karn's identity.
2. **The engine makes no choices.** Every subgame choice (down to "randomly
   determine which player goes first," CR 729.2) is a prompt through the same
   interpreter (randomness-as-prompt, design.md §2.2). A player being controlled
   (723) has their prompts routed to the controller's `Decider`; nothing is
   elided. Any elision carries a named expiry.

## 3. The phases

Three phases, one rules section each. Each is **one routine/axis + the real card
that falsifies its naive implementation** (the M2/M3/M4 discipline). Gate cards
are **candidates** — each phase's own spec re-derives its exact gate from real
cards and `rules.txt` when written. **Every CR number below was verified against
`docs/rules.txt` at this design pass** (the 723/727/729 numbering matches
design.md §3 and the rules.txt section headers); phase specs re-verify before the
number drives code (CLAUDE.md: never trust recalled Magic rules).

| # | Phase (CR section) | Gate card *(candidate)* → falsifier | New routine / axis | CR *(verified 2026-07-23)* |
|---|---|---|---|---|
| **M5a ✅** | **723 Controlling Another Player** | **Mindslaver** at *gameplay level* → a naive "a player always decides for themselves" cannot make player A make every one of player B's choices for B's whole turn; and the small correctness points below each have their own micro-falsifier | Gameplay gate test for the **existing** whole-turn mechanism; **723.1a** last-created-effect-wins overwrite; **723.6** the controller can't force a concede; **723.5a** the controller spends only the controlled player's resources | 723.1 / 723.1a / 723.3 / 723.5 / 723.5a / 723.6 |
| **M5b** | **727 Restarting the Game** | **labeled-synthetic** "restart the game" effect (documented expiry → **Karn Liberated**) → restart is **not** `Setup.emptyGame` + `Setup.newGame`: it must rebuild from the current game's *actual cards* (ownership preserved, CR 727.2) and set the starting player to the restart's controller (CR 727.1a), which a fresh-setup path gets wrong | a `restartGame` routine; a shared **`startGameFromCards`** primitive (build per-player libraries from an existing object pool, not from `Deck` definitions); CR 727.4 finish-just-before-first-untap timing | 727.1 / 727.1a / 727.2 / 727.3 / 727.4 |
| **M5c** | **729 Subgames** — *the M5 go/no-go* | **Shahrazad** → XMage calls it unimplementable (can't nest a game in a game); a mutable/IO-callback engine can't resume the parent. Falsifiers a naive design must survive: the subgame's outcome must feed the **parent** effect (729.1b), owned traditional cards must funnel **back** to the main library and reshuffle (729.5), and **Shahrazad-in-Shahrazad must nest arbitrarily** (729.6) | subgame = `runStateT playGame subState` run **inside** the resolving effect, sharing the `Program Prompt` interpreter; `startGameFromCards` from each player's **library only** (729.2); a generic **`PlaySubgame`** opcode binding the subgame outcome to a slot; funnel-back at subgame end (729.5) | 729.1 / 729.1a / 729.1b / 729.2 / 729.3 / 729.4 / 729.5 / 729.6 |

### Notes the phase specs must not lose

- **M5a is a close-out, not a new axis.** The map at this design pass confirms the
  substrate is already built and wired: `Pawl.Type.Decider`
  (`newtype Decider = MkDecider PlayerId`), `Pawl.Decide.deciderFor` (the single
  "who actually chooses" indirection, consulted at every prompt site),
  `GameState.pendingControl :: Map PlayerId Decider` and
  `GameState.activeControl :: Maybe Decider`, `Effect.ControlPlayerNextTurn
  SlotName` (Mindslaver's opcode, resolved in `Pawl.Resolve`), and the
  promotion `pendingControl → activeControl` at `Engine.handoffTurn` (CR 723.1b:
  a pending effect waits for the player to *actually take* a turn). What M5a adds
  is proof and edges, not machinery:
  - a **gameplay-level** gate test (scripted interpreter: A Mindslavers B, then A
    makes B's action/target/combat/mode choices for B's whole turn, and control
    lapses at the following turn boundary — CR 723.1/723.3, the affected player is
    still the active player);
  - **723.1a** — multiple player-controlling effects on the same player overwrite,
    last-created wins. `pendingControl` is a `Map PlayerId Decider`, so a second
    insert already overwrites; the phase asserts it and pins the ordering to
    *creation* time (timestamp), not insert order, if they can differ;
  - **723.6** — the controller cannot make the controlled player concede. Concede
    is a player-level action (CR 104.3a) that must **not** route through the
    `Decider`; verify the concede path reads the true player, not `deciderFor`;
  - **723.5a** — costs paid for the controlled player draw only on that player's
    resources (cards, mana). Verify the decider decides *which* action but the
    payment debits the controlled player's pools/zones, not the controller's.
- **723.2 (limited-duration control) is deferred, card-driven.** The only two real
  cards are **Word of Command** and **Opposition Agent**, and both drag in
  machinery M5 does not build: Word of Command makes the controlled player *cast a
  card the controller chooses from their hand, during resolution, under special
  mana-payment restrictions*; Opposition Agent controls a player *only while they
  search their library* and needs the search-replacement. Filed as an issue with
  a **card-driven** expiry trigger, not scheduled to a milestone. The general
  shape it will need — a control channel that begins and ends off the turn
  boundary — is noted here so M5a does not accidentally hard-wire control to the
  turn handoff in a way 723.2 would have to unpick.
- **M5b restart is replace-in-place: no nesting, no return.** It rebuilds the
  single `GameState` slot. The falsifier is that restart ≠ fresh setup:
  - CR 727.2 — *all* Magic cards involved in the ended game are in the new game,
    ownership preserved regardless of where they were. In M5's scope the card pool
    is exactly the current game's objects; every object returns to its owner's new
    library. (CR 727.2's "cards brought in from outside the game" example — Living
    Wish — is **deferred**: it needs an outside-the-game / sideboard subsystem.)
  - CR 727.1a — the starting player of the new game is the controller of the
    ability that restarted the game, **not** the normal first-player determination.
  - CR 727.3 — a player with fewer than seven cards loses at the first upkeep's SBA
    check; this reuses the existing draw-from-empty / opening-hand SBA path.
  - CR 727.4 — the restart effect finishes resolving *just before the first turn's
    untap step*; any additional instructions run then, with no player holding
    priority. For the **labeled-synthetic** gate this rider is empty; **Karn's**
    real rider (put the exempted cards onto the battlefield under its controller)
    is the deferred part.
  - **Deferred to card-driven backlog (→ Karn Liberated):** CR 727.5 / 727.5a — the
    exemption that leaves designated cards in exile across the restart, and Karn's
    put-onto-battlefield instruction. The gate is therefore a **labeled synthetic**
    "restart the game" effect (the `Landform` crutch pattern), whose documented
    expiry names Karn Liberated. This is legitimate precisely because the only real
    card intrinsically bundles the deferred exemption.
- **M5c subgame = a function call, arbitrarily nestable.** The design-committed
  shape (design.md §2.1/§3):
  - A subgame runs as `runStateT playGame subState` **inside** the resolution of
    the effect that created it (Shahrazad's resolution), producing a `Result`
    together with the subgame's final state. The parent `GameState` sits in the
    outer `StateT` frame, untouched while the subgame runs. Prompts issued by the
    subgame flow out through the **same** `Program Prompt` interpreter — this is
    the "function call" XMage and mtg-pure structurally cannot do.
  - **Arbitrary nesting (CR 729.6) is free recursion:** a subgame resolving another
    Shahrazad calls `playGame` again a level deeper. No `GameState` stack field is
    needed; the call stack *is* the game stack.
  - **Subgame prompts are untagged** (owner decision, 2026-07-23). Pure and
    scripted test interpreters do not need to distinguish which game a prompt
    belongs to, so M5's gameplay tests work directly. A real client's need for
    game-context (which game am I answering; CR 723.4 information visibility) is an
    **M7/interpreter** concern; the `Prompt` GADT is not changed for it now. Noted
    as a deferred item so M7 knows it is owed.
  - **Construction (CR 729.2):** each player moves *only their main-game library*
    into a fresh subgame, shuffles (a `Shuffle` prompt), and the subgame starts per
    CR 103 with a *randomly determined* first player (a randomness prompt). This is
    `startGameFromCards` (introduced by M5b) fed each player's current library
    objects. Note the main-game hand/battlefield/graveyard/etc. do **not** enter
    the subgame (CR 729.2) — Shahrazad is played from what remains of the library.
    CR 729.3's seven-card-minimum loss reuses the existing first-upkeep SBA.
  - **Outcome plumbing (CR 729.1b / 729.5):** the creating spell finishes resolving
    *after* the subgame ends. A generic `PlaySubgame` opcode runs the nested game
    and binds its outcome (the loser, or the full `Result`) to a named slot; the
    remaining opcodes on Shahrazad's card are ordinary vocabulary that read the
    slot (life loss). The phase spec settles the exact bound value; note that
    `Pawl.Type.Result` is today `Won PlayerId | Drawn`, from which a 2-player
    subgame's loser is derivable but which may need widening for "the loser."
  - **Teardown (CR 729.5):** at subgame end, each player takes all traditional
    cards they own anywhere in the subgame (library, hand, battlefield, graveyard,
    exile, phased-out) into their main-game library and reshuffles; all other
    subgame objects and the subgame's zones cease to exist (they were fresh
    `ObjectId`s — CR 400.7 — so this is dropping the subgame state, not reverting
    the parent). The main game resumes from exactly where it was discontinued.
  - **Deferred (subsystem-blocked):** CR 729.2a supplementary decks / nontraditional
    cards, 729.2b/729.5b Vanguard, 729.2c/729.5c Commander, and 729.4a/729.5
    (second sentence) cards brought *into* a subgame from a main game plus the
    main-game leave-the-zone triggers they queue until the main game resumes — all
    ride subsystems (nontraditional cards, Vanguard, Commander, outside-the-game)
    M5 does not build. 729.4b (a player's main-game counters are outside the
    subgame; subgame counters cease at end) falls out of the fresh-state / teardown
    for free and is asserted, not built.

## 4. Ordering, and what is deferred

**Recommended linear order: M5a → M5b → M5c as numbered.** A clean difficulty ramp
and one real dependency edge:

- **M5a first** — a close-out of already-built machinery (design.md §2.3's
  `Decider` bet, discharged at M4a). Lowest risk; proves the substrate at gameplay
  level and clears the three small 723 correctness edges before the game-lifecycle
  work.
- **M5b second** — introduces **`startGameFromCards`**, the "build a game from an
  existing object pool (not from `Deck` definitions)" primitive. Restart is the
  simpler creation case: replace-in-place, no return.
- **M5c last — the M5 go/no-go** (design.md §3/§5's canary posture, applied to the
  suspension model rather than the layer system). It **reuses** `startGameFromCards`
  and adds only the nesting-and-return: a recursive `runStateT`, funnel-back, and
  the generic outcome opcode. If the "subgame is a function call" thesis is going
  to break, it breaks here — but it breaks on top of a proven creation primitive,
  not mid-nightmare.

**Hard dependency edge (the only one):** `M5b → M5c` (subgames reuse restart's
`startGameFromCards`). M5a is independent of both and could reorder, but leading
with it is free and de-risks nothing to defer.

**Deferred to backlog** — each becomes an issue with an expiry trigger; the
card-driven ones fire when a card demands them and have no scheduled date
(CLAUDE.md, the `expires:card-driven` label):

- **723.2 limited-duration control** (Word of Command, Opposition Agent) —
  card-driven; needs cast-during-resolution + mana restrictions / search-replacement.
- **Full Karn Liberated** — CR 727.5/727.5a exemption + the put-onto-battlefield
  rider; card-driven, retires M5b's labeled-synthetic gate.
- **Outside-the-game cards** (CR 727.2 Living Wish example) — needs a sideboard /
  wish subsystem.
- **Nontraditional / Vanguard / Commander subgame movement** (CR 729.2a–c,
  729.5a–c) — subsystem-blocked.
- **Cards brought into a subgame + their deferred main-game triggers** (CR 729.4a,
  729.5 second sentence) — subsystem-blocked.
- **Subgame prompt tagging / game-context** (CR 723.4 information visibility) — an
  M7/interpreter concern; the engine tracks no per-game prompt identity in M5.
- **CR 732 Taking Shortcuts** — a client concern (issue #8 owner comment); the
  engine may later expose a shortcut-proposal seam, but not in M5.
- **CR 733 Handling Illegal Actions** — the engine forbids illegal actions in the
  first place (issue #8 owner comment; it is a paper-Magic rule). Ignored
  indefinitely, not merely deferred.
- **Adjacent, untouched:** CR 728 rad counters and CR 731 day/night ride the GAP-S
  player-substrate (P10) and the day/night designer-pick noted at P11; they are
  backlog, not M5.
- **design.md §6 out-of-scope, unchanged** — ante, dexterity, draft/Conspiracy,
  un-set social/art-content, contraptions.

## 5. Exit criterion and hand-off

**Exit criterion.** Each of the three phases has (a) its routine / axis in the type
system and (b) a real gate card — or the sanctioned labeled-synthetic for M5b —
with a passing *gameplay-level* test: Mindslaver controls a full turn (M5a), a
restart rebuilds the game from its own cards with the right starting player (M5b),
and Shahrazad plays a nested subgame to completion, feeds its loser back to the
parent, funnels cards back, and nests within itself (M5c). At that point
design.md §3's "nightmares" are retired and the **closed half is functionally
complete** for its flagged surface (M4.5 finished the axes; M5 finishes the
rules-section nightmares that sit on them).

**Hand-off to M6 (the transpiler).** With control, restart, and subgames closed,
the closed half has shape enough that M6's two inputs — LLM bulk translation of
MTGJSON oracle text → DSL draft, and Forge's `cardsfolder` as reference semantics
(license permitting) — land on finished axes. The card-count metric stays deferred
throughout; breadth (opcode/axis/section coverage) remains the progress signal
(design.md §4).

## 6. Tracking

- This umbrella tracks issue **#8** (milestone M5). When each phase lands, record
  its completion entry in `docs/progress.md` and tick the phase here.
- Each deferral in §4 gets its own issue at the phase that discovers its code site,
  carrying status, rationale, and expiry trigger, cited inline as `(#N)` per
  CLAUDE.md's "file the issue, cite it inline" rule. The `expires:card-driven`
  deferrals (723.2, full Karn, outside-the-game cards) have no scheduled date;
  the subsystem-blocked ones name the subsystem, not a milestone.
- The design.md §3 M5 subsection and this umbrella's phase table are the two places
  the phase list is authoritative; keep them in step as phases land.

## 7. Process

Each phase is brainstormed → specced → planned → implemented on its own, in the
recommended order (§4), following the standard milestone loop (`docs/workflow.md`):
`superpowers:brainstorming` for the phase spec, `superpowers:writing-plans` for
the plan, TDD per CLAUDE.md, one small complete commit per plan task on `main`,
then the close-out (invariant audit, rules-correctness pass, `progress.md` entry,
`CLAUDE.md` status bullet **replaced**, this umbrella's checkbox ticked). Model
tiering per `docs/workflow.md`: Fable/Opus for the specs and the M5c go/no-go
review, Sonnet for implementer subagents, Haiku for `rules.txt` citation checks.
