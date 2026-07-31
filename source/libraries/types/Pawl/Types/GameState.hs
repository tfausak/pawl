module Pawl.Types.GameState where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Types.ActivePlayerEffect (ActivePlayerEffect)
import Pawl.Types.ActiveReplacement (ActiveReplacement)
import Pawl.Types.Combat (Combat)
import Pawl.Types.ContinuousEffect (ContinuousEffect)
import Pawl.Types.Decider (Decider)
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import Pawl.Types.ExtraTurn (ExtraTurn)
import Pawl.Types.GameEvent (GameEvent)
import Pawl.Types.LastKnown (LastKnown)
import Pawl.Types.Mana (Mana)
import Pawl.Types.MonarchWatch (MonarchWatch)
import Pawl.Types.Object (Object)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Phase (Phase)
import Pawl.Types.Player (Player)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.RestartSignal (RestartSignal)
import Pawl.Types.Result (Result)
import Pawl.Types.Timestamp (Timestamp)

data GameState = MkGameState
  { objects :: Map ObjectId Object,
    library :: Map PlayerId (Seq ObjectId),
    hand :: Map PlayerId (Seq ObjectId),
    graveyard :: Map PlayerId (Seq ObjectId),
    battlefield :: Set ObjectId,
    exile :: Set ObjectId,
    -- CR 400.1: the command zone -- a shared collection (not per-player), keyed
    -- into `objects` like `battlefield`/`exile`. Emblems live here; their static
    -- abilities are gathered live by the projection (Pawl.Engine.Projection.gather).
    command :: Set ObjectId,
    stack :: [ObjectId],
    players :: Map PlayerId Player,
    -- CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map PlayerId Mana,
    -- CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat,
    -- CR 608.2i: what happened this turn, in order. Appended by the
    -- change-and-emit funnels (Event.changeZone, Event.createTokens,
    -- Damage.applyDamage) and by Engine.runStep's step-begin emission; NEVER
    -- cleared by a reader. Cleared with both watermarks at turn handoff
    -- (Engine.handoffTurn) -- not at cleanup, which is still part of this turn.
    events :: Seq GameEvent,
    -- CR 608.2h / 113.7a: last known information, keyed by the id an object had
    -- BEFORE it left a zone. "If the effect requires information from a specific
    -- object, including the source of the ability itself, the effect uses the
    -- current information of that object if it's in the public zone it was
    -- expected to be in; if it's no longer in that zone … the effect uses the
    -- object's last known information."
    --
    -- Every zone change mints a fresh id (CR 400.7, Event.changeZoneAttaching),
    -- so a departed object's OLD id names nothing in `objects`. That is exactly
    -- the condition under which this map is the answer, and it is why the key is
    -- the pre-move id rather than the incarnation's: the id an ability on the
    -- stack still carries as its source is the old one.
    --
    -- Written by the same funnel that records the Moved event, from the same
    -- snapshot value, so the two cannot disagree. Both are kept because they
    -- answer different questions: the log answers "what happened, in order"
    -- (CR 608.2i) and needs the NEW id for an enters trigger to scan, while this
    -- answers "what was that object" by the OLD id, in one lookup rather than a
    -- backwards scan.
    --
    -- The two are also read TOGETHER, by Event.eventTriggers: an entry event in
    -- the log names an id this map answers for once the permanent has already
    -- left, which is how a permanent that enters and dies inside one CR 117.5
    -- settle still gets its CR 603.6a entry trigger scanned.
    --
    -- Grows for the whole game, deliberately: an entry can be needed arbitrarily
    -- later (a delayed trigger's source, CR 603.7d), so there is no point at
    -- which pruning is provably safe. Correctness over footprint, per the
    -- project's standing guidance.
    lastKnown :: Map ObjectId LastKnown,
    -- CR 117.5: how far the trigger scan has consumed. Everything at or after
    -- this index is unscanned. Consumption is an index bump; the record stays.
    scannedThrough :: Natural,
    -- CR 704.5h ("since the last state-based action check"): how far the
    -- state-based-action damage read has consumed.
    damageScannedThrough :: Natural,
    -- CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order. Appended by Resolve's ArmDelayedTrigger; an entry is removed as it
    -- fires (CR 603.7b) unless it states a duration, in which case one of the
    -- Pawl.Engine.Expiry sweeps ends it instead. NOT cleared at turn handoff -- "at the
    -- beginning of the next end step" survives into the next turn if this turn's
    -- end step passed before the ability was armed, and a stated duration is the
    -- entry's own business rather than the handoff's.
    delayedTriggers :: Seq DelayedTrigger,
    -- CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with an expiry the Pawl.Engine.Expiry sweeps consult.
    -- Static-ability effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect],
    -- CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with an expiry the
    -- Pawl.Engine.Expiry sweeps consult (CR 514.2) and a use count (CR 614.3). The event-pipeline
    -- analog of continuousEffects; a permanent's STATIC replacement abilities are
    -- not here -- the projection re-derives those live. Pawl.Engine.Replacement reads it.
    replacements :: [ActiveReplacement],
    -- CR 611.1 / 613.11: stored PLAYER and RULES-modifying continuous effects
    -- from resolutions (Silence), each with an expiry the Pawl.Engine.Expiry sweeps
    -- consult. The third carrier sharing that vocabulary. A permanent's printed
    -- player abilities are NOT here -- Pawl.Engine.PlayerEffect re-derives those live.
    playerEffects :: [ActivePlayerEffect],
    -- The seating order -- CR 800.5 (or CR 806.3 for Grand Melee) only says
    -- players determine SOME seating order, "by any mutually agreeable
    -- method"; it does not say turnOrder is it. That comes from CR 103.1's
    -- last sentence, which does: "The game's default turn order begins with
    -- the starting player and proceeds clockwise" -- so this field is that
    -- seating order, rotated so the starting player is first. It lists every
    -- player who BEGAN this game and is never shortened. Who is still IN the
    -- game is Game.stillPlaying, and every departure-aware read filters
    -- through that on top of this.
    --
    -- Three rules depend on a departed player keeping their seat:
    --   * CR 800.4m -- the seat is how the handoff knows when a departed player's
    --     turn WOULD have begun.
    --   * CR 800.4a -- "priority passes to the next player in turn order who's
    --     still in the game" needs the departed player's own position to find
    --     their successor.
    --   * CR 729.1b lets a subgame's outcome mean something in the main game --
    --     "the effect may say that something happens in the main game to the
    --     winner or loser of the subgame". Its real customer is Shahrazad, whose
    --     own text is "each player who doesn't win the subgame", so the set that
    --     effect needs is the full starting roster minus the winner (#138).
    -- Pruning on departure makes all three impossible and buys nothing.
    turnOrder :: [PlayerId],
    activePlayer :: PlayerId,
    phase :: Phase,
    -- CR 500. The steps still scheduled this turn, in order; `phase` is the one
    -- in progress. The turn is DATA: CR 508.8 drops steps from this, CR 510.4 and
    -- 500.8/500.9 splice steps and phases into it. `Turn.allPhases` is the
    -- template a new turn refills from (see Engine.handoffTurn).
    remaining :: Seq Phase,
    priority :: Maybe PlayerId,
    passes :: Natural,
    turnNumber :: Natural,
    result :: Maybe Result,
    -- CR 727.4: raised while a restart has replaced this game underneath the
    -- frames still running it, so Engine.priorityLoop and Engine.runStep unwind
    -- to the rebuilt turn 1 instead of acting on it. Transient: Engine.runStep
    -- lowers it as that turn's untap step begins.
    restartSignal :: RestartSignal,
    nextObjectId :: ObjectId,
    -- CR 613.7: the monotonic source of timestamps for objects (at creation) and
    -- stored continuous effects (at CR 611 creation). See Timestamp.
    nextTimestamp :: Timestamp,
    drewFromEmpty :: Set PlayerId,
    landPlayed :: Set PlayerId,
    -- CR 723.1: pending player-controlling effects, keyed by the player to be
    -- controlled. Map.insert overwrites (CR 723.1a, last created wins). Promoted
    -- to activeControl at the actual start of that player's turn (CR 723.1b).
    pendingControl :: Map PlayerId Decider,
    -- CR 723.1/723.3: the decider controlling the ACTIVE player this turn, if any.
    -- Only the active player is ever controlled during their turn, so one Maybe
    -- suffices. Overwritten every turn start, so control ends at the next turn's
    -- beginning (CR 723.1).
    activeControl :: Maybe Decider,
    -- CR 725.1/725.3: the monarch, a single game-wide player designation (at most
    -- one at a time). Nothing until a player becomes the monarch. On GameState,
    -- not Player, because it is one designation, not a per-player counter.
    monarch :: Maybe PlayerId,
    -- CR 725 (Palace Jailer): objects exiled "until an opponent becomes the
    -- monarch", keyed by the exiled incarnation id to the watch that ends the
    -- exile -- the effect's controller, plus the monarch as of the last look, so
    -- that a CHANGE of crown can be told from an opponent merely holding it (see
    -- MonarchWatch). Not an Expiry: the Expiry sweeps are delete-and-recompute
    -- and cannot perform the return zone change.
    exiledUntilMonarch :: Map ObjectId MonarchWatch,
    -- CR 500.7: the extra turns that have been created and not yet taken, MOST
    -- RECENTLY CREATED FIRST -- "the most recently created turn will be taken
    -- first". A stack, not a queue, and a list precisely because the style guide
    -- reserves lists for stacks (GameState.stack is the other one). Pushed by
    -- Resolve's TakeExtraTurn arm, popped by Engine.handoffTurn before the
    -- seating order is consulted at all.
    --
    -- One entry per turn, so two effects giving one player an extra turn each
    -- are two entries: CR 500.7's "the extra turns are added one at a time" is
    -- what makes them countable rather than a set of players. Each entry also
    -- carries the steps and phases THAT turn skips (Pawl.Types.ExtraTurn), which
    -- is how a CR 500.11 skip printed as "the untap step of that turn" (Savor the
    -- Moment) names a turn without a reference that could dangle.
    extraTurns :: [ExtraTurn],
    -- CR 500.7 / 103.1: while an EXTRA turn is under way, the seat the ordinary
    -- turn order resumes from -- the active player of the most recent turn that
    -- was not an extra one. Nothing on an ordinary turn, where that seat IS
    -- GameState.activePlayer and there is nothing to remember.
    --
    -- CR 500.7 adds a turn "directly after the specified turn" and takes nothing
    -- away, so the turn that would have followed the specified turn still
    -- follows it. Anchoring the CR 103.1 walk on activePlayer instead would make
    -- an extra turn CONSUME the taker's ordinary turn whenever the two are
    -- different players -- Time Warp aimed at an opponent, which is exactly what
    -- Pawl.TurnSpec's "bob's own turn still follows" case pins.
    turnAnchor :: Maybe PlayerId
  }
  deriving (Eq, Show)
