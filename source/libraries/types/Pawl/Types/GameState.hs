module Pawl.Types.GameState where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Timestamp as Timestamp

data GameState = MkGameState
  { objects :: Map.Map ObjectId.ObjectId Object.Object,
    library :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    hand :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    graveyard :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    battlefield :: Set.Set ObjectId.ObjectId,
    exile :: Set.Set ObjectId.ObjectId,
    -- | CR 400.1: the command zone -- a shared collection (not per-player), keyed
    -- into `objects` like `battlefield`/`exile`. Emblems live here.
    command :: Set.Set ObjectId.ObjectId,
    stack :: [ObjectId.ObjectId],
    players :: Map.Map PlayerId.PlayerId Player.Player,
    -- | CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map.Map PlayerId.PlayerId Mana.Mana,
    -- | CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat.Combat,
    -- | CR 608.2i: what happened this turn, in order. Appended by the
    -- change-and-emit funnels and by Engine.runStep's step-begin emission; NEVER
    -- cleared by a reader. Cleared with both watermarks at turn handoff
    -- (Engine.handoffTurn) -- not at cleanup, which is still part of this turn.
    events :: Seq.Seq GameEvent.GameEvent,
    -- | CR 608.2h / 113.7a: last known information, keyed by the id an object had
    -- BEFORE it left a zone.
    --
    -- Every zone change mints a fresh id (CR 400.7), so a departed object's OLD
    -- id names nothing in `objects`. That is exactly the condition under which
    -- this map is the answer, and it is why the key is the pre-move id: the id
    -- an ability on the stack still carries as its source is the old one.
    --
    -- Written by the same funnel that records the Moved event, from the same
    -- snapshot value, so the two cannot disagree. Both are kept because they
    -- answer different questions: the log answers "what happened, in order"
    -- (CR 608.2i) and needs the NEW id for an enters trigger to scan, while this
    -- answers "what was that object" by the OLD id, in one lookup. They are also
    -- read TOGETHER by Event.eventTriggers, which is how a permanent that enters
    -- and dies inside one CR 117.5 settle still gets its CR 603.6a entry trigger
    -- scanned.
    --
    -- Grows for the whole game, deliberately: an entry can be needed arbitrarily
    -- later (a delayed trigger's source, CR 603.7d), so there is no point at
    -- which pruning is provably safe.
    lastKnown :: Map.Map ObjectId.ObjectId LastKnown.LastKnown,
    -- | CR 117.5: how far the trigger scan has consumed. Everything at or after
    -- this index is unscanned. Consumption is an index bump; the record stays.
    scannedThrough :: Natural.Natural,
    -- | CR 603.3a: a triggered ability is controlled by the player who controlled
    -- its source at the time it triggered. The trigger scan runs at the CR 117.5
    -- boundary rather than at each event, so by the time it asks, control may
    -- already have moved -- CR 514.2 ends an "until end of turn" control effect
    -- between CR 514.1's discard and CR 514.3a's placement. This is the answer as
    -- of the moment the OLDEST UNSCANNED event was recorded: every object a
    -- CR 613.1b layer-2 control effect named then, and the player it named it
    -- for. Read by Event.eventTriggers in preference to the live projection.
    --
    -- OVERRIDES ONLY, not the whole battlefield: an object no layer-2 effect
    -- names has its CR 110.2 default controller, which cannot change while it
    -- stays on the battlefield, so an absent id is answered live and gets the
    -- same answer.
    controlWhenTriggered :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | CR 704.5h ("since the last state-based action check"): how far the
    -- state-based-action damage read has consumed.
    damageScannedThrough :: Natural.Natural,
    -- | CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order. An entry is removed as it fires (CR 603.7b) unless it states a
    -- duration, in which case one of the Pawl.Engine.Expiry sweeps ends it
    -- instead. NOT cleared at turn handoff -- "at the beginning of the next end
    -- step" survives into the next turn if this turn's end step passed before
    -- the ability was armed.
    delayedTriggers :: Seq.Seq DelayedTrigger.DelayedTrigger,
    -- | CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with an expiry the Pawl.Engine.Expiry sweeps consult.
    -- Static-ability effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect.ContinuousEffect],
    -- | CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with an expiry the
    -- Pawl.Engine.Expiry sweeps consult (CR 514.2) and a use count (CR 614.3).
    -- The event-pipeline analog of continuousEffects; a permanent's STATIC
    -- replacement abilities are not here -- the projection re-derives those live.
    replacements :: [ActiveReplacement.ActiveReplacement],
    -- | CR 611.1 / 613.11: stored PLAYER and RULES-modifying continuous effects
    -- from resolutions (Silence), each with an expiry the Pawl.Engine.Expiry
    -- sweeps consult. A permanent's printed player abilities are NOT here --
    -- Pawl.Engine.PlayerEffect re-derives those live.
    playerEffects :: [ActivePlayerEffect.ActivePlayerEffect],
    -- | The seating order. CR 800.5 (or CR 806.3 for Grand Melee) only says
    -- players determine SOME seating order; CR 103.1's last sentence is what
    -- makes it the turn order, beginning with the starting player -- so this
    -- field is that seating order, rotated so the starting player is first. It
    -- lists every player who BEGAN this game and is never shortened. Who is
    -- still IN the game is Game.stillPlaying, and every departure-aware read
    -- filters through that on top of this.
    --
    -- Three rules depend on a departed player keeping their seat:
    --   * CR 800.4m -- the seat is how the handoff knows when a departed player's
    --     turn WOULD have begun.
    --   * CR 800.4a -- finding the departed player's successor in turn order
    --     needs their own position.
    --   * CR 729.1b, whose real customer is Shahrazad's "each player who doesn't
    --     win the subgame": the full starting roster minus the winner (#138).
    turnOrder :: [PlayerId.PlayerId],
    activePlayer :: PlayerId.PlayerId,
    phase :: Phase.Phase,
    -- | CR 500. The steps still scheduled this turn, in order; `phase` is the one
    -- in progress. The turn is DATA: CR 508.8 drops steps from this, CR 510.4 and
    -- 500.8/500.9 splice steps and phases into it. `Turn.allPhases` is the
    -- template a new turn refills from (see Engine.handoffTurn).
    remaining :: Seq.Seq Phase.Phase,
    priority :: Maybe PlayerId.PlayerId,
    passes :: Natural.Natural,
    turnNumber :: Natural.Natural,
    result :: Maybe Result.Result,
    -- | CR 727.4: raised while a restart has replaced this game underneath the
    -- frames still running it, so Engine.priorityLoop and Engine.runStep unwind
    -- to the rebuilt turn 1 instead of acting on it. Transient: Engine.runStep
    -- lowers it as that turn's untap step begins.
    restartSignal :: RestartSignal.RestartSignal,
    nextObjectId :: ObjectId.ObjectId,
    -- | CR 613.7: the monotonic source of timestamps for objects (at creation) and
    -- stored continuous effects (at CR 611 creation). See Timestamp.
    nextTimestamp :: Timestamp.Timestamp,
    -- | CR 104.4b: the timestamp as of the last time a player was offered an
    -- OPTIONAL action. The gap between this and nextTimestamp is how many events
    -- have happened with no player able to decide anything, which is
    -- Pawl.Engine.Engine.checkMandatoryLoop's heuristic for a loop of mandatory
    -- actions.
    --
    -- Advanced only by Pawl.Engine.Game.choose, so a new prompt site cannot
    -- forget to reset it. Conceding is deliberately not one of its callers: CR
    -- 104.3a lets a player concede at any time, so if that counted as the
    -- optional action, no loop would ever be mandatory.
    --
    -- Pawl.Engine.Setup also SETS it, at the three seams where a game begins
    -- (emptyGame, restartGame, subgameStateFrom) and where one ends back into its
    -- parent (funnelBack). Each sets it to the timestamp supply's value there, so
    -- neither a fresh game nor a resumed one inherits a gap run up elsewhere.
    lastChoice :: Timestamp.Timestamp,
    drewFromEmpty :: Set.Set PlayerId.PlayerId,
    landPlayed :: Set.Set PlayerId.PlayerId,
    -- | CR 723.1: pending player-controlling effects, keyed by the player to be
    -- controlled. Map.insert overwrites (CR 723.1a, last created wins). Promoted
    -- to activeControl at the actual start of that player's turn (CR 723.1b).
    pendingControl :: Map.Map PlayerId.PlayerId Decider.Decider,
    -- | CR 723.1/723.3: the decider controlling the ACTIVE player this turn, if any.
    -- Only the active player is ever controlled during their turn, so one Maybe
    -- suffices. Overwritten every turn start, so control ends at the next turn's
    -- beginning (CR 723.1).
    activeControl :: Maybe Decider.Decider,
    -- | CR 725.1/725.3: the monarch, a single game-wide player designation (at most
    -- one at a time). Nothing until a player becomes the monarch. On GameState,
    -- not Player, because it is one designation, not a per-player counter.
    monarch :: Maybe PlayerId.PlayerId,
    -- | CR 725 (Palace Jailer): objects exiled "until an opponent becomes the
    -- monarch", keyed by the exiled incarnation id to the watch that ends the
    -- exile -- the effect's controller, plus the monarch as of the last look, so
    -- that a CHANGE of crown can be told from an opponent merely holding it. Not
    -- an Expiry: the Expiry sweeps are delete-and-recompute and cannot perform
    -- the return zone change.
    exiledUntilMonarch :: Map.Map ObjectId.ObjectId MonarchWatch.MonarchWatch,
    -- | CR 500.7: the extra turns that have been created and not yet taken, MOST
    -- RECENTLY CREATED FIRST. A stack, not a queue, and a list precisely because
    -- the style guide reserves lists for stacks (GameState.stack is the other
    -- one). Popped by Engine.handoffTurn before the seating order is consulted.
    --
    -- One entry per turn, so two effects giving one player an extra turn each
    -- are two entries: CR 500.7 adds extra turns one at a time, which makes them
    -- countable rather than a set of players. Each entry also carries the steps
    -- and phases THAT turn skips, which is how a CR 500.11 skip printed as "the
    -- untap step of that turn" (Savor the Moment) names a turn without a
    -- reference that could dangle.
    extraTurns :: [ExtraTurn.ExtraTurn],
    -- | CR 500.7 / 103.1: while an EXTRA turn is under way, the seat the ordinary
    -- turn order resumes from -- the active player of the most recent turn that
    -- was not an extra one. Nothing on an ordinary turn, where that seat IS
    -- GameState.activePlayer and there is nothing to remember.
    --
    -- CR 500.7 adds a turn directly after the specified turn and takes nothing
    -- away, so the turn that would have followed it still follows it. Anchoring
    -- the CR 103.1 walk on activePlayer instead would make an extra turn CONSUME
    -- the taker's ordinary turn whenever the two are different players -- Time
    -- Warp aimed at an opponent.
    turnAnchor :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)
