module Pawl.Type.GameState where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.ActivePlayerEffect (ActivePlayerEffect)
import Pawl.Type.ActiveReplacement (ActiveReplacement)
import Pawl.Type.Combat (Combat)
import Pawl.Type.ContinuousEffect (ContinuousEffect)
import Pawl.Type.Decider (Decider)
import Pawl.Type.DelayedTrigger (DelayedTrigger)
import Pawl.Type.GameEvent (GameEvent)
import Pawl.Type.Mana (Mana)
import Pawl.Type.Object (Object)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Phase (Phase)
import Pawl.Type.Player (Player)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.RestartSignal (RestartSignal)
import Pawl.Type.Result (Result)
import Pawl.Type.Timestamp (Timestamp)

data GameState = MkGameState
  { objects :: Map ObjectId Object,
    library :: Map PlayerId (Seq ObjectId),
    hand :: Map PlayerId (Seq ObjectId),
    graveyard :: Map PlayerId (Seq ObjectId),
    battlefield :: Set ObjectId,
    exile :: Set ObjectId,
    -- CR 400.1: the command zone -- a shared collection (not per-player), keyed
    -- into `objects` like `battlefield`/`exile`. Emblems live here; their static
    -- abilities are gathered live by the projection (Pawl.Projection.gather).
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
    -- CR 117.5: how far the trigger scan has consumed. Everything at or after
    -- this index is unscanned. Consumption is an index bump; the record stays.
    scannedThrough :: Natural,
    -- CR 704.5h ("since the last state-based action check"): how far the
    -- state-based-action damage read has consumed.
    damageScannedThrough :: Natural,
    -- CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order. Appended by Resolve's ArmDelayedTrigger; an entry is removed as it
    -- fires (CR 603.7b). NOT cleared at turn handoff -- "at the beginning of the
    -- next end step" survives into the next turn if this turn's end step passed
    -- before the ability was armed.
    delayedTriggers :: Seq DelayedTrigger,
    -- CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with an expiry the Pawl.Expiry sweeps consult.
    -- Static-ability effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect],
    -- CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with an expiry the
    -- Pawl.Expiry sweeps consult (CR 514.2) and a use count (CR 614.3). The event-pipeline
    -- analog of continuousEffects; a permanent's STATIC replacement abilities are
    -- not here -- the projection re-derives those live. Pawl.Replacement reads it.
    replacements :: [ActiveReplacement],
    -- CR 611.1 / 613.11: stored PLAYER and RULES-modifying continuous effects
    -- from resolutions (Silence), each with an expiry the Pawl.Expiry sweeps
    -- consult. The third carrier sharing that vocabulary. A permanent's printed
    -- player abilities are NOT here -- Pawl.PlayerEffect re-derives those live.
    playerEffects :: [ActivePlayerEffect],
    -- The SEATING order (CR 800.5, CR 806.3), rotated so the starting player is
    -- first (CR 103.1: "The game's default turn order begins with the starting
    -- player and proceeds clockwise"). It lists every player who BEGAN this game
    -- and is never shortened. Who is still IN the game is Departure.stillPlaying,
    -- and every departure-aware read filters through that on top of this.
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
    -- monarch", keyed by the exiled incarnation id to the effect's controller
    -- (whose opponent taking the crown ends the exile). Not an Expiry: the Expiry
    -- sweeps are delete-and-recompute and cannot perform the return zone change.
    exiledUntilMonarch :: Map ObjectId PlayerId
  }
  deriving (Eq, Show)
