module Pawl.Type.GameState where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Numeric.Natural (Natural)
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
import Pawl.Type.Result (Result)
import Pawl.Type.Timestamp (Timestamp)

data GameState = MkGameState
  { objects :: Map ObjectId Object,
    library :: Map PlayerId (Seq ObjectId),
    hand :: Map PlayerId (Seq ObjectId),
    graveyard :: Map PlayerId (Seq ObjectId),
    battlefield :: Set ObjectId,
    exile :: Set ObjectId,
    stack :: [ObjectId],
    players :: Map PlayerId Player,
    -- CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map PlayerId Mana,
    -- CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat,
    -- CR 608.2i: what happened this turn, in order. Appended by the
    -- change-and-emit funnels (Event.changeZone, Event.createToken,
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
    -- Serpent's Gift), each with a duration cleanup consults. Static-ability
    -- effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect],
    -- CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with a duration
    -- cleanup consults (CR 514.2) and a use count (CR 614.3). The event-pipeline
    -- analog of continuousEffects; a permanent's STATIC replacement abilities are
    -- not here -- the projection re-derives those live. Pawl.Replacement reads it.
    replacements :: [ActiveReplacement],
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
    activeControl :: Maybe Decider
  }
  deriving (Eq, Show)
