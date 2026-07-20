module Pawl.Type.GameState where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.ActivePrevention (ActivePrevention)
import Pawl.Type.Combat (Combat)
import Pawl.Type.ContinuousEffect (ContinuousEffect)
import Pawl.Type.DamageEvent (DamageEvent)
import Pawl.Type.Decider (Decider)
import Pawl.Type.Mana (Mana)
import Pawl.Type.Object (Object)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Phase (Phase)
import Pawl.Type.Player (Player)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Result (Result)
import Pawl.Type.Timestamp (Timestamp)
import Pawl.Type.ZoneChange (ZoneChange)

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
    -- CR 510: combat damage dealt this step, as events, for the SBA to read.
    -- The change-and-emit funnel's log; drained at each SBA check (Sba). See spec §2.
    damageEvents :: [DamageEvent],
    -- CR 603 / 117.5: zone-change events emitted since triggers were last placed.
    -- changeZone appends the RESOLVED (post-replacement) event; the 117.5 boundary
    -- scans and drains it. The zone-change analog of damageEvents.
    zoneChanges :: [ZoneChange],
    -- CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with a duration cleanup consults. Static-ability
    -- effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect],
    -- CR 615.3: floating prevention effects from resolutions (Fog), each with a
    -- duration cleanup consults (CR 514.2). The event-pipeline analog of
    -- continuousEffects. Event.applyPreventions reads it.
    preventions :: [ActivePrevention],
    -- CR 701.19a: one-shot regeneration shields, counted per object (activating
    -- twice stacks two; each destruction consumes one). Keyed by the shielded
    -- object's id -- stable across regeneration (the creature stays on the
    -- battlefield). Cleared at cleanup ("this turn"). Event.destroy reads it.
    regenerationShields :: Map ObjectId Natural,
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
