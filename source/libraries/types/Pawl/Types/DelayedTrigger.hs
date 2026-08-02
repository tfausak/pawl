module Pawl.Types.DelayedTrigger where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.7: a delayed triggered ability that has been created and is waiting for
-- its trigger event. A concrete `TriggeredAbility Card`, exactly as
-- Source.OfTrigger already carries one.
--
-- `controller` is the player who controlled the SPELL OR ABILITY that created it,
-- as that spell or ability RESOLVED (CR 603.7d-f) -- baked in at arming, never
-- re-derived. `bindings` is the environment captured at that moment, which is how
-- "it" and "that card" (CR 603.7c) survive the resolution that armed the ability.
--
-- `expiry` is CR 603.7b's stated duration, as the game remembers it: "A delayed
-- triggered ability will trigger only once -- the next time its trigger event
-- occurs -- unless it has a stated duration, such as 'this turn.'" Nothing is
-- that rule's default, and an entry carrying it is removed as it fires. Just an
-- expiry keeps the entry armed through firing, and one of Pawl.Engine.Expiry's sweeps
-- is what eventually ends it -- CR 514.2's cleanup, for Full Throttle's "this
-- turn".
--
-- Nothing rather than an Expiry arm meaning "once", because once-ness is not a
-- duration: the rule words it as the ABSENCE of one, and an entry with no
-- duration must survive every time-based sweep -- its one shot is spent by
-- firing, never by the clock.
--
-- An Expiry and not a Duration, for the reason that type's own haddock gives:
-- card data says "this turn", and the game remembers whose turn and when. It is
-- armed by Pawl.Engine.Expiry.arm exactly as a continuous effect's is.
data DelayedTrigger = MkDelayedTrigger
  { ability :: TriggeredAbility.TriggeredAbility Card.Card,
    source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | Pawl.Types.Onset as the game remembers it: the earliest
    -- GameState.turnNumber at which this entry may fire. Nothing is the ordinary
    -- case -- an ability watches for its event from the moment it is created,
    -- which is CR 603.7a's floor and all those abilities ask. Just n is
    -- Onset.FromYourNextTurn resolved against the board: the turn AFTER the one
    -- the arming resolution happened on, which
    -- Pawl.Engine.Event.delayedPending compares the live turn number against.
    --
    -- A turn NUMBER and not a latch cleared at the handoff, because the number is
    -- already kept (GameState.turnNumber, bumped in Engine.beginTurnOf for every
    -- turn that begins, extra turns included) and a comparison cannot go stale.
    --
    -- A number carries no player, and that is a real limit rather than a
    -- shorthand: this field answers only "not the turn it was armed on", and
    -- WHOSE turn the entry may fire on is the ability's own condition's question
    -- (TurnScope.ControllersTurn). The two are only jointly "your next turn",
    -- which is why the pairing is lint-enforced rather than assumed -- see
    -- Pawl.Types.Onset.FromYourNextTurn.
    --
    -- Not implemented: a turn whose declare attackers step is skipped
    -- (Stonehorn Dignitary) leaves the entry armed for a LATER turn, where the
    -- printed "your next turn" named one particular turn and the event can never
    -- occur again (#507).
    notBefore :: Maybe Natural.Natural,
    expiry :: Maybe Expiry.Expiry
  }
  deriving (Eq, Show)
