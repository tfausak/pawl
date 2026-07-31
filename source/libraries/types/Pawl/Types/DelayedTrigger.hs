module Pawl.Types.DelayedTrigger where

import Data.Map.Strict (Map)
import Pawl.Types.Binding (Binding)
import Pawl.Types.Card (Card)
import Pawl.Types.Expiry (Expiry)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TriggeredAbility (TriggeredAbility)

-- CR 603.7: a delayed triggered ability that has been created and is waiting for
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
  { ability :: TriggeredAbility Card,
    source :: ObjectId,
    controller :: PlayerId,
    bindings :: Map SlotName Binding,
    expiry :: Maybe Expiry
  }
  deriving (Eq, Show)
