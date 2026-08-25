module Pawl.Types.Prevention where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.13: one prevention effect, applied, having prevented `amount` damage
-- addressed to `recipient`. What Pawl.Engine.Event.resolveDamageBatch
-- answers alongside the surviving damage events, and the whole of what
-- Pawl.Engine.Damage needs to record a CR 615.13 trigger event.
--
-- `by` is the applying instance's CR 614.5 identity, the same key the CR 616.1
-- loop's applied-set is drawn from. It is both a GROUPING key and a payload: CR
-- 615.13 fires an ability "each time a prevention effect is applied to one or
-- more simultaneous damage events", so several events of one batch prevented by
-- ONE instance are one prevention with the total, and two instances are two --
-- and the same identity rides the GameEvent it becomes, which is what a card
-- printing "prevented this way" compares against (Phyrexian Vindicator; see
-- Pawl.Types.DamagePrevented).
--
-- `amount` is the damage this instance stopped, which is the whole point of the
-- type: the CR 616.1 loop's own answer is the SURVIVING event, and a caller
-- holding only that cannot tell a prevented 3 from an event that was never
-- proposed.
--
-- `rider` is CR 615.5's additional effect, carried off the applying row so it
-- outlives it -- a CR 615.7 shield spent to 0 is dropped in the very application
-- that fires the rider. Nothing for every prevention but one a card wrote a
-- rider onto. Opaque here and everywhere below: Pawl.Engine.Damage queues it
-- without looking inside, and Pawl.Engine.Resolve is the one module that runs
-- it.
data Prevention = MkPrevention
  { by :: CandidateId.CandidateId,
    recipient :: Recipient.Recipient,
    amount :: Natural,
    rider :: Maybe PreventionRider.PreventionRider
  }
  deriving (Eq, Ord, Show)
