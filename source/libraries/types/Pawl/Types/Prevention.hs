module Pawl.Types.Prevention where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.13: one prevention effect, applied, having prevented `amount` damage
-- addressed to `recipient`. What Pawl.Engine.Event.resolveDamageBatch
-- answers alongside the surviving damage events, and the whole of what
-- Pawl.Engine.Damage needs to record a CR 615.13 trigger event.
--
-- `by` is the applying instance's CR 614.5 identity, the same key the CR 616.1
-- loop's applied-set is drawn from. It is a GROUPING key rather than a payload:
-- CR 615.13 fires an ability "each time a prevention effect is applied to one or
-- more simultaneous damage events", so several events of one batch prevented by
-- ONE instance are one prevention with the total, and two instances are two.
-- Nothing downstream reads it -- the GameEvent it becomes carries only the
-- recipient and the amount, because no card in the pool asks WHICH prevention
-- prevented the damage ("prevented this way", #687).
--
-- `amount` is the damage this instance stopped, which is the whole point of the
-- type: the CR 616.1 loop's own answer is the SURVIVING event, and a caller
-- holding only that cannot tell a prevented 3 from an event that was never
-- proposed.
data Prevention = MkPrevention
  { by :: CandidateId.CandidateId,
    recipient :: Recipient.Recipient,
    amount :: Natural
  }
  deriving (Eq, Ord, Show)
