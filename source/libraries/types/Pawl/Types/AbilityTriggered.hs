module Pawl.Types.AbilityTriggered where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.2: an ability triggered -- what it hangs on, its controller, and the
-- ability itself.
--
-- `source` and `ability` together are the DISCRIMINATOR
-- Pawl.Types.TriggerEntry carries for the same reason (#61): one object can bear
-- two distinct triggered abilities keyed on the same event, so the source alone
-- does not say which of them triggered, and neither does the trigger condition
-- the ability holds. The ability VALUE rather than an index into the source's
-- abilities, TriggerEntry's haddock giving both reasons.
--
-- `source` is a Pawl.Types.TriggerSource and not an object id, so a SOURCELESS
-- inherent ability the rulebook states without a card (CR 725.2's monarch pair,
-- CR 702.179d's speed increase, CR 728.1's rad counters) gets a record too.
-- `controller` is what tells two players' instances of one such ability apart,
-- there being no object to name.
data AbilityTriggered = MkAbilityTriggered
  { source :: TriggerSource.TriggerSource,
    controller :: PlayerId.PlayerId,
    ability :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
  }
  deriving (Eq, Ord, Show)
