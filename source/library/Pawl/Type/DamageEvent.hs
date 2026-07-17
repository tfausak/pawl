module Pawl.Type.DamageEvent where

import Numeric.Natural (Natural)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)

-- One instance of combat damage: a source dealt `amount` to `target`. The first
-- reader is deathtouch's CR 704.5h SBA, which asks whether `source` has deathtouch
-- (via the projection, at check time). Minimal by design (CR 702.2 needs only
-- source + creature target + nonzero amount); lifelink and M4 combat-damage
-- triggers grow the payload rather than reshape it. See the M2c spec, section 2.
data DamageEvent = MkDamageEvent
  { source :: ObjectId,
    target :: Recipient,
    amount :: Natural
  }
  deriving (Eq, Ord, Show)
