module Pawl.Type.ProposedEvent where

import Numeric.Natural (Natural)
import Pawl.Type.Card (Card)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.DamageEvent (DamageEvent)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.ZoneChange (ZoneChange)

-- CR 614.6: an event as it WOULD happen -- the thing a replacement effect
-- rewrites. Deliberately distinct from Pawl.Type.GameEvent: a GameEvent is
-- HISTORY (it carries a CR 608.2h last-known-information snapshot and exists only
-- after the fact), while a ProposedEvent exists only while it is being replaced,
-- and the one that survives the CR 616.1 loop is the one that actually happens.
--
-- WouldEnter is raised only for BATTLEFIELD entries (CR 614.1c-d apply nowhere
-- else) and is NESTED inside whatever caused the entry -- CR 616.1g's containment
-- ("one effect may apply to an event, and another to an event contained within
-- the first"), expressed as call nesting rather than as a field.
--
-- Six arms, not the ~40 replaceable event classes the rules define: each of the
-- rest is one more arm plus the funnel that raises it -- vocabulary on a finished
-- axis, which is what "the closed half can genuinely be finished" means here.
data ProposedEvent
  = WouldChangeZone ZoneChange
  | WouldEnter ObjectId
  | WouldDealDamage DamageEvent
  | WouldBeDestroyed ObjectId
  | WouldPutCounters ObjectId CounterKind Natural
  | WouldCreateTokens PlayerId Card Natural
  deriving (Eq, Show)
