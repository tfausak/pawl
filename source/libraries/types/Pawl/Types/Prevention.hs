module Pawl.Types.Prevention where

import qualified Data.Map as Map
import Numeric.Natural (Natural)
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.13: one prevention effect, applied, having prevented `amounts` of
-- damage across the recipients that damage was addressed to. What
-- Pawl.Engine.Event.resolveDamageBatch answers alongside the surviving damage
-- events, and the whole of what Pawl.Engine.Damage needs to record a CR 615.13
-- trigger event.
--
-- `by` is the applying instance's CR 614.5 identity, the same key the CR 616.1
-- loop's applied-set is drawn from. It is both the GROUPING key and a payload: CR
-- 615.13 fires an ability "each time a prevention effect is applied to one or
-- more simultaneous damage events", so every event of one batch prevented by
-- ONE instance is one prevention, and two instances are two -- and the same
-- identity rides the GameEvent it becomes, which is what a card printing
-- "prevented this way" compares against (Phyrexian Vindicator; see
-- Pawl.Types.DamagePrevented).
--
-- `amounts` is what this instance stopped, PER SOURCE and, within a source, PER
-- RECIPIENT. CR 615.13 counts one application however many simultaneous events
-- and sources it covered, so the sources are the outer KEYS rather than part of
-- the grouping key (see Pawl.Engine.Replacement.groupPreventions): Comeuppance
-- stopping two attackers at once is one prevention with two sources. A source's
-- inner sum is CR 615.5's "that much" for that source, since the rider runs once
-- per source (Pawl.Engine.Resolve.Effect.runPreventionRider); the inner map keeps
-- the reading a trigger scoped to one recipient needs ("damage that would be
-- dealt to YOU", Selfless Squire). The CR 120.1 source and the recipient are both
-- read off the event as PROPOSED: rule 615.13 watches "damage that WOULD be dealt
-- [and] is prevented", so the event as offered is the one it describes, and the
-- CR 616.1 loop's own answer is the SURVIVING event, from which a caller cannot
-- tell a prevented 3 from an event that was never proposed. An entry may be 0:
-- CR 615.12's inert application is on this map at 0 so that its rider still
-- queues.
--
-- `rider` is CR 615.5's additional effect, carried off the applying row so it
-- outlives it -- a CR 615.7 shield spent to 0 is dropped in the very application
-- that fires the rider. Nothing for every prevention but one a card wrote a
-- rider onto. Opaque here and everywhere below: Pawl.Engine.Damage queues it
-- without looking inside, and Pawl.Engine.Resolve is the one module that runs
-- it.
data Prevention = MkPrevention
  { by :: CandidateId.CandidateId,
    amounts :: Map.Map ObjectId.ObjectId (Map.Map Recipient.Recipient Natural),
    rider :: Maybe PreventionRider.PreventionRider
  }
  deriving (Eq, Ord, Show)
