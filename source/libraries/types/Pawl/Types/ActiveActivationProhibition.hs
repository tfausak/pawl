module Pawl.Types.ActiveActivationProhibition where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 602.2 / 611.1: a stored, resolution-generated ACTIVATION PROHIBITION,
-- held in GameState.activationProhibitions. Deadlock Trap's "its activated
-- abilities can't be activated this turn" is the producer.
--
-- Read at Pawl.Engine.ActivationProhibition.cantActivate, which unions it into
-- that module's answer beside the printed carrier's rows -- so CR 602.2's window
-- and CR 605.3a's see one answer and neither gate learns which road a
-- prohibition took.
--
-- OUTSIDE the layer system, which is CR 613.11: a prohibition on an activation
-- modifies the rules rather than any object's characteristics, so no
-- Pawl.Types.Modification arm could carry it and Pawl.Engine.Projection never
-- sees one.
--
-- A bare ObjectId where the printed carrier (Pawl.Types.ActivationProhibition)
-- holds an Affected, for Pawl.Types.ActiveBlockProhibition's reason: the ref is
-- read ONCE, as the ability resolves, and the objects it named are what the
-- prohibition covers thereafter.
--
-- CR 400.7 is answered by the id itself: Pawl.Engine.Event's placeObject mints a
-- fresh ObjectId on every zone change, so a permanent bounced and replayed this
-- turn is an object this row never named and the row merely dangles until its
-- expiry sweeps it. Pawl.ActivationProhibitionSpec's "a Troll bounced by
-- Unsummon and replayed the same turn is no longer prohibited" proves it, and
-- the two combat carriers are right for the same reason.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); "this turn" arms Expiry.AtCleanup and "until your next turn"
-- Expiry.AtTurnOf.
--
-- `timestamp` is stored for ActiveBlockProhibition's reason: CR 613.11 orders by
-- CR 613.7 timestamp, and nothing observes this one because two prohibitions
-- cannot conflict -- CR 602.2 has no degrees.
--
-- No `controller`, where ActivePlayerEffect stores one: the prohibition names no
-- player, so CR 109.5's "you" is never asked of it. No CR 605.1a kind, for
-- Pawl.Types.ForbidActivation's reason, so every activated ability of `object`
-- is refused. No CR 116.2d `name`, where the printed carrier holds one: a name
-- is what a face's own SpecialAction.IgnoreThisUntilEndOfTurn refers to, and a
-- row a resolution stored has outlived the ability that made it, so
-- Pawl.Engine.IgnoredAbility is never asked about one.
--
-- Runtime-only: card data writes the printed carrier, never one of these. It
-- does have a codec (Pawl.Codec.ActiveActivationProhibition), because a game in
-- progress has to be writable to JSON (#126).
data ActiveActivationProhibition = MkActiveActivationProhibition
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The permanent whose activated abilities can't be activated.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
