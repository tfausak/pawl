module Pawl.Types.ActiveBlockProhibition where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 509.1b / 611.1: a stored, resolution-generated BLOCKING RESTRICTION, held
-- in GameState.blockProhibitions. Zirda, the Dawnwaker's "target creature can't
-- block this turn" is the producer.
--
-- Read at Pawl.Engine.CombatRestriction.blockProhibited, which unions it into
-- `cantBlock` beside CR 701.35a's detentions -- so CR 509.1b sees one answer and
-- Pawl.Engine.Combat never learns which of the three roads a restriction took.
--
-- OUTSIDE the layer system, which is CR 613.11: a restriction on a declaration
-- modifies the rules rather than any object's characteristics, so no
-- Pawl.Types.Modification arm could carry it and Pawl.Engine.Projection never
-- sees one.
--
-- A bare ObjectId where the printed carrier (Pawl.Types.ForbidBlock) holds an
-- ObjectRef, for Pawl.Types.ActiveBlockRequirement's reason: the ref is read
-- ONCE, as the ability resolves, and the objects it named are what the
-- restriction covers thereafter.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); "this turn" arms Expiry.AtCleanup.
--
-- `timestamp` is stored for ActiveBlockRequirement's reason: CR 613.11 orders by
-- CR 613.7 timestamp, and nothing observes this one because two prohibitions
-- cannot conflict -- CR 509.1b has no degrees.
--
-- No `controller`, where ActivePlayerEffect stores one: the restriction names no
-- player, so CR 109.5's "you" is never asked of it, and no CR 509.1b "unless"
-- gate, because Pawl.Types.ForbidBlock states none for it to carry.
--
-- Runtime-only: card data writes the printed carrier, never one of these. It
-- does have a codec (Pawl.Codec.ActiveBlockProhibition), because a game in
-- progress has to be writable to JSON (#126).
data ActiveBlockProhibition = MkActiveBlockProhibition
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The permanent that can't block.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
