module Pawl.Types.ActiveAttackProhibition where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 508.1c / 611.1: a stored, resolution-generated ATTACKING RESTRICTION,
-- held in GameState.attackProhibitions. Netter en-Dal's "target creature
-- can't attack this turn" is the producer.
--
-- Read at Pawl.Engine.CombatRestriction.attackProhibited, which unions it into
-- that module's `cantAttack` beside CR 701.35a's detentions -- so CR 508.1c sees
-- one answer and Pawl.Engine.Combat never learns which of the three roads a
-- restriction took. CR 508.1d's maximization ranges over the candidate list
-- `cantAttack` has already narrowed, which is that rule's "without disobeying
-- any restrictions".
--
-- OUTSIDE the layer system, which is CR 613.11: a restriction on a declaration
-- modifies the rules rather than any object's characteristics, so no
-- Pawl.Types.Modification arm could carry it and Pawl.Engine.Projection never
-- sees one.
--
-- A bare ObjectId where the printed carrier (Pawl.Types.ForbidAttack) holds an
-- ObjectRef, for Pawl.Types.ActiveBlockProhibition's reason: the ref is read
-- ONCE, as the ability resolves, and the objects it named are what the
-- restriction covers thereafter.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); "this turn" arms Expiry.AtCleanup.
--
-- `timestamp` is stored for ActiveBlockProhibition's reason: CR 613.11 orders by
-- CR 613.7 timestamp, and nothing observes this one because two prohibitions
-- cannot conflict -- CR 508.1c has no degrees.
--
-- No `controller`, where ActivePlayerEffect stores one: the restriction names no
-- player, so CR 109.5's "you" is never asked of it, and no CR 508.1c "unless"
-- gate, because Pawl.Types.ForbidAttack states none for it to carry.
--
-- Runtime-only: card data writes the printed carrier, never one of these. It
-- does have a codec (Pawl.Codec.ActiveAttackProhibition), because a game in
-- progress has to be writable to JSON (#126).
data ActiveAttackProhibition = MkActiveAttackProhibition
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The permanent that can't attack.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
