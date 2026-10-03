module Pawl.Types.ActiveEvasion where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 509.1b / 611.2c: a stored, resolution-generated restriction that the
-- attackers 'affected' describes can't be blocked, held in GameState.evasions.
-- Veiling Oddity's "creatures can't be blocked this turn" is the producer.
--
-- Read at Pawl.Engine.CombatRestriction.storedEvasions, which `barredBlocks`
-- unions beside the printed Pawl.Types.CantBeBlockedBy rows, so
-- Pawl.Engine.Combat never learns which road a restriction took. 'affected' is
-- re-read against the live board at each declaration, as
-- Pawl.Types.ActiveAttackProhibition's Matching arm is.
--
-- OUTSIDE the layer system (CR 613.11), and `controller` is STORED, both for
-- ActiveAttackProhibition's reasons. `timestamp` is stored for
-- ActiveBlockProhibition's reason, and nothing observes it.
--
-- Runtime-only: card data writes Pawl.Types.ForbidBeingBlocked, never one of
-- these. It has a codec (Pawl.Codec.ActiveEvasion) because a game in progress
-- has to be writable to JSON (#126).
data ActiveEvasion = MkActiveEvasion
  { source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    affected :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
