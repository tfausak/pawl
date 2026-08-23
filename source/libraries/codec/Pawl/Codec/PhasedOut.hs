module Pawl.Codec.PhasedOut where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PhasedOut as PhasedOut

-- | Three arms over one shared field: CR 702.26a's schedule, CR 702.26g's
-- attached permanent, and CR 702.26n's rescheduled row. The tag is the schedule
-- and the payload is the player, so neither is recoverable from the other.
codec :: Codec.Codec PhasedOut.PhasedOut
codec =
  Arm.tagged
    [ Arm.payload "Directly" PlayerId.codec PhasedOut.Directly (\x -> case x of PhasedOut.Directly y -> Just y; _ -> Nothing),
      Arm.payload "Indirectly" PlayerId.codec PhasedOut.Indirectly (\x -> case x of PhasedOut.Indirectly y -> Just y; _ -> Nothing),
      Arm.payload "Orphaned" PlayerId.codec PhasedOut.Orphaned (\x -> case x of PhasedOut.Orphaned y -> Just y; _ -> Nothing)
    ]
