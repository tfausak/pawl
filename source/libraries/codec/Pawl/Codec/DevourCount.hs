module Pawl.Codec.DevourCount where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DevourCount as DevourCount

-- | Tagged rather than a bare number, because CR 702.82b's restated N (Thromok
-- the Insatiable) carries no number at all and a bare number could not say so.
codec :: Codec.Codec DevourCount.DevourCount
codec =
  Arm.tagged
    [ Arm.payload "Fixed" Common.natural DevourCount.Fixed (\x -> case x of DevourCount.Fixed y -> Just y; _ -> Nothing),
      Arm.nullary "Devoured" DevourCount.Devoured
    ]
