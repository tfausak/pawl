module Pawl.Codec.ExileLooker where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ExileLooker as ExileLooker

-- | Tagged rather than an enum: CR 406.3's grant names a seat and CR 702.75a's
-- names none.
codec :: Codec.Codec ExileLooker.ExileLooker
codec =
  Arm.tagged
    [ Arm.payload "ThePlayer" PlayerId.codec ExileLooker.ThePlayer (\x -> case x of ExileLooker.ThePlayer y -> Just y; _ -> Nothing),
      Arm.nullary "TheExiler" ExileLooker.TheExiler
    ]
