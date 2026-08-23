module Pawl.Codec.Status where

import qualified Pawl.Codec.Departure as Departure
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Status as Status

codec :: Codec.Codec Status.Status
codec =
  Arm.tagged
    [ Arm.nullary "Playing" Status.Playing,
      Arm.payload "Departed" Departure.codec Status.Departed (\x -> case x of Status.Departed y -> Just y; _ -> Nothing)
    ]
