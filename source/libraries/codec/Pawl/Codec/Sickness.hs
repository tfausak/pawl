module Pawl.Codec.Sickness where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Sickness as Sickness

-- | Settled carries WHO the CR 302.6 continuity claim is about, so the seat is
-- part of the wire form rather than a flag.
codec :: Codec.Codec Sickness.Sickness
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Sick" Sickness.Sick,
      Arm.payload "Settled" PlayerId.codec Sickness.Settled (\x -> case x of Sickness.Settled y -> Just y; _ -> Nothing)
    ]

tagOf :: Sickness.Sickness -> String
tagOf x = case x of
  Sickness.Sick {} -> "Sick"
  Sickness.Settled {} -> "Settled"
