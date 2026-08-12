module Pawl.Codec.Counterability where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Counterability as Counterability

codec :: Codec.Codec Counterability.Counterability
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Counterable" Counterability.Counterable,
      Arm.nullary "CantBeCountered" Counterability.CantBeCountered
    ]
  where
    encode c = Common.nullary $ case c of
      Counterability.Counterable -> "Counterable"
      Counterability.CantBeCountered -> "CantBeCountered"
