module Pawl.Codec.TapState where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TapState as TapState

codec :: Codec.Codec TapState.TapState
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Untapped" TapState.Untapped,
      Arm.nullary "Tapped" TapState.Tapped
    ]
  where
    encode t = Common.nullary $ case t of
      TapState.Untapped -> "Untapped"
      TapState.Tapped -> "Tapped"
