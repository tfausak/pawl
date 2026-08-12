module Pawl.Codec.ManaType where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaType as ManaType

codec :: Codec.Codec ManaType.ManaType
codec =
  Arm.tagged
    encode
    [ Arm.payload "Colored" Color.codec ManaType.Colored,
      Arm.nullary "Colorless" ManaType.Colorless
    ]
  where
    encode mt = case mt of
      ManaType.Colored c -> Common.tagged "Colored" . Just $ Codec.encode Color.codec c
      ManaType.Colorless -> Common.nullary "Colorless"
