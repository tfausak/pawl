module Pawl.Codec.ManaFilter where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaFilter as ManaFilter

codec :: Codec.Codec ManaFilter.ManaFilter
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Any" ManaFilter.Any,
      Arm.payload "OfType" ManaType.codec ManaFilter.OfType
    ]
  where
    encode f = case f of
      ManaFilter.Any -> Common.nullary "Any"
      ManaFilter.OfType mt -> Common.tagged "OfType" . Just $ Codec.encode ManaType.codec mt
