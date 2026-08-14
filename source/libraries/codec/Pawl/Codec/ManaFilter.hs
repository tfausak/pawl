module Pawl.Codec.ManaFilter where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaFilter as ManaFilter

codec :: Codec.Codec ManaFilter.ManaFilter
codec =
  Arm.tagged
    [ Arm.nullary "Any" ManaFilter.Any,
      Arm.payload "OfType" ManaType.codec ManaFilter.OfType (\x -> case x of ManaFilter.OfType y -> Just y; _ -> Nothing)
    ]
