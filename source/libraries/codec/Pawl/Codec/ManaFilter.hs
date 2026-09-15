module Pawl.Codec.ManaFilter where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaFilter as ManaFilter

codec :: Codec.Codec ManaFilter.ManaFilter
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Any" ManaFilter.Any,
      Arm.payload "OfType" ManaType.codec ManaFilter.OfType (\x -> case x of ManaFilter.OfType y -> Just y; _ -> Nothing),
      Arm.payload "NotOfType" ManaType.codec ManaFilter.NotOfType (\x -> case x of ManaFilter.NotOfType y -> Just y; _ -> Nothing)
    ]

tagOf :: ManaFilter.ManaFilter -> String
tagOf x = case x of
  ManaFilter.Any {} -> "Any"
  ManaFilter.OfType {} -> "OfType"
  ManaFilter.NotOfType {} -> "NotOfType"
