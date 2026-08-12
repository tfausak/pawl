module Pawl.Codec.SourceRelation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SourceRelation as SourceRelation

codec :: Codec.Codec SourceRelation.SourceRelation
codec =
  Arm.tagged
    encode
    [ Arm.nullary "AnySource" SourceRelation.AnySource,
      Arm.nullary "TheSource" SourceRelation.TheSource
    ]
  where
    encode r = Common.nullary $ case r of
      SourceRelation.AnySource -> "AnySource"
      SourceRelation.TheSource -> "TheSource"
