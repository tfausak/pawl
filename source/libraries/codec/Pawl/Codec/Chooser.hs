module Pawl.Codec.Chooser where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Chooser as Chooser

codec :: Codec.Codec Chooser.Chooser
codec =
  Arm.tagged
    encode
    [ Arm.nullary "TheController" Chooser.TheController,
      Arm.nullary "EachInScope" Chooser.EachInScope
    ]
  where
    encode c = Common.nullary $ case c of
      Chooser.TheController -> "TheController"
      Chooser.EachInScope -> "EachInScope"
