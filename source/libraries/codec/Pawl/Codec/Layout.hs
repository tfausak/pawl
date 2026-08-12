module Pawl.Codec.Layout where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Layout as Layout

codec :: Codec.Codec Layout.Layout
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Normal" Layout.Normal,
      Arm.nullary "Split" Layout.Split,
      Arm.nullary "Room" Layout.Room,
      Arm.nullary "Adventure" Layout.Adventure,
      Arm.nullary "Transforming" Layout.Transforming,
      Arm.nullary "ModalDoubleFaced" Layout.ModalDoubleFaced
    ]
  where
    encode l = Common.nullary $ case l of
      Layout.Normal -> "Normal"
      Layout.Split -> "Split"
      Layout.Room -> "Room"
      Layout.Adventure -> "Adventure"
      Layout.Transforming -> "Transforming"
      Layout.ModalDoubleFaced -> "ModalDoubleFaced"
