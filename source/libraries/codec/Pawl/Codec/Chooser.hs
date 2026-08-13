module Pawl.Codec.Chooser where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Chooser as Chooser

-- | Tagged rather than nullary-only, Pawl.Codec.Pool's shape and for its reason:
-- one arm names a slot and so carries a payload, while Common.nullary IS
-- Common.tagged with no value, so the other two are unaffected.
codec :: Codec.Codec Chooser.Chooser
codec =
  Arm.tagged
    encode
    [ Arm.nullary "TheController" Chooser.TheController,
      Arm.nullary "EachInScope" Chooser.EachInScope,
      Arm.payload "BoundInSlot" SlotName.codec Chooser.BoundInSlot
    ]
  where
    encode c = case c of
      Chooser.TheController -> Common.nullary "TheController"
      Chooser.EachInScope -> Common.nullary "EachInScope"
      Chooser.BoundInSlot slot -> Common.tagged "BoundInSlot" . Just $ Codec.encode SlotName.codec slot
