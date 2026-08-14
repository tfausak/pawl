module Pawl.Codec.DamageRewrite where

import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamageRewrite as DamageRewrite

-- | The wire format is unchanged by the conversion to a bundle.
codec :: Codec.Codec DamageRewrite.DamageRewrite
codec =
  Arm.tagged
    encode
    [ Arm.nullary "PreventAll" DamageRewrite.PreventAll,
      Arm.nullary "PreventRemovingShieldCounter" DamageRewrite.PreventRemovingShieldCounter,
      Arm.payload "PreventNext" Common.natural DamageRewrite.PreventNext,
      Arm.payload "SetAmount" Common.natural DamageRewrite.SetAmount,
      Arm.payload "Scale" Scaling.codec DamageRewrite.Scale,
      Arm.payload "Redirect" Recipient.codec DamageRewrite.Redirect
    ]
  where
    encode r = case r of
      DamageRewrite.PreventAll -> Common.nullary "PreventAll"
      DamageRewrite.PreventRemovingShieldCounter -> Common.nullary "PreventRemovingShieldCounter"
      DamageRewrite.PreventNext n -> Common.tagged "PreventNext" . Just $ Codec.encode Common.natural n
      DamageRewrite.SetAmount n -> Common.tagged "SetAmount" . Just $ Codec.encode Common.natural n
      DamageRewrite.Scale s -> Common.tagged "Scale" . Just $ Codec.encode Scaling.codec s
      DamageRewrite.Redirect recipient -> Common.tagged "Redirect" . Just $ Codec.encode Recipient.codec recipient
