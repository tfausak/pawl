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
    [ Arm.nullary "PreventAll" DamageRewrite.PreventAll,
      Arm.nullary "PreventRemovingShieldCounter" DamageRewrite.PreventRemovingShieldCounter,
      Arm.payload "PreventNext" Common.natural DamageRewrite.PreventNext (\x -> case x of DamageRewrite.PreventNext y -> Just y; _ -> Nothing),
      Arm.payload "SetAmount" Common.natural DamageRewrite.SetAmount (\x -> case x of DamageRewrite.SetAmount y -> Just y; _ -> Nothing),
      Arm.payload "Scale" Scaling.codec DamageRewrite.Scale (\x -> case x of DamageRewrite.Scale y -> Just y; _ -> Nothing),
      Arm.payload "Redirect" Recipient.codec DamageRewrite.Redirect (\x -> case x of DamageRewrite.Redirect y -> Just y; _ -> Nothing)
    ]
