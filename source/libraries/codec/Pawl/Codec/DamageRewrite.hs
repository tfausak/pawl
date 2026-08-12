module Pawl.Codec.DamageRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamageRewrite as DamageRewrite

toJson :: DamageRewrite.DamageRewrite -> Value.Value
toJson r = case r of
  DamageRewrite.PreventAll -> Common.nullary "PreventAll"
  DamageRewrite.PreventRemovingShieldCounter -> Common.nullary "PreventRemovingShieldCounter"
  DamageRewrite.PreventNext n -> Common.tagged "PreventNext" . Just $ Common.encodeNatural n
  DamageRewrite.SetAmount n -> Common.tagged "SetAmount" . Just $ Common.encodeNatural n
  DamageRewrite.Scale s -> Common.tagged "Scale" . Just $ Codec.encode Scaling.codec s
  DamageRewrite.Redirect recipient -> Common.tagged "Redirect" . Just $ Recipient.toJson recipient

fromJson :: Value.Value -> Either Text.Text DamageRewrite.DamageRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("PreventAll", Nothing) -> pure DamageRewrite.PreventAll
    ("PreventRemovingShieldCounter", Nothing) -> pure DamageRewrite.PreventRemovingShieldCounter
    ("PreventNext", Just v) -> DamageRewrite.PreventNext <$> Common.decodeNatural v
    ("SetAmount", Just v) -> DamageRewrite.SetAmount <$> Common.decodeNatural v
    ("Scale", Just v) -> DamageRewrite.Scale <$> Codec.decode Scaling.codec v
    ("Redirect", Just v) -> DamageRewrite.Redirect <$> Recipient.fromJson v
    _ -> Left . Text.pack $ "unknown DamageRewrite: " <> t
