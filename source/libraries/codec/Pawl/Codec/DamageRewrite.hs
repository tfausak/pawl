module Pawl.Codec.DamageRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DamageRewrite as DamageRewrite

toJson :: DamageRewrite.DamageRewrite -> Value.Value
toJson r = case r of
  DamageRewrite.PreventAll -> Common.nullary "PreventAll"
  DamageRewrite.SetAmount n -> Common.tagged "SetAmount" . Just $ Common.encodeNatural n
  DamageRewrite.Scale s -> Common.tagged "Scale" . Just $ Scaling.toJson s

fromJson :: Value.Value -> Either Text.Text DamageRewrite.DamageRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("PreventAll", Nothing) -> pure DamageRewrite.PreventAll
    ("SetAmount", Just v) -> DamageRewrite.SetAmount <$> Common.decodeNatural v
    ("Scale", Just v) -> DamageRewrite.Scale <$> Scaling.fromJson v
    _ -> Left . Text.pack $ "unknown DamageRewrite: " <> t
