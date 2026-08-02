module Pawl.Codec.DamageRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DamageRewrite as DamageRewrite

toJson :: DamageRewrite.DamageRewrite -> Value.Value
toJson r = Common.nullary $ case r of
  DamageRewrite.PreventAll -> "PreventAll"

fromJson :: Value.Value -> Either Text.Text DamageRewrite.DamageRewrite
fromJson = Common.decodeNullary "DamageRewrite" [("PreventAll", DamageRewrite.PreventAll)]
