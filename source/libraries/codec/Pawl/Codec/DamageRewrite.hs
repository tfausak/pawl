-- | The @DamageRewrite ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.DamageRewrite where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DamageRewrite as DamageRewrite

damageRewriteToJson :: DamageRewrite.DamageRewrite -> Value
damageRewriteToJson r = Json.nullary . Text.pack $ case r of
  DamageRewrite.PreventAll -> "PreventAll"

jsonToDamageRewrite :: Value -> Either Text DamageRewrite.DamageRewrite
jsonToDamageRewrite =
  Json.decodeNullary (Text.pack "DamageRewrite") [(Text.pack "PreventAll", DamageRewrite.PreventAll)]
