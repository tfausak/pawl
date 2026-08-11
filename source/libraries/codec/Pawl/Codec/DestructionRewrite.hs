module Pawl.Codec.DestructionRewrite where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

toJson :: DestructionRewrite.DestructionRewrite -> Value.Value
toJson r = Common.nullary $ case r of
  DestructionRewrite.Regenerate -> "Regenerate"
  DestructionRewrite.RemoveShieldCounter -> "RemoveShieldCounter"

fromJson :: Value.Value -> Either Text.Text DestructionRewrite.DestructionRewrite
fromJson =
  Common.decodeNullary
    "DestructionRewrite"
    [ ("Regenerate", DestructionRewrite.Regenerate),
      ("RemoveShieldCounter", DestructionRewrite.RemoveShieldCounter)
    ]
