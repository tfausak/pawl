module Pawl.Codec.DamageKind where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamageKind as DamageKind

toJson :: DamageKind.DamageKind -> Value.Value
toJson k = Common.nullary $ case k of
  DamageKind.Combat -> "Combat"
  DamageKind.Noncombat -> "Noncombat"

fromJson :: Value.Value -> Either Text.Text DamageKind.DamageKind
fromJson =
  Common.decodeNullary
    "DamageKind"
    [ ("Combat", DamageKind.Combat),
      ("Noncombat", DamageKind.Noncombat)
    ]
