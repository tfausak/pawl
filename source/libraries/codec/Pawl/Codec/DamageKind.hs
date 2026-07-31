-- | The @DamageKind ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.DamageKind where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DamageKind as DamageKind

damageKindToJson :: DamageKind.DamageKind -> Value
damageKindToJson k = Json.nullary . Text.pack $ case k of
  DamageKind.Combat -> "Combat"
  DamageKind.Noncombat -> "Noncombat"

jsonToDamageKind :: Value -> Either Text DamageKind.DamageKind
jsonToDamageKind =
  Json.decodeNullary
    (Text.pack "DamageKind")
    [ (Text.pack "Combat", DamageKind.Combat),
      (Text.pack "Noncombat", DamageKind.Noncombat)
    ]
