-- | The @DamagePattern ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.DamagePattern where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.DamageKind (damageKindToJson, jsonToDamageKind)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DamagePattern as DamagePattern

damagePatternToJson :: DamagePattern.DamagePattern -> Value
damagePatternToJson p =
  Json.jObject [(Text.pack "whichKind", Json.maybeTo damageKindToJson (DamagePattern.whichKind p))]

jsonToDamagePattern :: Value -> Either Text DamagePattern.DamagePattern
jsonToDamagePattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= Json.maybeFrom jsonToDamageKind
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k}
