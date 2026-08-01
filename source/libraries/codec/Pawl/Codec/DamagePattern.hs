-- | The @DamagePattern ⇆ Json@ codec (#481).
module Pawl.Codec.DamagePattern where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DamagePattern as DamagePattern

damagePatternToJson :: DamagePattern.DamagePattern -> Value
damagePatternToJson p =
  Json.jObject [(Text.pack "whichKind", Json.maybeTo DamageKind.toJson (DamagePattern.whichKind p))]

jsonToDamagePattern :: Value -> Either Text DamagePattern.DamagePattern
jsonToDamagePattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= Json.maybeFrom DamageKind.fromJson
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k}
