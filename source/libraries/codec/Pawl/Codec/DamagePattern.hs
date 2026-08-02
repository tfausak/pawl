module Pawl.Codec.DamagePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.SourceRelation as SourceRelation
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DamagePattern as DamagePattern

toJson :: DamagePattern.DamagePattern -> Value.Value
toJson p =
  Common.object
    [ Common.pair "whichKind" . Common.encodeMaybe DamageKind.toJson $ DamagePattern.whichKind p,
      Common.pair "whichSource" . SourceRelation.toJson $ DamagePattern.whichSource p
    ]

fromJson :: Value.Value -> Either Text.Text DamagePattern.DamagePattern
fromJson value = do
  ps <- Common.asObject value
  k <- Common.field "whichKind" ps >>= Common.decodeMaybe DamageKind.fromJson
  src <- Common.field "whichSource" ps >>= SourceRelation.fromJson
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k, DamagePattern.whichSource = src}
