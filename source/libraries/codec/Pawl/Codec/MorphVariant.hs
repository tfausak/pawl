module Pawl.Codec.MorphVariant where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.MorphVariant as MorphVariant

toJson :: MorphVariant.MorphVariant -> Value.Value
toJson v = Common.nullary $ case v of
  MorphVariant.Plain -> "Plain"
  MorphVariant.Mega -> "Mega"

fromJson :: Value.Value -> Either Text.Text MorphVariant.MorphVariant
fromJson =
  Common.decodeNullary
    "MorphVariant"
    [ ("Plain", MorphVariant.Plain),
      ("Mega", MorphVariant.Mega)
    ]
