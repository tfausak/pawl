module Pawl.Codec.Optionality where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Optionality as Optionality

toJson :: Optionality.Optionality -> Value.Value
toJson o = Common.nullary $ case o of
  Optionality.Mandatory -> "Mandatory"
  Optionality.Optional -> "Optional"

fromJson :: Value.Value -> Either Text.Text Optionality.Optionality
fromJson =
  Common.decodeNullary
    "Optionality"
    [ ("Mandatory", Optionality.Mandatory),
      ("Optional", Optionality.Optional)
    ]
