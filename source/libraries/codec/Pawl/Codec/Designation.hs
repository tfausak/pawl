module Pawl.Codec.Designation where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Designation as Designation

toJson :: Designation.Designation -> Value.Value
toJson d = Common.nullary $ case d of
  Designation.Renowned -> "Renowned"
  Designation.Monstrous -> "Monstrous"
  Designation.Suspected -> "Suspected"

fromJson :: Value.Value -> Either Text.Text Designation.Designation
fromJson =
  Common.decodeNullary
    "Designation"
    [ ("Renowned", Designation.Renowned),
      ("Monstrous", Designation.Monstrous),
      ("Suspected", Designation.Suspected)
    ]
