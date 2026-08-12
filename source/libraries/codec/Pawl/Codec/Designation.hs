module Pawl.Codec.Designation where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Designation as Designation

codec :: Codec.Codec Designation.Designation
codec =
  Arm.tagged
    toJson
    [ Arm.nullary "Renowned" Designation.Renowned,
      Arm.nullary "Monstrous" Designation.Monstrous,
      Arm.nullary "Suspected" Designation.Suspected
    ]

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
