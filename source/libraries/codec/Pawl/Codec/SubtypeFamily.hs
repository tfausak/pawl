module Pawl.Codec.SubtypeFamily where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

toJson :: SubtypeFamily.SubtypeFamily -> Value.Value
toJson f = Common.nullary $ case f of
  SubtypeFamily.BasicLandType -> "BasicLandType"
  SubtypeFamily.CreatureType -> "CreatureType"

fromJson :: Value.Value -> Either Text.Text SubtypeFamily.SubtypeFamily
fromJson =
  Common.decodeNullary
    "SubtypeFamily"
    [ ("BasicLandType", SubtypeFamily.BasicLandType),
      ("CreatureType", SubtypeFamily.CreatureType)
    ]
