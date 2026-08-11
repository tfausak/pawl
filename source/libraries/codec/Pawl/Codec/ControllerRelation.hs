module Pawl.Codec.ControllerRelation where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ControllerRelation as ControllerRelation

toJson :: ControllerRelation.ControllerRelation -> Value.Value
toJson r = Common.nullary $ case r of
  ControllerRelation.Yours -> "Yours"
  ControllerRelation.Anyones -> "Anyones"
  ControllerRelation.Opponents -> "Opponents"

fromJson :: Value.Value -> Either Text.Text ControllerRelation.ControllerRelation
fromJson =
  Common.decodeNullary
    "ControllerRelation"
    [ ("Yours", ControllerRelation.Yours),
      ("Anyones", ControllerRelation.Anyones),
      ("Opponents", ControllerRelation.Opponents)
    ]
