-- | The @ControllerRelation ⇆ Json@ codec (#481).
module Pawl.Codec.ControllerRelation where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ControllerRelation as ControllerRelation

controllerRelationToJson :: ControllerRelation.ControllerRelation -> Value
controllerRelationToJson r = Json.nullary . Text.pack $ case r of
  ControllerRelation.Yours -> "Yours"
  ControllerRelation.Anyones -> "Anyones"
  ControllerRelation.Opponents -> "Opponents"

jsonToControllerRelation :: Value -> Either Text ControllerRelation.ControllerRelation
jsonToControllerRelation =
  Json.decodeNullary
    (Text.pack "ControllerRelation")
    [ (Text.pack "Yours", ControllerRelation.Yours),
      (Text.pack "Anyones", ControllerRelation.Anyones),
      (Text.pack "Opponents", ControllerRelation.Opponents)
    ]
