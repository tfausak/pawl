-- | The @ZoneChangePattern ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.ZoneChangePattern where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.ControllerRelation (controllerRelationToJson, jsonToControllerRelation)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Codec.ZoneChangeSubject (jsonToZoneChangeSubject, zoneChangeSubjectToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

zoneChangePatternToJson :: ZoneChangePattern.ZoneChangePattern -> Value
zoneChangePatternToJson p =
  Json.jObject
    [ (Text.pack "whenDestination", zoneToJson (ZoneChangePattern.whenDestination p)),
      (Text.pack "whichObject", zoneChangeSubjectToJson (ZoneChangePattern.whichObject p)),
      (Text.pack "whoseObject", controllerRelationToJson (ZoneChangePattern.whoseObject p))
    ]

jsonToZoneChangePattern :: Value -> Either Text ZoneChangePattern.ZoneChangePattern
jsonToZoneChangePattern value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "whenDestination") ps >>= jsonToZone
  s <- Json.field (Text.pack "whichObject") ps >>= jsonToZoneChangeSubject
  w <- Json.field (Text.pack "whoseObject") ps >>= jsonToControllerRelation
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whichObject = s,
        ZoneChangePattern.whoseObject = w
      }
