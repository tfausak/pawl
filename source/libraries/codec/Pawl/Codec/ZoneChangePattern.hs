module Pawl.Codec.ZoneChangePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangeSubject as ZoneChangeSubject
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

toJson :: ZoneChangePattern.ZoneChangePattern -> Value.Value
toJson p =
  Common.object
    [ Common.pair "whenDestination" . Zone.toJson $ ZoneChangePattern.whenDestination p,
      Common.pair "whichObject" . ZoneChangeSubject.toJson $ ZoneChangePattern.whichObject p,
      Common.pair "whoseObject" . ControllerRelation.toJson $ ZoneChangePattern.whoseObject p
    ]

fromJson :: Value.Value -> Either Text.Text ZoneChangePattern.ZoneChangePattern
fromJson value = do
  ps <- Common.asObject value
  d <- Common.field "whenDestination" ps >>= Zone.fromJson
  o <- Common.field "whichObject" ps >>= ZoneChangeSubject.fromJson
  w <- Common.field "whoseObject" ps >>= ControllerRelation.fromJson
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whichObject = o,
        ZoneChangePattern.whoseObject = w
      }
