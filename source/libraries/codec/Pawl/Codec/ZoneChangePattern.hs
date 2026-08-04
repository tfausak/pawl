module Pawl.Codec.ZoneChangePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangeSubject as ZoneChangeSubject
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

-- | Rest in Peace's "if a card would be put into a graveyard from anywhere"
-- names no object in particular -- naming one (CR 702.34a's flashback, "this
-- card") is the marked case, so naming none is what a pattern that says
-- nothing about the object means.
defaultWhichObject :: ZoneChangeSubject.ZoneChangeSubject
defaultWhichObject = ZoneChangeSubject.AnyObject

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhoseObject :: ControllerRelation.ControllerRelation
defaultWhoseObject = ControllerRelation.Anyones

toJson :: ZoneChangePattern.ZoneChangePattern -> Value.Value
toJson p =
  Common.object
    ( Common.requiredPair "whenDestination" Zone.toJson (ZoneChangePattern.whenDestination p)
        <> Common.optionalPair "whichObject" defaultWhichObject ZoneChangeSubject.toJson (ZoneChangePattern.whichObject p)
        <> Common.optionalPair "whoseObject" defaultWhoseObject ControllerRelation.toJson (ZoneChangePattern.whoseObject p)
    )

fromJson :: Value.Value -> Either Text.Text ZoneChangePattern.ZoneChangePattern
fromJson value = do
  ps <- Common.asObject value
  d <- Common.field "whenDestination" ps >>= Zone.fromJson
  o <- Common.defaultedField "whichObject" defaultWhichObject ZoneChangeSubject.fromJson ps
  w <- Common.defaultedField "whoseObject" defaultWhoseObject ControllerRelation.fromJson ps
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whichObject = o,
        ZoneChangePattern.whoseObject = w
      }
