module Pawl.Codec.ZoneChangePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

-- | Saying what the object is -- CR 702.34a's "this card", Anafenza, the
-- Foremost's "a nontoken creature" -- is the marked case, so the trivial
-- predicate is what a pattern that says nothing about the object means.
defaultWhatObject :: Filter.Filter Keyword.Keyword
defaultWhatObject = Filter.And []

-- | CR 109.5 reads a controller relation against the effect's source;
-- "anyone's" is the unrestricted reading, so it is what a pattern that says
-- nothing means.
defaultWhoseObject :: ControllerRelation.ControllerRelation
defaultWhoseObject = ControllerRelation.Anyones

toJson :: ZoneChangePattern.ZoneChangePattern -> Value.Value
toJson p =
  Common.object
    ( Common.requiredPair "whenDestination" Zone.toJson (ZoneChangePattern.whenDestination p)
        <> Common.optionalPair "whatObject" defaultWhatObject (Filter.toJson Keyword.toJson) (ZoneChangePattern.whatObject p)
        <> Common.optionalPair "whoseObject" defaultWhoseObject ControllerRelation.toJson (ZoneChangePattern.whoseObject p)
    )

fromJson :: Value.Value -> Either Text.Text ZoneChangePattern.ZoneChangePattern
fromJson value = do
  ps <- Common.asObject value
  d <- Common.field "whenDestination" ps >>= Zone.fromJson
  o <- Common.defaultedField "whatObject" defaultWhatObject (Filter.fromJson Keyword.fromJson) ps
  w <- Common.defaultedField "whoseObject" defaultWhoseObject ControllerRelation.fromJson ps
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whatObject = o,
        ZoneChangePattern.whoseObject = w
      }
