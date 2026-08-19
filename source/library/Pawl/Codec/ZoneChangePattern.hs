{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ZoneChangePattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
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

codec :: Codec.Codec ZoneChangePattern.ZoneChangePattern
codec = Fields.object $ do
  whenDestination <- Fields.required "whenDestination" Zone.codec ZoneChangePattern.whenDestination
  whoseObject <- Fields.defaulted "whoseObject" defaultWhoseObject ControllerRelation.codec ZoneChangePattern.whoseObject
  whatObject <- Fields.defaulted "whatObject" defaultWhatObject (Filter.codec Keyword.codec) ZoneChangePattern.whatObject
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = whenDestination,
        ZoneChangePattern.whoseObject = whoseObject,
        ZoneChangePattern.whatObject = whatObject
      }
