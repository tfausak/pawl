{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CrewRestriction where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CrewRestriction as CrewRestriction

-- | An object with one named key, Pawl.Codec.SacrificeRestriction's shape and for
-- its reason: Pawl.Types.CrewRestriction is a newtype over one field, so there is
-- no sum for a tag to discriminate.
codec :: Codec.Codec CrewRestriction.CrewRestriction
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CrewRestriction.affected
  pure CrewRestriction.MkCrewRestriction {CrewRestriction.affected = affected}
