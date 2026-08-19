{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.UntapRestriction where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.UntapRestriction as UntapRestriction

-- | An object with one named key, Pawl.Codec.SacrificeRestriction's shape and for
-- its reason: Pawl.Types.UntapRestriction is a newtype over one field, so there
-- is no sum for a tag to discriminate.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec UntapRestriction.UntapRestriction
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec UntapRestriction.affected
  pure UntapRestriction.MkUntapRestriction {UntapRestriction.affected = affected}
