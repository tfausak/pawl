{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SacrificeRestriction where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction

-- | An object with one named key, the shape Pawl.Codec.AttackRequirement has and
-- for its reason: Pawl.Types.SacrificeRestriction is a newtype over one field,
-- so there is no sum for a tag to discriminate. Pawl.Codec.CombatRestriction is
-- tagged because that type has six arms.
--
-- The key is "affected" and not "subject": CR 701.21a's prohibition names the
-- permanents it restricts, which is the word CombatRestriction's payload spells,
-- where a requirement's "subject" is opposed to an object it acts on.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec SacrificeRestriction.SacrificeRestriction
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec SacrificeRestriction.affected
  pure SacrificeRestriction.MkSacrificeRestriction {SacrificeRestriction.affected = affected}
