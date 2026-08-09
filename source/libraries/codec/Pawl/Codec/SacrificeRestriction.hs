module Pawl.Codec.SacrificeRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction

-- | An object with one named key, the shape Pawl.Codec.AttackRequirement has and
-- for its reason: Pawl.Types.SacrificeRestriction is a newtype over one field,
-- so there is no sum for a tag to discriminate. Pawl.Codec.CombatRestriction is
-- tagged because that type has five arms.
--
-- The key is "affected" and not "subject": CR 701.21a's prohibition names the
-- permanents it restricts, which is the word CombatRestriction's payload spells,
-- where a requirement's "subject" is opposed to an object it acts on.
toJson :: SacrificeRestriction.SacrificeRestriction -> Value.Value
toJson sr =
  Common.object (Common.requiredPair "affected" Affected.toJson (SacrificeRestriction.affected sr))

fromJson :: Value.Value -> Either Text.Text SacrificeRestriction.SacrificeRestriction
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  pure (SacrificeRestriction.MkSacrificeRestriction a)
