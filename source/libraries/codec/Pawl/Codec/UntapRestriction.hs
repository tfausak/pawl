module Pawl.Codec.UntapRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.UntapRestriction as UntapRestriction

-- | An object with one named key, Pawl.Codec.SacrificeRestriction's shape and for
-- its reason: Pawl.Types.UntapRestriction is a newtype over one field, so there
-- is no sum for a tag to discriminate.
toJson :: UntapRestriction.UntapRestriction -> Value.Value
toJson ur =
  Value.object (Common.requiredPair "affected" Affected.toJson (UntapRestriction.affected ur))

fromJson :: Value.Value -> Either Text.Text UntapRestriction.UntapRestriction
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  pure (UntapRestriction.MkUntapRestriction a)
