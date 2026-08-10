module Pawl.Codec.BlockPermission where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.BlockPermission as BlockPermission

-- | An object with two named keys, never a tagged sum: the type has one shape.
-- "additional" is REQUIRED rather than defaulted to one, because a permission
-- that adds nothing is not a thing any card prints and a missing key is far
-- likelier to be a typo than a deliberate zero.
toJson :: BlockPermission.BlockPermission -> Value.Value
toJson bp =
  Common.object
    ( Common.requiredPair "affected" Affected.toJson (BlockPermission.affected bp)
        <> Common.requiredPair "additional" Common.encodeNatural (BlockPermission.additional bp)
    )

fromJson :: Value.Value -> Either Text.Text BlockPermission.BlockPermission
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  n <- Common.field "additional" ps >>= Common.decodeNatural
  pure (BlockPermission.MkBlockPermission a n)
