module Pawl.Codec.BlockRequirement where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BlockRequirement as BlockRequirement

toJson :: BlockRequirement.BlockRequirement -> Value.Value
toJson br =
  Common.object (Common.requiredPair "attacker" Affected.toJson (BlockRequirement.attacker br))

fromJson :: Value.Value -> Either Text.Text BlockRequirement.BlockRequirement
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "attacker" ps >>= Affected.fromJson
  pure (BlockRequirement.MkBlockRequirement a)
