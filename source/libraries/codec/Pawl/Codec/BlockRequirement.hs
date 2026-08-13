module Pawl.Codec.BlockRequirement where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BlockRequirement as BlockRequirement

toJson :: BlockRequirement.BlockRequirement -> Value.Value
toJson br =
  Value.object (Common.requiredPair "attacker" (Codec.encode Affected.codec) (BlockRequirement.attacker br))

fromJson :: Value.Value -> Either Text.Text BlockRequirement.BlockRequirement
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "attacker" ps >>= Codec.decode Affected.codec
  pure (BlockRequirement.MkBlockRequirement a)
