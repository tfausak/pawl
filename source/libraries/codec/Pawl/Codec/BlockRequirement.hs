-- | The @BlockRequirement ⇆ Json@ codec (#481).
module Pawl.Codec.BlockRequirement where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.BlockRequirement as BlockRequirement

blockRequirementToJson :: BlockRequirement.BlockRequirement -> Value
blockRequirementToJson br =
  Json.jObject [(Text.pack "attacker", Affected.toJson (BlockRequirement.attacker br))]

jsonToBlockRequirement :: Value -> Either Text BlockRequirement.BlockRequirement
jsonToBlockRequirement value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "attacker") ps >>= Affected.fromJson
  pure (BlockRequirement.MkBlockRequirement a)
