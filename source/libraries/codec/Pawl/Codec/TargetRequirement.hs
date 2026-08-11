module Pawl.Codec.TargetRequirement where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TargetRequirement as TargetRequirement

toJson :: TargetRequirement.TargetRequirement -> Value.Value
toJson r = Common.nullary $ case r of
  TargetRequirement.Required -> "Required"
  TargetRequirement.UpToOne -> "UpToOne"

fromJson :: Value.Value -> Either Text.Text TargetRequirement.TargetRequirement
fromJson =
  Common.decodeNullary
    "TargetRequirement"
    [ ("Required", TargetRequirement.Required),
      ("UpToOne", TargetRequirement.UpToOne)
    ]
