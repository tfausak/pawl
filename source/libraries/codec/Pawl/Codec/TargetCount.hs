module Pawl.Codec.TargetCount where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TargetCount as TargetCount

toJson :: TargetCount.TargetCount -> Value.Value
toJson c =
  Value.object
    [ Value.pair "least" (Common.encodeNatural (TargetCount.least c)),
      Value.pair "most" (Common.encodeNatural (TargetCount.most c))
    ]

-- Both invariants Pawl.Types.TargetCount states are enforced HERE, this being
-- where a range enters the engine at all: a card whose slot takes no target is
-- not a target slot, and one whose minimum exceeds its maximum names no legal
-- number for CR 601.2c to announce.
fromJson :: Value.Value -> Either Text.Text TargetCount.TargetCount
fromJson value = do
  ps <- Common.asObject value
  least <- Common.field "least" ps >>= Common.decodeNatural
  most <- Common.field "most" ps >>= Common.decodeNatural
  if most < 1
    then Left (Text.pack "TargetCount: most must be at least 1")
    else
      if least > most
        then Left (Text.pack "TargetCount: least must not exceed most")
        else pure TargetCount.MkTargetCount {TargetCount.least = least, TargetCount.most = most}
