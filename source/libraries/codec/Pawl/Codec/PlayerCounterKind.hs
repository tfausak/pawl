module Pawl.Codec.PlayerCounterKind where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

toJson :: PlayerCounterKind.PlayerCounterKind -> Value.Value
toJson k = Common.nullary $ case k of
  PlayerCounterKind.Energy -> "Energy"
  PlayerCounterKind.Poison -> "Poison"
  PlayerCounterKind.Experience -> "Experience"

fromJson :: Value.Value -> Either Text.Text PlayerCounterKind.PlayerCounterKind
fromJson =
  Common.decodeNullary
    "PlayerCounterKind"
    [ ("Energy", PlayerCounterKind.Energy),
      ("Poison", PlayerCounterKind.Poison),
      ("Experience", PlayerCounterKind.Experience)
    ]
