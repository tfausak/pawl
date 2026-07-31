-- | The @PlayerCounterKind ⇆ Json@ codec (#481).
module Pawl.Codec.PlayerCounterKind where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

playerCounterKindToJson :: PlayerCounterKind.PlayerCounterKind -> Value
playerCounterKindToJson k = Json.nullary . Text.pack $ case k of
  PlayerCounterKind.Energy -> "Energy"
  PlayerCounterKind.Poison -> "Poison"

jsonToPlayerCounterKind :: Value -> Either Text PlayerCounterKind.PlayerCounterKind
jsonToPlayerCounterKind =
  Json.decodeNullary
    (Text.pack "PlayerCounterKind")
    [ (Text.pack "Energy", PlayerCounterKind.Energy),
      (Text.pack "Poison", PlayerCounterKind.Poison)
    ]
