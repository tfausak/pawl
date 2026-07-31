-- | The @PlayerScope ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.PlayerScope where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerScope as PlayerScope

playerScopeToJson :: PlayerScope.PlayerScope -> Value
playerScopeToJson s = Json.nullary . Text.pack $ case s of
  PlayerScope.You -> "You"
  PlayerScope.Opponents -> "Opponents"
  PlayerScope.EachPlayer -> "EachPlayer"

jsonToPlayerScope :: Value -> Either Text PlayerScope.PlayerScope
jsonToPlayerScope =
  Json.decodeNullary
    (Text.pack "PlayerScope")
    [ (Text.pack "You", PlayerScope.You),
      (Text.pack "Opponents", PlayerScope.Opponents),
      (Text.pack "EachPlayer", PlayerScope.EachPlayer)
    ]
