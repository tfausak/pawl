module Pawl.Codec.PlayerScope where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerScope as PlayerScope

toJson :: PlayerScope.PlayerScope -> Value.Value
toJson s = Common.nullary $ case s of
  PlayerScope.You -> "You"
  PlayerScope.Opponents -> "Opponents"
  PlayerScope.EachPlayer -> "EachPlayer"
  PlayerScope.ControllingMostPermanents -> "ControllingMostPermanents"

fromJson :: Value.Value -> Either Text.Text PlayerScope.PlayerScope
fromJson =
  Common.decodeNullary
    "PlayerScope"
    [ ("You", PlayerScope.You),
      ("Opponents", PlayerScope.Opponents),
      ("EachPlayer", PlayerScope.EachPlayer),
      ("ControllingMostPermanents", PlayerScope.ControllingMostPermanents)
    ]
