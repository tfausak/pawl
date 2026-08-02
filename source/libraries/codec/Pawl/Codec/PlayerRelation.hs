module Pawl.Codec.PlayerRelation where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerRelation as PlayerRelation

toJson :: PlayerRelation.PlayerRelation -> Value.Value
toJson r = Common.nullary $ case r of
  PlayerRelation.You -> "You"
  PlayerRelation.Opponent -> "Opponent"

fromJson :: Value.Value -> Either Text.Text PlayerRelation.PlayerRelation
fromJson =
  Common.decodeNullary
    "PlayerRelation"
    [ ("You", PlayerRelation.You),
      ("Opponent", PlayerRelation.Opponent)
    ]
