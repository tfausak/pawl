-- | The @PlayerRelation ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.PlayerRelation where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerRelation as PlayerRelation

playerRelationToJson :: PlayerRelation.PlayerRelation -> Value
playerRelationToJson r = Json.nullary . Text.pack $ case r of
  PlayerRelation.You -> "You"
  PlayerRelation.Opponent -> "Opponent"

jsonToPlayerRelation :: Value -> Either Text PlayerRelation.PlayerRelation
jsonToPlayerRelation =
  Json.decodeNullary
    (Text.pack "PlayerRelation")
    [ (Text.pack "You", PlayerRelation.You),
      (Text.pack "Opponent", PlayerRelation.Opponent)
    ]
