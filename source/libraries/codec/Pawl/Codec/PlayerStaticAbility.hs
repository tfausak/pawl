-- | The @PlayerStaticAbility ⇆ Json@ codec (#481).
module Pawl.Codec.PlayerStaticAbility where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerEffect (jsonToPlayerEffect, playerEffectToJson)
import qualified Pawl.Codec.PlayerScope as PlayerScope
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

playerStaticAbilityToJson :: PlayerStaticAbility.PlayerStaticAbility -> Value
playerStaticAbilityToJson pa =
  Json.jObject
    [ (Text.pack "scope", PlayerScope.toJson (PlayerStaticAbility.scope pa)),
      (Text.pack "effect", playerEffectToJson (PlayerStaticAbility.effect pa))
    ]

jsonToPlayerStaticAbility :: Value -> Either Text PlayerStaticAbility.PlayerStaticAbility
jsonToPlayerStaticAbility value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "scope") ps >>= PlayerScope.fromJson
  e <- Json.field (Text.pack "effect") ps >>= jsonToPlayerEffect
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)
