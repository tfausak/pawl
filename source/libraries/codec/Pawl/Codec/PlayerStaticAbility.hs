-- | The @PlayerStaticAbility ⇆ Json@ codec (#481).
module Pawl.Codec.PlayerStaticAbility where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerScope as PlayerScope
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

playerStaticAbilityToJson :: PlayerStaticAbility.PlayerStaticAbility -> Value
playerStaticAbilityToJson pa =
  Json.jObject
    [ (Text.pack "scope", PlayerScope.toJson (PlayerStaticAbility.scope pa)),
      (Text.pack "effect", PlayerEffect.toJson (PlayerStaticAbility.effect pa))
    ]

jsonToPlayerStaticAbility :: Value -> Either Text PlayerStaticAbility.PlayerStaticAbility
jsonToPlayerStaticAbility value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "scope") ps >>= PlayerScope.fromJson
  e <- Json.field (Text.pack "effect") ps >>= PlayerEffect.fromJson
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)
