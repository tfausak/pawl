module Pawl.Codec.PlayerStaticAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

toJson :: PlayerStaticAbility.PlayerStaticAbility -> Value.Value
toJson pa =
  Common.object
    ( Common.requiredPair "scope" PlayerScope.toJson (PlayerStaticAbility.scope pa)
        <> Common.requiredPair "effect" PlayerEffect.toJson (PlayerStaticAbility.effect pa)
    )

fromJson :: Value.Value -> Either Text.Text PlayerStaticAbility.PlayerStaticAbility
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "scope" ps >>= PlayerScope.fromJson
  e <- Common.field "effect" ps >>= PlayerEffect.fromJson
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)
