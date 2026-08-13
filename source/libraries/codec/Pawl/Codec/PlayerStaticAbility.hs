module Pawl.Codec.PlayerStaticAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

toJson :: PlayerStaticAbility.PlayerStaticAbility -> Value.Value
toJson pa =
  Value.object
    ( Common.requiredPair "scope" (Codec.encode PlayerScope.codec) (PlayerStaticAbility.scope pa)
        <> Common.requiredPair "effect" (Codec.encode PlayerEffect.codec) (PlayerStaticAbility.effect pa)
    )

fromJson :: Value.Value -> Either Text.Text PlayerStaticAbility.PlayerStaticAbility
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "scope" ps >>= Codec.decode PlayerScope.codec
  e <- Common.field "effect" ps >>= Codec.decode PlayerEffect.codec
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)
