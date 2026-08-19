{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerStaticAbility where

import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec PlayerStaticAbility.PlayerStaticAbility
codec = Fields.object $ do
  scope <- Fields.required "scope" PlayerScope.codec PlayerStaticAbility.scope
  effect <- Fields.required "effect" PlayerEffect.codec PlayerStaticAbility.effect
  pure
    PlayerStaticAbility.MkPlayerStaticAbility
      { PlayerStaticAbility.scope = scope,
        PlayerStaticAbility.effect = effect
      }
