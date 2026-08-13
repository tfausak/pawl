{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerStaticAbilitySpec where

import qualified Pawl.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerStaticAbility" $ do
  Spec.it s "MkPlayerStaticAbility" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1))
      """ {"scope":{"type":"EachPlayer"},"effect":{"type":"CantCastMoreThan","value":1}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerStaticAbility.codec
