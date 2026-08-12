{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerScopeSpec where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerScope" $ do
  Spec.it s "You" $
    Common.assertCodec
      s
      PlayerScope.codec
      PlayerScope.You
      """ {"type":"You"} """
  Spec.it s "Opponents" $
    Common.assertCodec
      s
      PlayerScope.codec
      PlayerScope.Opponents
      """ {"type":"Opponents"} """
  Spec.it s "EachPlayer" $
    Common.assertCodec
      s
      PlayerScope.codec
      PlayerScope.EachPlayer
      """ {"type":"EachPlayer"} """
  Spec.it s "ControllingMostPermanents" $
    Common.assertCodec
      s
      PlayerScope.codec
      PlayerScope.ControllingMostPermanents
      """ {"type":"ControllingMostPermanents"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayerScope.codec
