{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerScopeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerScope" $ do
  Spec.it s "You" $
    Common.assertJsonCodec
      s
      PlayerScope.toJson
      PlayerScope.fromJson
      PlayerScope.You
      """ {"type":"You"} """
  Spec.it s "Opponents" $
    Common.assertJsonCodec
      s
      PlayerScope.toJson
      PlayerScope.fromJson
      PlayerScope.Opponents
      """ {"type":"Opponents"} """
  Spec.it s "EachPlayer" $
    Common.assertJsonCodec
      s
      PlayerScope.toJson
      PlayerScope.fromJson
      PlayerScope.EachPlayer
      """ {"type":"EachPlayer"} """
  Spec.it s "ControllingMostPermanents" $
    Common.assertJsonCodec
      s
      PlayerScope.toJson
      PlayerScope.fromJson
      PlayerScope.ControllingMostPermanents
      """ {"type":"ControllingMostPermanents"} """
