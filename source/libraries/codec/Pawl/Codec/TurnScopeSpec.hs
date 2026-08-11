{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TurnScopeSpec where

import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnScope" $ do
  Spec.it s "EachTurn" $
    Common.assertJsonCodec
      s
      TurnScope.toJson
      TurnScope.fromJson
      TurnScope.EachTurn
      """ {"type":"EachTurn"} """
  Spec.it s "ControllersTurn" $
    Common.assertJsonCodec
      s
      TurnScope.toJson
      TurnScope.fromJson
      TurnScope.ControllersTurn
      """ {"type":"ControllersTurn"} """
  Spec.it s "OpponentsTurn" $
    Common.assertJsonCodec
      s
      TurnScope.toJson
      TurnScope.fromJson
      TurnScope.OpponentsTurn
      """ {"type":"OpponentsTurn"} """
