{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SearchDestinationSpec where

import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SearchDestination as SearchDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SearchDestination" $ do
  Spec.it s "BattlefieldTapped" $
    Common.assertJsonCodec
      s
      SearchDestination.toJson
      SearchDestination.fromJson
      SearchDestination.BattlefieldTapped
      """ {"type":"BattlefieldTapped"} """
  Spec.it s "RevealThenHand" $
    Common.assertJsonCodec
      s
      SearchDestination.toJson
      SearchDestination.fromJson
      SearchDestination.RevealThenHand
      """ {"type":"RevealThenHand"} """
  Spec.it s "Exile" $
    Common.assertJsonCodec
      s
      SearchDestination.toJson
      SearchDestination.fromJson
      SearchDestination.Exile
      """ {"type":"Exile"} """
