{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ZoneSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Zone" $ do
  Spec.it s "Library" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Library
      """ {"type":"Library"} """
  Spec.it s "Hand" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Hand
      """ {"type":"Hand"} """
  Spec.it s "Graveyard" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Graveyard
      """ {"type":"Graveyard"} """
  Spec.it s "Battlefield" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Battlefield
      """ {"type":"Battlefield"} """
  Spec.it s "Stack" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Stack
      """ {"type":"Stack"} """
  Spec.it s "Exile" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Exile
      """ {"type":"Exile"} """
  Spec.it s "Command" $
    Common.assertJsonCodec
      s
      Zone.toJson
      Zone.fromJson
      Zone.Command
      """ {"type":"Command"} """
