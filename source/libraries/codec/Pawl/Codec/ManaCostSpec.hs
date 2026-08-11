{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaCostSpec where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaCost" $ do
  Spec.it s "MkManaCost" $
    Common.assertJsonCodec
      s
      ManaCost.toJson
      ManaCost.fromJson
      (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)])
      """ [{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Red"}}}] """
  -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS {0} --
  -- so the empty array has to round-trip, not just a nonempty one.
  Spec.it s "MkManaCost []" $
    Common.assertJsonCodec
      s
      ManaCost.toJson
      ManaCost.fromJson
      (ManaCost.MkManaCost [])
      """ [] """
