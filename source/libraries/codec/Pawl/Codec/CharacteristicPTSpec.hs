{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CharacteristicPTSpec where

import qualified Pawl.Codec.CharacteristicPT as CharacteristicPT
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CharacteristicPT" $ do
  -- Tarmogoyf's pair, which is CR 208.2's shape and the asymmetric case this
  -- record exists for: both keys are a Quantity, so a fixture with the same
  -- value in each would round-trip a codec that swapped them.
  Spec.it s "MkCharacteristicPT, an asymmetric pair" $
    Common.assertCodec
      s
      CharacteristicPT.codec
      ( CharacteristicPT.MkCharacteristicPT
          { CharacteristicPT.power = Quantity.Star,
            CharacteristicPT.toughness = Quantity.Plus (Plus.MkPlus (Quantity.Literal 1) Quantity.Star)
          }
      )
      """ {"power":{"type":"Star"},"toughness":{"type":"Plus","value":{"left":{"type":"Literal","value":1},"right":{"type":"Star"}}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s CharacteristicPT.codec
