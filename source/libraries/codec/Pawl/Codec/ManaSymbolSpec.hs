{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaSymbolSpec where

import qualified Pawl.Codec.ManaSymbol as ManaSymbol
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaSymbol" $ do
  Spec.it s "Generic" $
    Common.assertCodec
      s
      ManaSymbol.codec
      (ManaSymbol.Generic 2)
      """ {"type":"Generic","value":2} """
  Spec.it s "OfType" $
    Common.assertCodec
      s
      ManaSymbol.codec
      (ManaSymbol.OfType (ManaType.Colored Color.Red))
      """ {"type":"OfType","value":{"type":"Colored","value":{"type":"Red"}}} """
  -- CR 107.4e's two-halves shape, e.g. {W/U}.
  Spec.it s "Hybrid" $
    Common.assertCodec
      s
      ManaSymbol.codec
      (ManaSymbol.Hybrid (ManaType.Colored Color.White) (ManaType.Colored Color.Blue))
      """ {"type":"Hybrid","value":[{"type":"Colored","value":{"type":"White"}},{"type":"Colored","value":{"type":"Blue"}}]} """
  -- CR 107.4e's {2/B} shape.
  Spec.it s "MonocoloredHybrid" $
    Common.assertCodec
      s
      ManaSymbol.codec
      (ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Black))
      """ {"type":"MonocoloredHybrid","value":{"type":"Colored","value":{"type":"Black"}}} """
  -- CR 107.4f: {G/P}, colored not by a ManaType but by a bare Color.
  Spec.it s "Phyrexian" $
    Common.assertCodec
      s
      ManaSymbol.codec
      (ManaSymbol.Phyrexian Color.Green)
      """ {"type":"Phyrexian","value":{"type":"Green"}} """
  -- CR 107.4h's {S}: nullary, unlike every other payload-carrying arm above.
  Spec.it s "Snow" $
    Common.assertCodec
      s
      ManaSymbol.codec
      ManaSymbol.Snow
      """ {"type":"Snow"} """
  Spec.it s "Variable" $
    Common.assertCodec
      s
      ManaSymbol.codec
      ManaSymbol.Variable
      """ {"type":"Variable"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaSymbol.codec
