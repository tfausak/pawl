module Pawl.Codec.PrototypeSpec where

import qualified Pawl.Codec.Prototype as Prototype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Prototype as Prototype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Prototype" $ do
  -- CR 718.1: Autonomous Assembler's inset frame, "Prototype {1}{W} -- 2/2".
  Spec.it s "MkPrototype" $
    Common.assertCodec
      s
      Prototype.codec
      ( Prototype.MkPrototype
          { Prototype.cost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)],
            Prototype.power = 2,
            Prototype.toughness = 2
          }
      )
      " {\"cost\":[{\"type\":\"Generic\",\"value\":1},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}}],\"power\":2,\"toughness\":2} "
  -- CR 718.3b: the coloured symbol is what makes a prototyped Autonomous
  -- Assembler white, so an encoding that dropped it would still round-trip the
  -- box above. Told apart from the same frame written colourless.
  Spec.it s "the inset cost's colour survives the round trip" $
    Spec.assertBool
      s
      ( Codec.encode Prototype.codec (Prototype.MkPrototype (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)]) 2 2)
          /= Codec.encode Prototype.codec (Prototype.MkPrototype (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType ManaType.Colorless]) 2 2)
      )
      "{1}{W} and {1}{C} encode differently"
  Spec.it s "has a schema" $ Common.assertHasSchema s Prototype.codec
