module Pawl.Codec.AlternativeActivationCostSpec where

import qualified Pawl.Codec.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AlternativeActivationCost" $ do
  -- Kíli the Resourceful's shape: the first equip ability each turn, for {0}.
  Spec.it s "the first equip ability each turn" $
    Common.assertCodec
      s
      AlternativeActivationCost.codec
      AlternativeActivationCost.MkAlternativeActivationCost
        { AlternativeActivationCost.grantedBy = KeywordDesignator.OfFamily KeywordFamily.Equip,
          AlternativeActivationCost.onlyFirst = Just TurnScope.EachTurn,
          AlternativeActivationCost.cost = ManaCost.MkManaCost []
        }
      " {\"grantedBy\":{\"type\":\"OfFamily\",\"value\":{\"type\":\"Equip\"}},\"onlyFirst\":{\"type\":\"EachTurn\"},\"cost\":[]} "
  -- onlyFirst is DEFAULTED, so its absence is its own case.
  Spec.it s "every matching ability" $
    Common.assertCodec
      s
      AlternativeActivationCost.codec
      AlternativeActivationCost.MkAlternativeActivationCost
        { AlternativeActivationCost.grantedBy = KeywordDesignator.OfFamily KeywordFamily.Equip,
          AlternativeActivationCost.onlyFirst = Nothing,
          AlternativeActivationCost.cost = ManaCost.MkManaCost [ManaSymbol.Generic 1]
        }
      " {\"grantedBy\":{\"type\":\"OfFamily\",\"value\":{\"type\":\"Equip\"}},\"cost\":[{\"type\":\"Generic\",\"value\":1}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s AlternativeActivationCost.codec
