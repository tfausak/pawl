module Pawl.Codec.AnyNumberMatchingSpec where

import qualified Pawl.Codec.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AnyNumberMatching" $ do
  Spec.it s "no ceiling writes no key" $
    Common.assertCodec
      s
      AnyNumberMatching.codec
      (AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Creature) Nothing)
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "a ceiling" $
    Common.assertCodec
      s
      AnyNumberMatching.codec
      (AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Land) (Just (Quantity.Literal 2)))
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"atMost\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AnyNumberMatching.codec
