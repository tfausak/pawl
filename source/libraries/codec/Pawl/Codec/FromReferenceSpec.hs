module Pawl.Codec.FromReferenceSpec where

import qualified Pawl.Codec.FromReference as FromReference
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromReference as FromReference
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FromReference" $ do
  Spec.it s "MkFromReference, no amount" $
    Common.assertCodec
      s
      FromReference.codec
      (FromReference.MkFromReference (Filter.HasCardType CardType.Creature) Nothing)
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "MkFromReference, an amount" $
    Common.assertCodec
      s
      FromReference.codec
      (FromReference.MkFromReference Filter.ManaValueEqualToAmount (Just (Quantity.Literal 3)))
      " {\"amount\":{\"type\":\"Literal\",\"value\":3},\"filter\":{\"type\":\"ManaValueEqualToAmount\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FromReference.codec
