module Pawl.Codec.CastOfferSpec where

import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastOffer" $ do
  -- CR 310.12b's own offer: both riders written out.
  Spec.it s "MkCastOffer, transformed and without paying its mana cost" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = True, CastOffer.withoutPayingManaCost = True, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AsProduced}
      " {\"transformed\":true,\"withoutPayingManaCost\":true} "
  Spec.it s "MkCastOffer, an ordinary cast omits both keys" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.defaultValue
      " {} "
  -- The two riders are independent (CR 712.11a is about a face, CR 118.9 about a
  -- cost), so each must survive on its own -- which is also what makes the
  -- elision above unambiguous.
  Spec.it s "MkCastOffer, transformed alone" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = True, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AsProduced}
      " {\"transformed\":true} "
  Spec.it s "MkCastOffer, free alone" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = True, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AsProduced}
      " {\"withoutPayingManaCost\":true} "
  -- CR 702.94a's own offer: the alternative cost STATED, which is the rider
  -- withoutPayingManaCost cannot express -- miracle pays something.
  Spec.it s "MkCastOffer, a stated alternative cost" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Just (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)])) []), CastOffer.spending = ManaSpending.AsProduced}
      " {\"payingInstead\":{\"mana\":[{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}}]}} "
  -- CR 118.14's rider, which Tinybones, the Pickpocket prints beside its offer
  -- and neither cost rider can express: it widens the payment rather than
  -- replacing the cost.
  Spec.it s "MkCastOffer, mana of any type" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AnyType}
      " {\"spending\":{\"type\":\"AnyType\"}} "
  Spec.describe s "defaultValue" $ do
    Spec.it s "carries no rider" $
      Spec.assertEq s CastOffer.defaultValue CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AsProduced}
    Spec.it s "a missing transformed key decodes as False" $
      Common.assertFromJson s (Codec.decode CastOffer.codec) "{\"withoutPayingManaCost\":false}" CastOffer.defaultValue
    Spec.it s "a missing withoutPayingManaCost key decodes as False" $
      Common.assertFromJson s (Codec.decode CastOffer.codec) "{\"transformed\":false}" CastOffer.defaultValue
    Spec.it s "a missing spending key decodes as AsProduced" $
      Common.assertFromJson s (Codec.decode CastOffer.codec) "{\"spending\":{\"type\":\"AsProduced\"}}" CastOffer.defaultValue
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastOffer.codec
