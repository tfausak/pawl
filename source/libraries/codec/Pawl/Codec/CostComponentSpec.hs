module Pawl.Codec.CostComponentSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostComponent" $ do
  Spec.it s "TapThis" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson CostComponent.TapThis "{\"type\":\"TapThis\"}"
  Spec.it s "UntapThis" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson CostComponent.UntapThis "{\"type\":\"UntapThis\"}"
  Spec.it s "SacrificeThis" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson CostComponent.SacrificeThis "{\"type\":\"SacrificeThis\"}"
  Spec.it s "PayLife" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson (CostComponent.PayLife 2) "{\"type\":\"PayLife\",\"value\":2}"
  -- Village Rites' one creature: the count and the Filter both ride the
  -- payload, positionally.
  Spec.it s "Sacrifice" $
    Common.assertJsonCodec
      s
      CostComponent.toJson
      CostComponent.fromJson
      (CostComponent.Sacrifice 2 (Filter.HasSubtype Subtype.Mountain))
      "{\"type\":\"Sacrifice\",\"value\":[2,{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Mountain\"}}]}"
  Spec.it s "DiscardCards" $
    Common.assertJsonCodec
      s
      CostComponent.toJson
      CostComponent.fromJson
      (CostComponent.DiscardCards 2)
      "{\"type\":\"DiscardCards\",\"value\":2}"
  Spec.it s "DiscardThis" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson CostComponent.DiscardThis "{\"type\":\"DiscardThis\"}"
  Spec.it s "PayEnergy" $
    Common.assertJsonCodec s CostComponent.toJson CostComponent.fromJson (CostComponent.PayEnergy 2) "{\"type\":\"PayEnergy\",\"value\":2}"
  -- CR 606.4's two halves, Jace Beleren's +2 and -1.
  Spec.it s "AddLoyaltyToThis" $
    Common.assertJsonCodec
      s
      CostComponent.toJson
      CostComponent.fromJson
      (CostComponent.AddLoyaltyToThis 2)
      "{\"type\":\"AddLoyaltyToThis\",\"value\":2}"
  Spec.it s "RemoveLoyaltyFromThis" $
    Common.assertJsonCodec
      s
      CostComponent.toJson
      CostComponent.fromJson
      (CostComponent.RemoveLoyaltyFromThis 1)
      "{\"type\":\"RemoveLoyaltyFromThis\",\"value\":1}"
