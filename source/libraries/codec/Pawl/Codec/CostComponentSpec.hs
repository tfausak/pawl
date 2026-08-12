{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CostComponentSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation
-- anywhere in the pool.
toJson :: CostComponent.CostComponent Keyword.Keyword -> Value.Value
toJson = CostComponent.toJson Keyword.toJson

fromJson :: Value.Value -> Either Text.Text (CostComponent.CostComponent Keyword.Keyword)
fromJson = CostComponent.fromJson Keyword.fromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostComponent" $ do
  Spec.it s "TapThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      CostComponent.TapThis
      """ {"type":"TapThis"} """
  Spec.it s "UntapThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      CostComponent.UntapThis
      """ {"type":"UntapThis"} """
  Spec.it s "SacrificeThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      CostComponent.SacrificeThis
      """ {"type":"SacrificeThis"} """
  Spec.it s "PayLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.PayLife 2)
      """ {"type":"PayLife","value":2} """
  -- The count and the Filter both ride the payload, positionally.
  Spec.it s "Sacrifice" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.Sacrifice 2 (Filter.HasSubtype Subtype.Mountain))
      """ {"type":"Sacrifice","value":[2,{"type":"HasSubtype","value":{"type":"Mountain"}}]} """
  -- The THRESHOLD and the Filter ride the payload positionally, Sacrifice's
  -- shape -- but the Natural means something else here (CR 702.122a's total
  -- power, not a count of objects), which is why the two are separate arms.
  Spec.it s "TapForTotalPower" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.TapForTotalPower 6 (Filter.HasCardType CardType.Creature))
      """ {"type":"TapForTotalPower","value":[6,{"type":"HasCardType","value":{"type":"Creature"}}]} """
  Spec.it s "DiscardCards" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.DiscardCards 2)
      """ {"type":"DiscardCards","value":2} """
  Spec.it s "DiscardThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      CostComponent.DiscardThis
      """ {"type":"DiscardThis"} """
  Spec.it s "PayEnergy" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.PayEnergy 2)
      """ {"type":"PayEnergy","value":2} """
  -- CR 606.4's two halves.
  Spec.it s "AddLoyaltyToThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.AddLoyaltyToThis 2)
      """ {"type":"AddLoyaltyToThis","value":2} """
  Spec.it s "RemoveLoyaltyFromThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.RemoveLoyaltyFromThis 1)
      """ {"type":"RemoveLoyaltyFromThis","value":1} """
  -- CR 118.12's counter-placing cost, CR 701.63a's endure.
  Spec.it s "PutPlusOneCountersOnThis" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.PutPlusOneCountersOnThis 1)
      """ {"type":"PutPlusOneCountersOnThis","value":1} """
  -- CR 406.2's two halves: the one that names the object the cost is on, and
  -- the one that names a count and a criterion.
  Spec.it s "ExileThisFromGraveyard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      CostComponent.ExileThisFromGraveyard
      """ {"type":"ExileThisFromGraveyard"} """
  Spec.it s "ExileCardsFromGraveyard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.ExileCardsFromGraveyard 1 (Filter.HasCardType CardType.Creature))
      """ {"type":"ExileCardsFromGraveyard","value":[1,{"type":"HasCardType","value":{"type":"Creature"}}]} """
  Spec.it s "ExileTopFromGraveyard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (CostComponent.ExileTopFromGraveyard (Filter.HasCardType CardType.Creature))
      """ {"type":"ExileTopFromGraveyard","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
