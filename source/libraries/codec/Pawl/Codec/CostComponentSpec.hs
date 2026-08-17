{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CostComponentSpec where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation
-- anywhere in the pool.
codec :: Codec.Codec (CostComponent.CostComponent Keyword.Keyword)
codec = CostComponent.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostComponent" $ do
  Spec.it s "TapThis" $
    Common.assertCodec
      s
      codec
      CostComponent.TapThis
      """ {"type":"TapThis"} """
  Spec.it s "UntapThis" $
    Common.assertCodec
      s
      codec
      CostComponent.UntapThis
      """ {"type":"UntapThis"} """
  Spec.it s "SacrificeThis" $
    Common.assertCodec
      s
      codec
      CostComponent.SacrificeThis
      """ {"type":"SacrificeThis"} """
  Spec.it s "PayLife" $
    Common.assertCodec
      s
      codec
      (CostComponent.PayLife 2)
      """ {"type":"PayLife","value":2} """
  -- CR 601.2b's announced X, which carries no number until it is announced.
  Spec.it s "PayLifeX" $
    Common.assertCodec
      s
      codec
      CostComponent.PayLifeX
      """ {"type":"PayLifeX"} """
  -- The count and the Filter both ride the payload, positionally.
  Spec.it s "Sacrifice" $
    Common.assertCodec
      s
      codec
      (CostComponent.Sacrifice (Sacrifice.MkSacrifice 2 (Filter.HasSubtype Subtype.Mountain)))
      """ {"type":"Sacrifice","value":{"count":2,"whichPermanents":{"type":"HasSubtype","value":{"type":"Mountain"}}}} """
  -- The THRESHOLD and the Filter ride the payload positionally, Sacrifice's
  -- shape -- but the Natural means something else here (CR 702.122a's total
  -- power, not a count of objects), which is why the two are separate arms.
  Spec.it s "TapForTotalPower" $
    Common.assertCodec
      s
      codec
      (CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower 6 (Filter.HasCardType CardType.Creature)))
      """ {"type":"TapForTotalPower","value":{"totalPower":6,"whichPermanents":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  -- A COUNT and a Filter, Sacrifice's payload shape spelled over tapping --
  -- Springleaf Drum's one untapped creature.
  Spec.it s "TapPermanents" $
    Common.assertCodec
      s
      codec
      (CostComponent.TapPermanents (TapPermanents.MkTapPermanents 1 (Filter.HasCardType CardType.Creature)))
      """ {"type":"TapPermanents","value":{"count":1,"whichPermanents":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "DiscardCards" $
    Common.assertCodec
      s
      codec
      (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 2 (Filter.And [])))
      """ {"type":"DiscardCards","value":{"count":2,"whichCards":{"type":"And","value":[]}}} """
  Spec.it s "DiscardThis" $
    Common.assertCodec
      s
      codec
      CostComponent.DiscardThis
      """ {"type":"DiscardThis"} """
  Spec.it s "PayEnergy" $
    Common.assertCodec
      s
      codec
      (CostComponent.PayEnergy 2)
      """ {"type":"PayEnergy","value":2} """
  -- CR 606.4's two halves.
  Spec.it s "AddLoyaltyToThis" $
    Common.assertCodec
      s
      codec
      (CostComponent.AddLoyaltyToThis 2)
      """ {"type":"AddLoyaltyToThis","value":2} """
  Spec.it s "RemoveLoyaltyFromThis" $
    Common.assertCodec
      s
      codec
      (CostComponent.RemoveLoyaltyFromThis 1)
      """ {"type":"RemoveLoyaltyFromThis","value":1} """
  -- CR 118.12's counter-placing cost, CR 701.63a's endure.
  Spec.it s "PutPlusOneCountersOnThis" $
    Common.assertCodec
      s
      codec
      (CostComponent.PutPlusOneCountersOnThis 1)
      """ {"type":"PutPlusOneCountersOnThis","value":1} """
  -- CR 701.68a as a cost, CR 601.2f/602.1b/118.12's three positions for it.
  Spec.it s "Blight" $
    Common.assertCodec
      s
      codec
      (CostComponent.Blight 2)
      """ {"type":"Blight","value":2} """
  -- CR 406.2's two halves: the one that names the object the cost is on, and
  -- the one that names a count and a criterion.
  Spec.it s "ExileThisFromGraveyard" $
    Common.assertCodec
      s
      codec
      CostComponent.ExileThisFromGraveyard
      """ {"type":"ExileThisFromGraveyard"} """
  Spec.it s "ExileCardsFromGraveyard" $
    Common.assertCodec
      s
      codec
      (CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard 1 (Filter.HasCardType CardType.Creature)))
      """ {"type":"ExileCardsFromGraveyard","value":{"count":1,"whichCards":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "ExileTopFromGraveyard" $
    Common.assertCodec
      s
      codec
      (CostComponent.ExileTopFromGraveyard (Filter.HasCardType CardType.Creature))
      """ {"type":"ExileTopFromGraveyard","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s codec
