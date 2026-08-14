module Pawl.Codec.CostComponent where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Sacrifice as Sacrifice
import qualified Pawl.Codec.TapForTotalPower as TapForTotalPower
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CostComponent as CostComponent

-- | Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so it is built from
-- 'Arm.tagged' rather than delegated to a nullary-table helper. A new arm
-- needs an entry in the list below AND a case in the hand-written @encode@
-- below it -- 'Arm.tagged' derives the decoder and the schema from the list
-- alone, but the encoder is deliberately not derived (see 'Arm.tagged'\'s own
-- Haddock), so every arm is written twice.
--
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CostComponent.CostComponent keyword)
codec keywordCodec =
  Arm.tagged
    [ Arm.nullary "TapThis" CostComponent.TapThis,
      Arm.nullary "UntapThis" CostComponent.UntapThis,
      Arm.nullary "SacrificeThis" CostComponent.SacrificeThis,
      Arm.payload "PayLife" Common.natural CostComponent.PayLife (\x -> case x of CostComponent.PayLife y -> Just y; _ -> Nothing),
      Arm.nullary "PayLifeX" CostComponent.PayLifeX,
      Arm.payload "Sacrifice" (Sacrifice.codec keywordCodec) CostComponent.Sacrifice (\x -> case x of CostComponent.Sacrifice y -> Just y; _ -> Nothing),
      Arm.payload "TapForTotalPower" (TapForTotalPower.codec keywordCodec) CostComponent.TapForTotalPower (\x -> case x of CostComponent.TapForTotalPower y -> Just y; _ -> Nothing),
      Arm.payload "DiscardCards" Common.natural CostComponent.DiscardCards (\x -> case x of CostComponent.DiscardCards y -> Just y; _ -> Nothing),
      Arm.nullary "DiscardThis" CostComponent.DiscardThis,
      Arm.payload "PayEnergy" Common.natural CostComponent.PayEnergy (\x -> case x of CostComponent.PayEnergy y -> Just y; _ -> Nothing),
      Arm.payload "AddLoyaltyToThis" Common.natural CostComponent.AddLoyaltyToThis (\x -> case x of CostComponent.AddLoyaltyToThis y -> Just y; _ -> Nothing),
      Arm.payload "RemoveLoyaltyFromThis" Common.natural CostComponent.RemoveLoyaltyFromThis (\x -> case x of CostComponent.RemoveLoyaltyFromThis y -> Just y; _ -> Nothing),
      Arm.payload "PutPlusOneCountersOnThis" Common.natural CostComponent.PutPlusOneCountersOnThis (\x -> case x of CostComponent.PutPlusOneCountersOnThis y -> Just y; _ -> Nothing),
      Arm.nullary "ExileThisFromGraveyard" CostComponent.ExileThisFromGraveyard,
      Arm.payload "ExileCardsFromGraveyard" (ExileCardsFromGraveyard.codec keywordCodec) CostComponent.ExileCardsFromGraveyard (\x -> case x of CostComponent.ExileCardsFromGraveyard y -> Just y; _ -> Nothing),
      Arm.payload "ExileTopFromGraveyard" (Filter.codec keywordCodec) CostComponent.ExileTopFromGraveyard (\x -> case x of CostComponent.ExileTopFromGraveyard y -> Just y; _ -> Nothing)
    ]
