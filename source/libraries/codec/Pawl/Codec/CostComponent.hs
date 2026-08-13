module Pawl.Codec.CostComponent where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Json.Value as Value
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
codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (CostComponent.CostComponent keyword)
codec keywordCodec =
  Arm.tagged
    encode
    [ Arm.nullary "TapThis" CostComponent.TapThis,
      Arm.nullary "UntapThis" CostComponent.UntapThis,
      Arm.nullary "SacrificeThis" CostComponent.SacrificeThis,
      Arm.payload "PayLife" Common.natural CostComponent.PayLife,
      Arm.nullary "PayLifeX" CostComponent.PayLifeX,
      Arm.payload "Sacrifice" (Common.tuple Common.natural (Filter.codec keywordCodec)) (uncurry CostComponent.Sacrifice),
      Arm.payload "TapForTotalPower" (Common.tuple Common.natural (Filter.codec keywordCodec)) (uncurry CostComponent.TapForTotalPower),
      Arm.payload "DiscardCards" Common.natural CostComponent.DiscardCards,
      Arm.nullary "DiscardThis" CostComponent.DiscardThis,
      Arm.payload "PayEnergy" Common.natural CostComponent.PayEnergy,
      Arm.payload "AddLoyaltyToThis" Common.natural CostComponent.AddLoyaltyToThis,
      Arm.payload "RemoveLoyaltyFromThis" Common.natural CostComponent.RemoveLoyaltyFromThis,
      Arm.payload "PutPlusOneCountersOnThis" Common.natural CostComponent.PutPlusOneCountersOnThis,
      Arm.nullary "ExileThisFromGraveyard" CostComponent.ExileThisFromGraveyard,
      Arm.payload "ExileCardsFromGraveyard" (Common.tuple Common.natural (Filter.codec keywordCodec)) (uncurry CostComponent.ExileCardsFromGraveyard),
      Arm.payload "ExileTopFromGraveyard" (Filter.codec keywordCodec) CostComponent.ExileTopFromGraveyard
    ]
  where
    encode c = case c of
      CostComponent.TapThis -> Common.nullary "TapThis"
      CostComponent.UntapThis -> Common.nullary "UntapThis"
      CostComponent.SacrificeThis -> Common.nullary "SacrificeThis"
      CostComponent.PayLife n -> Common.tagged "PayLife" . Just $ Common.encodeNatural n
      CostComponent.PayLifeX -> Common.nullary "PayLifeX"
      CostComponent.Sacrifice n c_ -> Common.tagged "Sacrifice" . Just . Value.array $ [Common.encodeNatural n, Codec.encode (Filter.codec keywordCodec) c_]
      CostComponent.TapForTotalPower n c_ -> Common.tagged "TapForTotalPower" . Just . Value.array $ [Common.encodeNatural n, Codec.encode (Filter.codec keywordCodec) c_]
      CostComponent.DiscardCards n -> Common.tagged "DiscardCards" . Just $ Common.encodeNatural n
      CostComponent.DiscardThis -> Common.nullary "DiscardThis"
      CostComponent.PayEnergy n -> Common.tagged "PayEnergy" . Just $ Common.encodeNatural n
      CostComponent.AddLoyaltyToThis n -> Common.tagged "AddLoyaltyToThis" . Just $ Common.encodeNatural n
      CostComponent.RemoveLoyaltyFromThis n -> Common.tagged "RemoveLoyaltyFromThis" . Just $ Common.encodeNatural n
      CostComponent.PutPlusOneCountersOnThis n -> Common.tagged "PutPlusOneCountersOnThis" . Just $ Common.encodeNatural n
      CostComponent.ExileThisFromGraveyard -> Common.nullary "ExileThisFromGraveyard"
      CostComponent.ExileCardsFromGraveyard n c_ -> Common.tagged "ExileCardsFromGraveyard" . Just . Value.array $ [Common.encodeNatural n, Codec.encode (Filter.codec keywordCodec) c_]
      CostComponent.ExileTopFromGraveyard c_ -> Common.tagged "ExileTopFromGraveyard" . Just $ Codec.encode (Filter.codec keywordCodec) c_
