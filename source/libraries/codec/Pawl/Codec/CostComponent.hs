module Pawl.Codec.CostComponent where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.DiscardCards as DiscardCards
import qualified Pawl.Codec.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Sacrifice as Sacrifice
import qualified Pawl.Codec.TapForTotalPower as TapForTotalPower
import qualified Pawl.Codec.TapPermanents as TapPermanents
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.DiscardCause as DiscardCause

-- | Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so it is built from
-- 'Arm.tagged' rather than delegated to a nullary-table helper -- which derives
-- the encoder, the decoder and the schema from the list below, so a new arm is
-- one entry and nothing else. Note that the list is the ONLY thing that says an
-- arm exists: every extractor carries its own @_ -> Nothing@, so a constructor
-- with no entry here compiles, encodes as @{}@ and has no round-trip test
-- (#1715).
--
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CostComponent.CostComponent keyword)
codec keywordCodec =
  Arm.tagged
    [ Arm.nullary "TapThis" CostComponent.TapThis,
      Arm.nullary "UntapThis" CostComponent.UntapThis,
      Arm.nullary "SacrificeThis" CostComponent.SacrificeThis,
      Arm.nullary "ReturnThis" CostComponent.ReturnThis,
      Arm.payload "PayLife" Common.natural CostComponent.PayLife (\x -> case x of CostComponent.PayLife y -> Just y; _ -> Nothing),
      Arm.nullary "PayLifeX" CostComponent.PayLifeX,
      Arm.payload "Sacrifice" (Sacrifice.codec keywordCodec) CostComponent.Sacrifice (\x -> case x of CostComponent.Sacrifice y -> Just y; _ -> Nothing),
      Arm.payload "TapForTotalPower" (TapForTotalPower.codec keywordCodec) CostComponent.TapForTotalPower (\x -> case x of CostComponent.TapForTotalPower y -> Just y; _ -> Nothing),
      Arm.payload "TapPermanents" (TapPermanents.codec keywordCodec) CostComponent.TapPermanents (\x -> case x of CostComponent.TapPermanents y -> Just y; _ -> Nothing),
      Arm.payload "DiscardCards" (DiscardCards.codec keywordCodec) CostComponent.DiscardCards (\x -> case x of CostComponent.DiscardCards y -> Just y; _ -> Nothing),
      -- The component carries a DiscardCause, but the WIRE does not: a card
      -- prints "Discard this card" and never says the discard is a cycle, which
      -- CR 702.29c makes true only of a cycling ability's cost -- and a cycling
      -- ability is minted from the keyword by Pawl.Engine.Keyword rather than
      -- authored. So this stays nullary over the Ordinary cause, which leaves
      -- ToPayCyclingCost unspellable by card data. Faerie Macabre is the card
      -- that writes this arm, and Pawl.ActivateSpec's "CR 702.29c an authored
      -- discard-this cost is not a cycle" is what proves the cause it decodes to
      -- -- a round trip cannot, the wire having one spelling for both causes.
      Arm.nullary "DiscardThis" (CostComponent.DiscardThis DiscardCause.Ordinary),
      Arm.payload "PayEnergy" Common.natural CostComponent.PayEnergy (\x -> case x of CostComponent.PayEnergy y -> Just y; _ -> Nothing),
      Arm.payload "AddLoyaltyToThis" Common.natural CostComponent.AddLoyaltyToThis (\x -> case x of CostComponent.AddLoyaltyToThis y -> Just y; _ -> Nothing),
      Arm.payload "RemoveLoyaltyFromThis" Common.natural CostComponent.RemoveLoyaltyFromThis (\x -> case x of CostComponent.RemoveLoyaltyFromThis y -> Just y; _ -> Nothing),
      Arm.payload "PutPlusOneCountersOnThis" Common.natural CostComponent.PutPlusOneCountersOnThis (\x -> case x of CostComponent.PutPlusOneCountersOnThis y -> Just y; _ -> Nothing),
      Arm.payload "Blight" Common.natural CostComponent.Blight (\x -> case x of CostComponent.Blight y -> Just y; _ -> Nothing),
      Arm.nullary "BlightX" CostComponent.BlightX,
      Arm.nullary "ExileThisFromGraveyard" CostComponent.ExileThisFromGraveyard,
      Arm.payload "ExileCardsFromGraveyard" (ExileCardsFromGraveyard.codec keywordCodec) CostComponent.ExileCardsFromGraveyard (\x -> case x of CostComponent.ExileCardsFromGraveyard y -> Just y; _ -> Nothing),
      Arm.payload "ExileTopFromGraveyard" (Filter.codec keywordCodec) CostComponent.ExileTopFromGraveyard (\x -> case x of CostComponent.ExileTopFromGraveyard y -> Just y; _ -> Nothing)
    ]
