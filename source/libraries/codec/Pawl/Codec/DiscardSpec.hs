module Pawl.Codec.DiscardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Discard as Discard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Discard" $ do
  -- CR 701.9b: the player the slot names discards this many, choosing which.
  Spec.it s "Counted" $
    Common.assertCodec
      s
      Discard.codec
      ( Discard.Counted
          CountedDiscard.MkCountedDiscard
            { CountedDiscard.slot = SlotName.MkSlotName (Text.pack "player"),
              CountedDiscard.quantity = Quantity.Literal 1,
              CountedDiscard.discarded = Nothing
            }
      )
      " {\"type\":\"Counted\",\"value\":{\"slot\":\"player\",\"quantity\":{\"type\":\"Literal\",\"value\":1}}} "
  -- The card names the set instead, so CR 701.9b's choice does not arise --
  -- Amnesia's "discards all nonland cards".
  Spec.it s "These" $
    Common.assertCodec
      s
      Discard.codec
      (Discard.These (ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand (GraveyardScope.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Just (Filter.Not (Filter.HasCardType CardType.Land))))))
      " {\"type\":\"These\",\"value\":{\"type\":\"EachCardInHand\",\"value\":{\"hands\":{\"type\":\"InSlot\",\"value\":\"target\"},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Discard.codec
