module Pawl.Codec.AnyNumberDiscardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AnyNumberDiscard as AnyNumberDiscard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AnyNumberDiscard as AnyNumberDiscard
import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AnyNumberDiscard" $ do
  Spec.it s "MkAnyNumberDiscard, no ceiling and no bound slot" $
    Common.assertCodec
      s
      AnyNumberDiscard.codec
      ( AnyNumberDiscard.MkAnyNumberDiscard
          { AnyNumberDiscard.player = PlayerRef.Relative PlayerRelation.You,
            AnyNumberDiscard.cards = AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Land) Nothing,
            AnyNumberDiscard.discarded = Nothing
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"cards\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  Spec.it s "MkAnyNumberDiscard, with a ceiling and the discarded slot" $
    Common.assertCodec
      s
      AnyNumberDiscard.codec
      ( AnyNumberDiscard.MkAnyNumberDiscard
          { AnyNumberDiscard.player = PlayerRef.Relative PlayerRelation.You,
            AnyNumberDiscard.cards = AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Land) (Just (Quantity.Literal 2)),
            AnyNumberDiscard.discarded = Just (SlotName.MkSlotName (Text.pack "discarded"))
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"cards\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"atMost\":{\"type\":\"Literal\",\"value\":2}},\"discarded\":\"discarded\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AnyNumberDiscard.codec
