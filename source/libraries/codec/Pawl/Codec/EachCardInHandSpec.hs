module Pawl.Codec.EachCardInHandSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EachCardInHand as EachCardInHand
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EachCardInHand" $ do
  -- Amnesia's discard: the hands a target slot names, narrowed by the card's own
  -- word (CR 109.2a).
  Spec.it s "MkEachCardInHand, a slot-named hand and a filter" $
    Common.assertCodec
      s
      EachCardInHand.codec
      ( EachCardInHand.MkEachCardInHand
          { EachCardInHand.hands = GraveyardScope.InSlot (SlotName.MkSlotName (Text.pack "target")),
            EachCardInHand.filter = Just (Filter.Not (Filter.HasCardType CardType.Land))
          }
      )
      " {\"hands\":{\"type\":\"InSlot\",\"value\":\"target\"},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  -- Amnesia's reveal: the whole hand, which Pawl.Types.Filter has no
  -- tautological arm to say, so the absent key is the only spelling.
  Spec.it s "MkEachCardInHand, no filter, and the key is omitted" $
    Common.assertCodec
      s
      EachCardInHand.codec
      ( EachCardInHand.MkEachCardInHand
          { EachCardInHand.hands = GraveyardScope.Scoped PlayerScope.You,
            EachCardInHand.filter = Nothing
          }
      )
      " {\"hands\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EachCardInHand.codec
