module Pawl.Codec.ChosenCardFromAmongSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChosenCardFromAmong" $ do
  -- Commune with the Gods' "a creature or enchantment card from among them": the
  -- slot names the group an earlier clause bound, and the Filter narrows what may
  -- be picked out of it.
  Spec.it s "MkChosenCardFromAmong, the two required keys" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = Quantity.Literal 1,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- Ancestral Memories' two and Animal Magnetism's opponent: the two keys the
  -- defaults above leave off the wire entirely.
  Spec.it s "MkChosenCardFromAmong, all four keys" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = Quantity.Literal 2,
            ChosenCardFromAmong.chooser = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "opponent"))
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"count\":{\"type\":\"Literal\",\"value\":2},\"chooser\":{\"type\":\"InSlot\",\"value\":\"opponent\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardFromAmong.codec
