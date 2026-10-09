module Pawl.Codec.RollDieSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RollDie as RollDie
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DiceReading as DiceReading
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RollDie" $ do
  -- CR 706.1a's N, and the slot CR 706.4's later text reads the result from.
  -- The instruction prints no modifier here, so the field is elided.
  Spec.it s "MkRollDie" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 20,
          RollDie.count = Quantity.Literal 1,
          RollDie.modifier = Nothing,
          RollDie.reading = DiceReading.ChooseOne,
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing,
          RollDie.roller = PlayerScope.Related PlayerRelation.You,
          RollDie.highest = Nothing,
          RollDie.store = Nothing
        }
      " {\"sides\":20,\"slot\":\"result\"} "
  -- CR 706.2's first sentence: the instruction's own modifier, which an
  -- always-absent optional field would round-trip vacuously.
  Spec.it s "MkRollDie with a modifier" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 20,
          RollDie.count = Quantity.Literal 1,
          RollDie.modifier = Just (Quantity.Literal 3),
          RollDie.reading = DiceReading.ChooseOne,
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing,
          RollDie.roller = PlayerScope.Related PlayerRelation.You,
          RollDie.highest = Nothing,
          RollDie.store = Nothing
        }
      " {\"modifier\":{\"type\":\"Literal\",\"value\":3},\"sides\":20,\"slot\":\"result\"} "
  -- CR 706.1's count beside CR 706.4's second reading, the Endeavor cycle's wire
  -- form: neither field elides here, and an always-defaulted field would
  -- round-trip vacuously.
  Spec.it s "two dice read for both results" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 6,
          RollDie.count = Quantity.Literal 2,
          RollDie.modifier = Nothing,
          RollDie.reading = DiceReading.ChooseOne,
          RollDie.slot = SlotName.MkSlotName (Text.pack "chosen"),
          RollDie.other = Just (SlotName.MkSlotName (Text.pack "other")),
          RollDie.roller = PlayerScope.Related PlayerRelation.You,
          RollDie.highest = Nothing,
          RollDie.store = Nothing
        }
      " {\"count\":{\"type\":\"Literal\",\"value\":2},\"other\":\"other\",\"sides\":6,\"slot\":\"chosen\"} "
  -- CR 706.4's total, Neverwinter Hydra's wire form: the one reading that is
  -- not the elided choice, so it is what a dropped field would lose.
  Spec.it s "X dice read as their total" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 6,
          RollDie.count = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")),
          RollDie.modifier = Nothing,
          RollDie.reading = DiceReading.Total,
          RollDie.slot = SlotName.MkSlotName (Text.pack "total"),
          RollDie.other = Nothing,
          RollDie.roller = PlayerScope.Related PlayerRelation.You,
          RollDie.highest = Nothing,
          RollDie.store = Nothing
        }
      " {\"count\":{\"type\":\"InSlot\",\"value\":\"X\"},\"reading\":{\"type\":\"Total\"},\"sides\":6,\"slot\":\"total\"} "
  -- CR 706.1's roller and the players who rolled highest, Chaos Dragon's wire
  -- form: both fields elide everywhere else.
  Spec.it s "each player rolls, the highest bound" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 20,
          RollDie.count = Quantity.Literal 1,
          RollDie.modifier = Nothing,
          RollDie.reading = DiceReading.ChooseOne,
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing,
          RollDie.roller = PlayerScope.Related PlayerRelation.AnyPlayer,
          RollDie.highest = Just (SlotName.MkSlotName (Text.pack "highest")),
          RollDie.store = Nothing
        }
      " {\"highest\":\"highest\",\"roller\":{\"type\":\"AnyPlayer\"},\"sides\":20,\"slot\":\"result\"} "
  -- CR 706.8a's store, Centaur of Attention's wire form: the field elides
  -- everywhere else.
  Spec.it s "five dice stored on the permanent" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 6,
          RollDie.count = Quantity.Literal 5,
          RollDie.modifier = Nothing,
          RollDie.reading = DiceReading.ChooseOne,
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing,
          RollDie.roller = PlayerScope.Related PlayerRelation.You,
          RollDie.highest = Nothing,
          RollDie.store = Just (SlotName.MkSlotName (Text.pack "self"))
        }
      " {\"count\":{\"type\":\"Literal\",\"value\":5},\"sides\":6,\"slot\":\"result\",\"store\":\"self\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RollDie.codec
