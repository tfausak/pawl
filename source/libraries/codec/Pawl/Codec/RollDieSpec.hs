module Pawl.Codec.RollDieSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RollDie as RollDie
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
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
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing
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
          RollDie.slot = SlotName.MkSlotName (Text.pack "result"),
          RollDie.other = Nothing
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
          RollDie.slot = SlotName.MkSlotName (Text.pack "chosen"),
          RollDie.other = Just (SlotName.MkSlotName (Text.pack "other"))
        }
      " {\"count\":{\"type\":\"Literal\",\"value\":2},\"other\":\"other\",\"sides\":6,\"slot\":\"chosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RollDie.codec
