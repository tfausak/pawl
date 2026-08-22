module Pawl.Codec.RollDieSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RollDie as RollDie
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RollDie" $ do
  -- CR 706.1a's N, and the slot CR 706.4's later text reads the result from.
  Spec.it s "MkRollDie" $
    Common.assertCodec
      s
      RollDie.codec
      RollDie.MkRollDie
        { RollDie.sides = 20,
          RollDie.slot = SlotName.MkSlotName (Text.pack "result")
        }
      " {\"sides\":20,\"slot\":\"result\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RollDie.codec
