module Pawl.Codec.MoveCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MoveCounters as MoveCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveCounters" $ do
  -- CR 122.5's pair: the object a counter leaves and the object it lands on.
  Spec.it s "MkMoveCounters, both keys" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = SlotName.MkSlotName (Text.pack "self"),
            MoveCounters.to = SlotName.MkSlotName (Text.pack "became")
          }
      )
      " {\"from\":\"self\",\"to\":\"became\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveCounters.codec
