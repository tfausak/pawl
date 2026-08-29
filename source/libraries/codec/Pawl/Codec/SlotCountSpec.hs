module Pawl.Codec.SlotCountSpec where

import qualified Pawl.Codec.SlotCount as SlotCount
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SlotCount as SlotCount
import qualified Pawl.Types.TargetCount as TargetCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SlotCount" $ do
  Spec.it s "a printed range" $
    Common.assertCodec
      s
      SlotCount.codec
      (SlotCount.Printed (TargetCount.upTo 2))
      " {\"type\":\"Printed\",\"value\":{\"least\":0,\"most\":2}} "
  -- CR 601.2c's variable number of targets, Rot-Curse Rakshasa's "each of X
  -- target creatures".
  Spec.it s "the announced X" $
    Common.assertCodec
      s
      SlotCount.codec
      SlotCount.AnnouncedX
      " {\"type\":\"AnnouncedX\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s SlotCount.codec
