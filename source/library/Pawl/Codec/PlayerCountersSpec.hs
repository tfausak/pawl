module Pawl.Codec.PlayerCountersSpec where

import qualified Pawl.Codec.PlayerCounters as PlayerCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerCounters" $ do
  -- Shared by GainPlayerCounters and RemovePlayerCounters, which differ only in
  -- their tag. All three keys are required: this payload has no optional part,
  -- which is what makes it shareable at all.
  Spec.it s "MkPlayerCounters, all three keys" $
    Common.assertCodec
      s
      PlayerCounters.codec
      ( PlayerCounters.MkPlayerCounters
          { PlayerCounters.player = PlayerRef.Relative PlayerRelation.You,
            PlayerCounters.kind = PlayerCounterKind.Rad,
            PlayerCounters.quantity = Quantity.Literal 2
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"kind\":{\"type\":\"Rad\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerCounters.codec
