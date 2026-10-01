module Pawl.Codec.SelfCountersRemovedSpec where

import qualified Pawl.Codec.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SelfCountersRemoved" $ do
  Spec.it s "MkSelfCountersRemoved, the battlefield left off the wire" $
    Common.assertCodec
      s
      SelfCountersRemoved.codec
      (SelfCountersRemoved.MkSelfCountersRemoved {SelfCountersRemoved.kind = CounterKind.Loyalty, SelfCountersRemoved.zone = Zone.Battlefield})
      " {\"kind\":{\"type\":\"Loyalty\"}} "
  Spec.it s "MkSelfCountersRemoved, a stated zone" $
    Common.assertCodec
      s
      SelfCountersRemoved.codec
      (SelfCountersRemoved.MkSelfCountersRemoved {SelfCountersRemoved.kind = CounterKind.Time, SelfCountersRemoved.zone = Zone.Exile})
      " {\"kind\":{\"type\":\"Time\"},\"zone\":{\"type\":\"Exile\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SelfCountersRemoved.codec
