module Pawl.Codec.SelfCountersReachedSpec where

import qualified Pawl.Codec.SelfCountersReached as SelfCountersReached
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SelfCountersReached" $ do
  -- CR 714.2's chapter trigger: a lore counter count, which is what a Saga's
  -- third chapter watches for.
  Spec.it s "MkSelfCountersReached, both keys" $
    Common.assertCodec
      s
      SelfCountersReached.codec
      ( SelfCountersReached.MkSelfCountersReached
          { SelfCountersReached.kind = CounterKind.Lore,
            SelfCountersReached.amount = 3
          }
      )
      " {\"kind\":{\"type\":\"Lore\"},\"amount\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SelfCountersReached.codec
