module Pawl.Codec.WhichCountersSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WhichCounters as WhichCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.WhichCounters as WhichCounters

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (WhichCounters.WhichCounters Keyword.Keyword)
codec = WhichCounters.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.WhichCounters" $ do
  Spec.it s "OfKind" $
    Common.assertCodec
      s
      codec
      (WhichCounters.OfKind CounterKind.MinusOneMinusOne)
      " {\"type\":\"OfKind\",\"value\":{\"type\":\"MinusOneMinusOne\"}} "
  Spec.it s "OfAnyKind" $
    Common.assertCodec
      s
      codec
      WhichCounters.OfAnyKind
      " {\"type\":\"OfAnyKind\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
