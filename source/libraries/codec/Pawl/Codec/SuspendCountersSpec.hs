module Pawl.Codec.SuspendCountersSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.SuspendCounters as SuspendCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SuspendCounters as SuspendCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SuspendCounters" $ do
  -- CR 702.62a: Rift Bolt's "Suspend 1--{R}".
  Spec.it s "Literal" $
    Common.assertCodec
      s
      SuspendCounters.codec
      (SuspendCounters.Literal 1)
      " {\"type\":\"Literal\",\"value\":1} "

  -- CR 107.3d / CR 101.1: Benalish Commander's "Suspend X--{X}{W}{W}. X can't be
  -- 0", whose floor is the payload.
  Spec.it s "Variable" $
    Common.assertCodec
      s
      SuspendCounters.codec
      (SuspendCounters.Variable 1)
      " {\"type\":\"Variable\",\"value\":1} "

  Spec.it s "has a schema" $
    Common.assertHasSchema s SuspendCounters.codec

  -- Both payloads are Natural, so a negative number is a decode failure rather
  -- than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"Literal\",\"value\":-1} ") >>= Codec.decode SuspendCounters.codec))
      "expected a decode failure"
