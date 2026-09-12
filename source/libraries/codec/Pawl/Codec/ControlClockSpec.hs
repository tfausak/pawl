module Pawl.Codec.ControlClockSpec where

import qualified Pawl.Codec.ControlClock as ControlClock
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlClock as ControlClock

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControlClock" $ do
  Spec.it s "Gained" $
    Common.assertCodec
      s
      ControlClock.codec
      ControlClock.Gained
      " {\"type\":\"Gained\"} "
  Spec.it s "SinceLastUpkeep" $
    Common.assertCodec
      s
      ControlClock.codec
      ControlClock.SinceLastUpkeep
      " {\"type\":\"SinceLastUpkeep\"} "
  Spec.it s "Elapsed" $
    Common.assertCodec
      s
      ControlClock.codec
      ControlClock.Elapsed
      " {\"type\":\"Elapsed\"} "
  -- Pawl.Codec.DamageDirectionSpec's reason: Arm.enum derives the arm list from
  -- the type, so this is what would catch a constructor the derivation missed.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ControlClock.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlClock.codec
