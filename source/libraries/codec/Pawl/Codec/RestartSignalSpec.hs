module Pawl.Codec.RestartSignalSpec where

import qualified Pawl.Codec.RestartSignal as RestartSignal
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RestartSignal as RestartSignal

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RestartSignal" $ do
  Spec.it s "Playing" $
    Common.assertCodec
      s
      RestartSignal.codec
      RestartSignal.Playing
      " {\"type\":\"Playing\"} "
  Spec.it s "Restarted" $
    Common.assertCodec
      s
      RestartSignal.codec
      RestartSignal.Restarted
      " {\"type\":\"Restarted\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RestartSignal.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RestartSignal.codec
