module Pawl.Codec.EndTurnSignalSpec where

import qualified Pawl.Codec.EndTurnSignal as EndTurnSignal
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EndTurnSignal" $ do
  Spec.it s "Running" $
    Common.assertCodec
      s
      EndTurnSignal.codec
      EndTurnSignal.Running
      " {\"type\":\"Running\"} "
  -- CR 724.1f: raised means no player gets priority for the rest of the process,
  -- so a state written out mid-process has to say which of the two it is in.
  Spec.it s "Ended" $
    Common.assertCodec
      s
      EndTurnSignal.codec
      EndTurnSignal.Ended
      " {\"type\":\"Ended\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s EndTurnSignal.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s EndTurnSignal.codec
