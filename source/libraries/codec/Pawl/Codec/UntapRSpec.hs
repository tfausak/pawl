module Pawl.Codec.UntapRSpec where

import qualified Pawl.Codec.UntapR as UntapR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.UntapR as UntapR
import qualified Pawl.Types.UntapRewrite as UntapRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.UntapR" $ do
  -- CR 122.1d: every untap.
  Spec.it s "MkUntapR (any untap)" $
    Common.assertCodec
      s
      UntapR.codec
      (UntapR.MkUntapR {UntapR.during = Nothing, UntapR.rewrite = UntapRewrite.RemoveStunCounter})
      " {\"rewrite\":{\"type\":\"RemoveStunCounter\"}} "
  -- CR 502.3, Bewitching Leechcraft's "during your untap step".
  Spec.it s "MkUntapR (during the untap step)" $
    Common.assertCodec
      s
      UntapR.codec
      (UntapR.MkUntapR {UntapR.during = Just (Phase.Beginning BeginningStep.Untap), UntapR.rewrite = UntapRewrite.RemoveCounterToUntap CounterKind.PlusOnePlusOne})
      " {\"during\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}},\"rewrite\":{\"type\":\"RemoveCounterToUntap\",\"value\":{\"type\":\"PlusOnePlusOne\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s UntapR.codec
