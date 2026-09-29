module Pawl.Codec.UntapRewriteSpec where

import qualified Pawl.Codec.UntapRewrite as UntapRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.UntapRewrite as UntapRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.UntapRewrite" $ do
  -- CR 122.1d's replacement. Minted from a permanent's stun counters and never
  -- authored on a card, so this codec is the only place its wire form is pinned.
  Spec.it s "RemoveStunCounter" $
    Common.assertCodec
      s
      UntapRewrite.codec
      UntapRewrite.RemoveStunCounter
      " {\"type\":\"RemoveStunCounter\"} "
  -- Bewitching Leechcraft's "remove a +1/+1 counter from it instead. If you do,
  -- untap it."
  Spec.it s "RemoveCounterToUntap" $
    Common.assertCodec
      s
      UntapRewrite.codec
      (UntapRewrite.RemoveCounterToUntap CounterKind.PlusOnePlusOne)
      " {\"type\":\"RemoveCounterToUntap\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s UntapRewrite.codec
