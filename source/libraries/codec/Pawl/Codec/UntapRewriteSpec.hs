module Pawl.Codec.UntapRewriteSpec where

import qualified Pawl.Codec.UntapRewrite as UntapRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
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
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s UntapRewrite.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s UntapRewrite.codec
