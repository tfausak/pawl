module Pawl.Codec.VillainousChoiceRewriteSpec where

import qualified Pawl.Codec.VillainousChoiceRewrite as VillainousChoiceRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.VillainousChoiceRewrite as VillainousChoiceRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.VillainousChoiceRewrite" $ do
  -- CR 701.55c: The Valeyard.
  Spec.it s "AdditionalTime" $
    Common.assertCodec
      s
      VillainousChoiceRewrite.codec
      VillainousChoiceRewrite.AdditionalTime
      " {\"type\":\"AdditionalTime\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s VillainousChoiceRewrite.codec
