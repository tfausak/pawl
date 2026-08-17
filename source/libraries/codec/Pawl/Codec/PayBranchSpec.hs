module Pawl.Codec.PayBranchSpec where

import qualified Pawl.Codec.PayBranch as PayBranch
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PayBranch as PayBranch

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PayBranch" $ do
  Spec.it s "IfPaid" $
    Common.assertCodec
      s
      PayBranch.codec
      PayBranch.IfPaid
      " {\"type\":\"IfPaid\"} "
  Spec.it s "IfNotPaid" $
    Common.assertCodec
      s
      PayBranch.codec
      PayBranch.IfNotPaid
      " {\"type\":\"IfNotPaid\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PayBranch.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PayBranch.codec
