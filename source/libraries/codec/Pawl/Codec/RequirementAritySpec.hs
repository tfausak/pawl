module Pawl.Codec.RequirementAritySpec where

import qualified Pawl.Codec.RequirementArity as RequirementArity
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RequirementArity as RequirementArity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RequirementArity" $ do
  Spec.it s "EachSubject" $
    Common.assertCodec
      s
      RequirementArity.codec
      RequirementArity.EachSubject
      " {\"type\":\"EachSubject\"} "
  Spec.it s "AnySubject" $
    Common.assertCodec
      s
      RequirementArity.codec
      RequirementArity.AnySubject
      " {\"type\":\"AnySubject\"} "
  -- Exhaustive where the literals above are representative, for
  -- Pawl.Codec.RequiredDefenderSpec's reason.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RequirementArity.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RequirementArity.codec
