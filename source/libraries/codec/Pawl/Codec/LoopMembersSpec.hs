module Pawl.Codec.LoopMembersSpec where

import qualified Pawl.Codec.LoopMembers as LoopMembers
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LoopMembers as LoopMembers

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LoopMembers" $ do
  Spec.it s "Every" $
    Common.assertCodec
      s
      LoopMembers.codec
      LoopMembers.Every
      " {\"type\":\"Every\"} "
  Spec.it s "AnyNumber" $
    Common.assertCodec
      s
      LoopMembers.codec
      LoopMembers.AnyNumber
      " {\"type\":\"AnyNumber\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s LoopMembers.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s LoopMembers.codec
