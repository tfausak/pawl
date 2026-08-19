module Pawl.Codec.BlocksDeclaredSpec where

import qualified Pawl.Codec.BlocksDeclared as BlocksDeclared
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlocksDeclared" $ do
  -- CR 509.1i read the other way: how many attackers one creature blocked.
  Spec.it s "MkBlocksDeclared, both keys" $
    Common.assertCodec
      s
      BlocksDeclared.codec
      (BlocksDeclared.MkBlocksDeclared {BlocksDeclared.blocker = ObjectId.MkObjectId 1, BlocksDeclared.count = 2})
      " {\"blocker\":1,\"count\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlocksDeclared.codec
