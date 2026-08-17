module Pawl.Codec.BlockerDeclaredSpec where

import qualified Pawl.Codec.BlockerDeclared as BlockerDeclared
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BlockerDeclared as BlockerDeclared
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockerDeclared" $ do
  -- CR 509.1i. BOTH keys are an ObjectId, so the fixture names them differently
  -- on purpose: only an asymmetric case catches a codec that had the attacker
  -- blocking the blocker.
  Spec.it s "MkBlockerDeclared, both keys" $
    Common.assertCodec
      s
      BlockerDeclared.codec
      ( BlockerDeclared.MkBlockerDeclared
          { BlockerDeclared.blocker = ObjectId.MkObjectId 1,
            BlockerDeclared.attacker = ObjectId.MkObjectId 2
          }
      )
      " {\"blocker\":1,\"attacker\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockerDeclared.codec
