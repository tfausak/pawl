module Pawl.Codec.BecameBlockingSpec where

import qualified Pawl.Codec.BecameBlocking as BecameBlocking
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BecameBlocking" $ do
  -- CR 509.1g. BOTH ids are an ObjectId, so the fixture names them differently
  -- on purpose: only an asymmetric case catches a codec that had the attacker
  -- blocking the blocker.
  Spec.it s "MkBecameBlocking, both keys" $
    Common.assertCodec
      s
      BecameBlocking.codec
      ( BecameBlocking.MkBecameBlocking
          { BecameBlocking.blocker = ObjectId.MkObjectId 1,
            BecameBlocking.attacker = ObjectId.MkObjectId 2,
            BecameBlocking.putOntoBattlefield = False
          }
      )
      " {\"blocker\":1,\"attacker\":2} "
  -- CR 509.4's producer, the one that writes the key.
  Spec.it s "MkBecameBlocking, put onto the battlefield blocking" $
    Common.assertCodec
      s
      BecameBlocking.codec
      ( BecameBlocking.MkBecameBlocking
          { BecameBlocking.blocker = ObjectId.MkObjectId 1,
            BecameBlocking.attacker = ObjectId.MkObjectId 2,
            BecameBlocking.putOntoBattlefield = True
          }
      )
      " {\"blocker\":1,\"attacker\":2,\"putOntoBattlefield\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameBlocking.codec
