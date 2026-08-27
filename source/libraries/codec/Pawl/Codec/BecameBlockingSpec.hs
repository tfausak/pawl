module Pawl.Codec.BecameBlockingSpec where

import qualified Data.Set as Set
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
            BecameBlocking.putOntoBattlefield = False,
            BecameBlocking.attackerWasBlocked = False,
            BecameBlocking.blockersBefore = Set.empty
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
            BecameBlocking.putOntoBattlefield = True,
            BecameBlocking.attackerWasBlocked = False,
            BecameBlocking.blockersBefore = Set.empty
          }
      )
      " {\"blocker\":1,\"attacker\":2,\"putOntoBattlefield\":true} "
  -- CR 509.1h's flag and CR 509.3e's set, and the one producer that can carry
  -- either: an arrival at an attacker that was ALREADY blocked. The case above
  -- holds both clear while the other flag is set, which is what tells the three
  -- apart -- a codec that read any key into another field disagrees there. The
  -- set names an id that is neither of the other two, for the same reason the
  -- first case names two.
  Spec.it s "MkBecameBlocking, put onto the battlefield blocking an already-blocked attacker" $
    Common.assertCodec
      s
      BecameBlocking.codec
      ( BecameBlocking.MkBecameBlocking
          { BecameBlocking.blocker = ObjectId.MkObjectId 1,
            BecameBlocking.attacker = ObjectId.MkObjectId 2,
            BecameBlocking.putOntoBattlefield = True,
            BecameBlocking.attackerWasBlocked = True,
            BecameBlocking.blockersBefore = Set.singleton (ObjectId.MkObjectId 3)
          }
      )
      " {\"blocker\":1,\"attacker\":2,\"putOntoBattlefield\":true,\"attackerWasBlocked\":true,\"blockersBefore\":[3]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameBlocking.codec
