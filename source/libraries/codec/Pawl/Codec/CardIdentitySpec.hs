module Pawl.Codec.CardIdentitySpec where

import qualified Pawl.Codec.CardIdentity as CardIdentity
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardIdentity as CardIdentity
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardIdentity" $ do
  Spec.it s "MkCardIdentity, every key" $
    Common.assertCodec
      s
      CardIdentity.codec
      ( CardIdentity.MkCardIdentity
          { CardIdentity.serial = 4,
            CardIdentity.startingOwner = PlayerId.MkPlayerId 2
          }
      )
      " {\"serial\":4,\"startingOwner\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardIdentity.codec
