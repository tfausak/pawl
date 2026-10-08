module Pawl.Codec.AfterObjectTurnSpec where

import qualified Pawl.Codec.AfterObjectTurn as AfterObjectTurn
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AfterObjectTurn as AfterObjectTurn
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AfterObjectTurn" $ do
  -- Runtime-only, Pawl.Codec.AfterTurnSpec's reason.
  Spec.it s "MkAfterObjectTurn, both keys" $
    Common.assertCodec
      s
      AfterObjectTurn.codec
      ( AfterObjectTurn.MkAfterObjectTurn
          { AfterObjectTurn.object = ObjectId.MkObjectId 5,
            AfterObjectTurn.turn = 7
          }
      )
      " {\"object\":5,\"turn\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AfterObjectTurn.codec
