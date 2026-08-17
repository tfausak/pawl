module Pawl.Codec.AfterTurnSpec where

import qualified Pawl.Codec.AfterTurn as AfterTurn
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AfterTurn" $ do
  -- CR 611.2a's baked seat and the turn the duration began on. Runtime-only:
  -- the one thing that serialises this is a DelayedTrigger (CR 603.7b).
  Spec.it s "MkAfterTurn, both keys" $
    Common.assertCodec
      s
      AfterTurn.codec
      ( AfterTurn.MkAfterTurn
          { AfterTurn.player = PlayerId.MkPlayerId 1,
            AfterTurn.turn = 7
          }
      )
      " {\"player\":1,\"turn\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AfterTurn.codec
