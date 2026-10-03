module Pawl.Codec.ForbidBeingBlockedSpec where

import qualified Pawl.Codec.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ForbidBeingBlocked as ForbidBeingBlocked

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidBeingBlocked" $ do
  -- CR 509.1b. Two keys, as Pawl.Codec.ForbidUntap's are.
  Spec.it s "MkForbidBeingBlocked, both keys" $
    Common.assertCodec
      s
      ForbidBeingBlocked.codec
      ( ForbidBeingBlocked.MkForbidBeingBlocked
          { ForbidBeingBlocked.duration = Duration.UntilEndOfTurn,
            ForbidBeingBlocked.affected = Filter.HasCardType CardType.Creature
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"affected\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidBeingBlocked.codec
