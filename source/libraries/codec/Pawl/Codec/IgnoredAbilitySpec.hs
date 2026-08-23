module Pawl.Codec.IgnoredAbilitySpec where

import qualified Pawl.Codec.IgnoredAbility as IgnoredAbility
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.IgnoredAbility" $ do
  -- CR 116.2d: who ignores, whose static abilities, and for how long. The seat
  -- and the permanent are different types, so only the expiry could be confused
  -- with anything -- and CR 514.2's AtCleanup is what every printed producer arms.
  Spec.it s "a player ignoring a permanent until end of turn" $
    Common.assertCodec
      s
      IgnoredAbility.codec
      IgnoredAbility.MkIgnoredAbility
        { IgnoredAbility.player = PlayerId.MkPlayerId 1,
          IgnoredAbility.source = ObjectId.MkObjectId 7,
          IgnoredAbility.expiry = Expiry.AtCleanup
        }
      " {\"player\":1,\"source\":7,\"expiry\":{\"type\":\"AtCleanup\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s IgnoredAbility.codec
