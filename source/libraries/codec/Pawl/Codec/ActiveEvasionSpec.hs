module Pawl.Codec.ActiveEvasionSpec where

import qualified Pawl.Codec.ActiveEvasion as ActiveEvasion
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveEvasion as ActiveEvasion
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveEvasion" $ do
  -- CR 509.1b. Distinct numbers in every numeric field, so a swap cannot pass.
  Spec.it s "a class of attackers that can't be blocked" $
    Common.assertCodec
      s
      ActiveEvasion.codec
      ActiveEvasion.MkActiveEvasion
        { ActiveEvasion.source = ObjectId.MkObjectId 1,
          ActiveEvasion.controller = PlayerId.MkPlayerId 4,
          ActiveEvasion.timestamp = Timestamp.MkTimestamp 2,
          ActiveEvasion.expiry = Expiry.AtCleanup,
          ActiveEvasion.affected = Filter.HasCardType CardType.Creature
        }
      " {\"source\":1,\"controller\":4,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"affected\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveEvasion.codec
