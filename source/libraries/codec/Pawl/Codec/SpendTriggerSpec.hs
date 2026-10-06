module Pawl.Codec.SpendTriggerSpec where

import qualified Pawl.Codec.FaceSpec as FaceSpec
import qualified Pawl.Codec.SpendTrigger as SpendTrigger
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SpendTrigger as SpendTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpendTrigger" $ do
  Spec.it s "every field written out" $
    Common.assertCodec
      s
      SpendTrigger.codec
      SpendTrigger.MkSpendTrigger
        { SpendTrigger.casts = Filter.HasCardType CardType.Instant,
          SpendTrigger.ability = FaceSpec.minimalTriggeredAbility,
          SpendTrigger.source = ObjectId.MkObjectId 4,
          SpendTrigger.controller = PlayerId.MkPlayerId 0
        }
      ( " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Instant\"}},"
          <> "\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}},\"source\":4,\"controller\":0} "
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s SpendTrigger.codec
