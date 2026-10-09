module Pawl.Codec.ReturnEndingSpec where

import qualified Pawl.Codec.ReturnEnding as ReturnEnding
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ReturnEnding as ReturnEnding

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReturnEnding" $ do
  Spec.it s "SourceLeaves" $
    Common.assertCodec
      s
      ReturnEnding.codec
      (ReturnEnding.SourceLeaves (ObjectId.MkObjectId 3))
      " {\"type\":\"SourceLeaves\",\"value\":3} "
  Spec.it s "OpponentCrowned" $
    Common.assertCodec
      s
      ReturnEnding.codec
      (ReturnEnding.OpponentCrowned (MonarchWatch.MkMonarchWatch {MonarchWatch.controller = PlayerId.MkPlayerId 2, MonarchWatch.due = Just (EventGroup.MkEventGroup 7)}))
      " {\"type\":\"OpponentCrowned\",\"value\":{\"controller\":2,\"due\":7}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ReturnEnding.codec
