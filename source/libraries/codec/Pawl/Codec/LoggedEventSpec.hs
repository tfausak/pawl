module Pawl.Codec.LoggedEventSpec where

import qualified Pawl.Codec.LoggedEvent as LoggedEvent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StepBegan as StepBegan

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LoggedEvent" $ do
  -- The group is NOT the first one, so an encoder that wrote a constant could
  -- not pass: CR 608.2f's groups are what tell one "single event" from the next,
  -- and a log that lost them could not be replayed into the same trigger scan.
  Spec.it s "an event and the group it belongs to" $
    Common.assertCodec
      s
      LoggedEvent.codec
      LoggedEvent.MkLoggedEvent
        { LoggedEvent.group = EventGroup.next EventGroup.first,
          LoggedEvent.event = GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) (PlayerId.MkPlayerId 0))
        }
      " {\"group\":1,\"event\":{\"type\":\"StepBegan\",\"value\":{\"phase\":{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}},\"player\":0}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s LoggedEvent.codec
