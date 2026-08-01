module Pawl.Codec.EventShapeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.EventShape" . Spec.it s "MovedBetween" $
    -- CR 700.4's "dies": moved from the battlefield to a graveyard.
    Common.assertJsonCodec
      s
      EventShape.toJson
      EventShape.fromJson
      (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard)
      "{\"type\":\"MovedBetween\",\"value\":[{\"type\":\"Battlefield\"},{\"type\":\"Graveyard\"}]}"
