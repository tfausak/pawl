module Pawl.Codec.TriggerSource where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TriggerSource as TriggerSource

-- | CR 113.7: an object id under `OfObject`, and a bare tag for the inherent
-- abilities rule 725.2 and rule 702.179d state without a card.
codec :: Codec.Codec TriggerSource.TriggerSource
codec =
  Arm.tagged
    [ Arm.payload "OfObject" ObjectId.codec TriggerSource.OfObject (\x -> case x of TriggerSource.OfObject y -> Just y; _ -> Nothing),
      Arm.nullary "Sourceless" TriggerSource.Sourceless
    ]
