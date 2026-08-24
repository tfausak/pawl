module Pawl.Codec.Scope where

import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Scope as Scope

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Scope.Scope
codec =
  Arm.tagged
    [ Arm.payload "InZone" InZone.codec Scope.InZone (\x -> case x of Scope.InZone y -> Just y; _ -> Nothing),
      Arm.payload "InHistory" EventShape.codec Scope.InHistory (\x -> case x of Scope.InHistory y -> Just y; _ -> Nothing),
      Arm.payload "OverPlayers" PlayerRef.codec Scope.OverPlayers (\x -> case x of Scope.OverPlayers y -> Just y; _ -> Nothing),
      Arm.payload "OverBound" SlotName.codec Scope.OverBound (\x -> case x of Scope.OverBound y -> Just y; _ -> Nothing)
    ]
