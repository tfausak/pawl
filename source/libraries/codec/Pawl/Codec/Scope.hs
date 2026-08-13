module Pawl.Codec.Scope where

import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Scope as Scope

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Scope.Scope
codec =
  Arm.tagged
    encode
    [ Arm.payload "InZone" (Common.tuple Zone.codec PlayerRef.codec) (uncurry Scope.InZone),
      Arm.payload "InHistory" EventShape.codec Scope.InHistory,
      Arm.payload "OverPlayers" PlayerRef.codec Scope.OverPlayers
    ]
  where
    encode s = case s of
      Scope.InZone z r ->
        Common.tagged "InZone" . Just . Value.array $
          [Codec.encode Zone.codec z, Codec.encode PlayerRef.codec r]
      Scope.InHistory e -> Common.tagged "InHistory" . Just $ Codec.encode EventShape.codec e
      Scope.OverPlayers r -> Common.tagged "OverPlayers" . Just $ Codec.encode PlayerRef.codec r
