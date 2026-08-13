module Pawl.Codec.PlayerRef where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The wire format is the hand-written pair's, which 'Arm.tagged' emits
-- identically; what the conversion added is the schema. "Candidate" is the one
-- tag written since.
codec :: Codec.Codec PlayerRef.PlayerRef
codec =
  Arm.tagged
    encode
    [ Arm.nullary "EachPlayer" PlayerRef.EachPlayer,
      Arm.payload "Relative" PlayerRelation.codec PlayerRef.Relative,
      Arm.payload "InSlot" SlotName.codec PlayerRef.InSlot,
      -- CR 611.2b's baked half. Decodable because an Expiry.While serialises its
      -- whole Condition (CR 603.7b's delayed ability), never because a card may
      -- write one -- Pawl.CardSpec sweeps the pool for that.
      Arm.payload "Specific" PlayerId.codec PlayerRef.Specific,
      -- The fold's own candidate, which a card DOES write: Malignus names it
      -- inside an Aggregation.Greatest over Scope.OverPlayers.
      Arm.nullary "Candidate" PlayerRef.Candidate
    ]
  where
    encode r = case r of
      PlayerRef.EachPlayer -> Common.nullary "EachPlayer"
      PlayerRef.Relative rel -> Common.tagged "Relative" . Just $ Codec.encode PlayerRelation.codec rel
      PlayerRef.InSlot n -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec n
      PlayerRef.Specific pid -> Common.tagged "Specific" . Just $ Codec.encode PlayerId.codec pid
      PlayerRef.Candidate -> Common.nullary "Candidate"
