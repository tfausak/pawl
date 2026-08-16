module Pawl.Codec.PlayerRef where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The wire format is the hand-written pair's, which 'Arm.tagged' emits
-- identically; what the conversion added is the schema. "Candidate",
-- "ControllerOfBound" and "EachPlayerExcept" are the tags written since.
codec :: Codec.Codec PlayerRef.PlayerRef
codec =
  Arm.tagged
    [ Arm.nullary "EachPlayer" PlayerRef.EachPlayer,
      -- The table minus one seat, which a card writes: Shahrazad's "each player
      -- who doesn't win the subgame".
      Arm.payload "EachPlayerExcept" SlotName.codec PlayerRef.EachPlayerExcept (\x -> case x of PlayerRef.EachPlayerExcept y -> Just y; _ -> Nothing),
      Arm.payload "Relative" PlayerRelation.codec PlayerRef.Relative (\x -> case x of PlayerRef.Relative y -> Just y; _ -> Nothing),
      Arm.payload "InSlot" SlotName.codec PlayerRef.InSlot (\x -> case x of PlayerRef.InSlot y -> Just y; _ -> Nothing),
      -- CR 611.2b's baked half. Decodable because an Expiry.While serialises its
      -- whole Condition (CR 603.7b's delayed ability), never because a card may
      -- write one -- Pawl.CardSpec sweeps the pool for that.
      Arm.payload "Specific" PlayerId.codec PlayerRef.Specific (\x -> case x of PlayerRef.Specific y -> Just y; _ -> Nothing),
      -- The fold's own candidate, which a card DOES write: Malignus names it
      -- inside an Aggregation.Greatest over Scope.OverPlayers.
      Arm.nullary "Candidate" PlayerRef.Candidate,
      -- CR 608.2h's reference, which a card writes: Spikeshell Harrier names the
      -- controller of the permanent its trigger targeted.
      Arm.payload "ControllerOfBound" SlotName.codec PlayerRef.ControllerOfBound (\x -> case x of PlayerRef.ControllerOfBound y -> Just y; _ -> Nothing)
    ]
