module Pawl.Codec.EntryAttack where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EntryAttack as EntryAttack

-- | CR 508.4's readings: chosen as it enters, specified by the effect, or CR
-- 702.116a's choice narrowed to one player's subjects.
codec :: Codec.Codec EntryAttack.EntryAttack
codec =
  Arm.tagged
    [ Arm.nullary "Chosen" EntryAttack.Chosen,
      Arm.payload "SameAs" SlotName.codec EntryAttack.SameAs (\x -> case x of EntryAttack.SameAs y -> Just y; _ -> Nothing),
      Arm.payload "UnderPlayer" SlotName.codec EntryAttack.UnderPlayer (\x -> case x of EntryAttack.UnderPlayer y -> Just y; _ -> Nothing)
    ]
