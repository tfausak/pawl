module Pawl.Codec.EntryAttack where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EntryAttack as EntryAttack

-- | CR 508.4's two readings: chosen as it enters, or specified by the effect.
codec :: Codec.Codec EntryAttack.EntryAttack
codec =
  Arm.tagged
    [ Arm.nullary "Chosen" EntryAttack.Chosen,
      Arm.payload "SameAs" SlotName.codec EntryAttack.SameAs (\x -> case x of EntryAttack.SameAs y -> Just y; _ -> Nothing)
    ]
