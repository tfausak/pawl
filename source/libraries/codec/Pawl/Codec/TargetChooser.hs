module Pawl.Codec.TargetChooser where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TargetChooser as TargetChooser

-- | Both arms are spelled as Pawl.Codec.PlayerRef spells its own two of the same
-- name, so a card writes one wire shape for "a player named by relation or by
-- slot" wherever it appears.
codec :: Codec.Codec TargetChooser.TargetChooser
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Relative" PlayerRelation.codec TargetChooser.Relative (\x -> case x of TargetChooser.Relative y -> Just y; _ -> Nothing),
      Arm.payload "InSlot" SlotName.codec TargetChooser.InSlot (\x -> case x of TargetChooser.InSlot y -> Just y; _ -> Nothing)
    ]

tagOf :: TargetChooser.TargetChooser -> String
tagOf x = case x of
  TargetChooser.Relative {} -> "Relative"
  TargetChooser.InSlot {} -> "InSlot"
