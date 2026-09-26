module Pawl.Codec.EntryBlock where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EntryBlock as EntryBlock

-- | CR 509.4's readings: chosen as it enters, or specified by the effect.
codec :: Codec.Codec EntryBlock.EntryBlock
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Chosen" EntryBlock.Chosen,
      Arm.payload "Specified" SlotName.codec EntryBlock.Specified (\x -> case x of EntryBlock.Specified y -> Just y; _ -> Nothing)
    ]

tagOf :: EntryBlock.EntryBlock -> String
tagOf x = case x of
  EntryBlock.Chosen {} -> "Chosen"
  EntryBlock.Specified {} -> "Specified"
