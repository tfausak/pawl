{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Binding where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.SlotName as SlotName

-- | Runtime-only, never in card JSON: the codec must stay total over the
-- transitive closure of what the game state carries.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Binding.Binding
codec = Fields.object $ do
  targets <- Fields.defaulted "targets" Nothing (Common.maybe (Common.set Recipient.codec)) Binding.targets
  amount <- Fields.defaulted "amount" Nothing (Common.maybe Common.natural) Binding.amount
  modes <- Fields.defaulted "modes" Nothing (Common.maybe (Common.seq ModeIndex.codec)) Binding.modes
  copy <- Fields.defaulted "copy" Nothing (Common.maybe ProjectedCharacteristics.codec) Binding.copy
  objects <- Fields.defaulted "objects" Nothing (Common.maybe (Common.seq ObjectId.codec)) Binding.objects
  pure
    Binding.MkBinding
      { Binding.targets = targets,
        Binding.amount = amount,
        Binding.modes = modes,
        Binding.copy = copy,
        Binding.objects = objects
      }

-- | A name-keyed map as a JSON OBJECT keyed by the slot name.
codecMap :: Codec.Codec (Map.Map SlotName.SlotName Binding.Binding)
codecMap = Common.textMap SlotName.unwrap SlotName.MkSlotName codec
