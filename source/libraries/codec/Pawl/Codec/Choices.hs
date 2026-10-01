{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Choices where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.SlotName as SlotName

codec :: Codec.Codec Choices.Choices
codec = Fields.object fields

-- | The keys alone, which Pawl.Codec.Casting and Pawl.Codec.Activation write
-- into their own objects.
fields :: Fields.Fields Choices.Choices Choices.Choices
fields = do
  targets <- Fields.defaulted "targets" Nothing (Common.maybe (Common.list Reference.codec)) Choices.targets
  targetsBySlot <- Fields.defaulted "targetsBySlot" Map.empty (Common.textMap SlotName.unwrap (Right . SlotName.MkSlotName) (Common.seq Reference.codec)) Choices.targetsBySlot
  modes <- Fields.defaulted "modes" Nothing (Common.maybe (Common.seq ModeIndex.codec)) Choices.modes
  x <- Fields.defaulted "x" Nothing (Common.maybe Common.natural) Choices.x
  cost <- Fields.defaulted "cost" Nothing (Common.maybe ManaCost.codec) Choices.cost
  costOrder <- Fields.defaulted "costOrder" Nothing (Common.maybe (Common.list Common.natural)) Choices.costOrder
  manaSources <- Fields.defaulted "mana" Seq.empty (Common.seq (Common.maybe Reference.codec)) Choices.manaSources
  manaYields <- Fields.defaulted "yields" Seq.empty (Common.seq Mana.codec) Choices.manaYields
  pure
    Choices.MkChoices
      { Choices.targets = targets,
        Choices.targetsBySlot = targetsBySlot,
        Choices.modes = modes,
        Choices.x = x,
        Choices.cost = cost,
        Choices.costOrder = costOrder,
        Choices.manaSources = manaSources,
        Choices.manaYields = manaYields
      }
