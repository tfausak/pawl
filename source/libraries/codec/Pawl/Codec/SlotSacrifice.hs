{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SlotSacrifice where

import qualified Pawl.Codec.Sacrificer as Sacrificer
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.SlotSacrifice as SlotSacrifice

-- | The sacrificer is ELIDED when it is CR 701.21a's ordinary "sacrifice it",
-- which is every card in the pool; the other arm is written only by the emblem
-- Pawl.Engine.Ring mints, and that is not card data.
codec :: Codec.Codec SlotSacrifice.SlotSacrifice
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec SlotSacrifice.slot
  sacrificer <- Fields.defaulted "sacrificer" Sacrificer.EffectController Sacrificer.codec SlotSacrifice.sacrificer
  pure
    SlotSacrifice.MkSlotSacrifice
      { SlotSacrifice.slot = slot,
        SlotSacrifice.sacrificer = sacrificer
      }
