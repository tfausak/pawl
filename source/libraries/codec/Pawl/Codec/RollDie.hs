{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RollDie where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RollDie as RollDie

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.AgainstSlot takes: `sides` and `slot` are required, since a die
-- with no stated size is not a die CR 706.1a can describe and a roll nothing
-- reads is an effect with no observable outcome.
--
-- CR 706.2's modifier is ELIDED when absent, as Draw's slot is: most rolls
-- print none, and a card that does write one (Diviner's Portent) carries the
-- Quantity the instruction adds.
codec :: Codec.Codec RollDie.RollDie
codec = Fields.object $ do
  sides <- Fields.required "sides" Common.natural RollDie.sides
  modifier <- Fields.defaulted "modifier" Nothing (Common.maybe Quantity.codec) RollDie.modifier
  slot <- Fields.required "slot" SlotName.codec RollDie.slot
  pure RollDie.MkRollDie {RollDie.sides = sides, RollDie.modifier = modifier, RollDie.slot = slot}
