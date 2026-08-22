{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RollDie where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RollDie as RollDie

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.AgainstSlot takes: both fields are required, since a die with no
-- stated size is not a die CR 706.1a can describe and a roll nothing reads is
-- an effect with no observable outcome.
codec :: Codec.Codec RollDie.RollDie
codec = Fields.object $ do
  sides <- Fields.required "sides" Common.natural RollDie.sides
  slot <- Fields.required "slot" SlotName.codec RollDie.slot
  pure RollDie.MkRollDie {RollDie.sides = sides, RollDie.slot = slot}
