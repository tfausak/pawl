{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ControlSlots where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControlSlots as ControlSlots

codec :: Codec.Codec ControlSlots.ControlSlots
codec = Fields.object $ do
  first <- Fields.required "first" SlotName.codec ControlSlots.first
  second <- Fields.required "second" SlotName.codec ControlSlots.second
  pure
    ControlSlots.MkControlSlots
      { ControlSlots.first = first,
        ControlSlots.second = second
      }
