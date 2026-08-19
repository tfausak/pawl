{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Fight where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Fight as Fight

-- Both keys required: CR 701.14a's fight has no one-sided form, so a card that
-- named only one slot would be a fight with nothing.
codec :: Codec.Codec Fight.Fight
codec = Fields.object $ do
  first <- Fields.required "first" SlotName.codec Fight.first
  second <- Fields.required "second" SlotName.codec Fight.second
  pure
    Fight.MkFight
      { Fight.first = first,
        Fight.second = second
      }
