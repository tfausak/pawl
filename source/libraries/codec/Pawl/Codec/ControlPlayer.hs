{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ControlPlayer where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControlPlayer as ControlPlayer

-- | An object keyed by the record's field names, with rule 723.7's restriction
-- elided when it is absent.
codec :: Codec.Codec ControlPlayer.ControlPlayer
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ControlPlayer.slot
  manaFromLandsOnly <- Fields.defaulted "manaFromLandsOnly" False Common.boolean ControlPlayer.manaFromLandsOnly
  pure
    ControlPlayer.MkControlPlayer
      { ControlPlayer.slot = slot,
        ControlPlayer.manaFromLandsOnly = manaFromLandsOnly
      }
