{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SetHalfLocked where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's SetHalfLocked arm.
codec :: Codec.Codec SetHalfLocked.SetHalfLocked
codec = Fields.object $ do
  locked <- Fields.required "locked" Common.boolean SetHalfLocked.locked
  slot <- Fields.required "slot" SlotName.codec SetHalfLocked.slot
  pure
    SetHalfLocked.MkSetHalfLocked
      { SetHalfLocked.locked = locked,
        SetHalfLocked.slot = slot
      }
