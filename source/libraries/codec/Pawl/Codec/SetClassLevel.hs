{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SetClassLevel where

import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SetClassLevel as SetClassLevel

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's SetClassLevel arm.
codec :: Codec.Codec SetClassLevel.SetClassLevel
codec = Fields.object $ do
  level <- Fields.required "level" ClassLevel.codec SetClassLevel.level
  slot <- Fields.required "slot" SlotName.codec SetClassLevel.slot
  pure
    SetClassLevel.MkSetClassLevel
      { SetClassLevel.level = level,
        SetClassLevel.slot = slot
      }
