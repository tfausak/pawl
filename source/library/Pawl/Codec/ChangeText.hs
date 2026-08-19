{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChangeText where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.SubtypeFamily as SubtypeFamily
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChangeText as ChangeText

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's ChangeText arm.
codec :: Codec.Codec ChangeText.ChangeText
codec = Fields.object $ do
  family <- Fields.required "family" SubtypeFamily.codec ChangeText.family
  forbidden <- Fields.required "forbidden" (Common.set Subtype.codec) ChangeText.forbidden
  slot <- Fields.required "slot" SlotName.codec ChangeText.slot
  pure
    ChangeText.MkChangeText
      { ChangeText.family = family,
        ChangeText.forbidden = forbidden,
        ChangeText.slot = slot
      }
