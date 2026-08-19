{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForEach where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForEach as ForEach

-- | A bare object keyed by the record's field names.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.ForEach gives: the record is parametric in the effect so that
-- neither module has to name the other.
--
-- Every field is REQUIRED, unlike Pawl.Codec.PreventNextDamage's rider: a loop
-- with no body and a loop with no name for its member are both an author's
-- mistake rather than a shorter way of saying something.
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (ForEach.ForEach effect)
codec effectCodec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec ForEach.ref
  slot <- Fields.required "slot" SlotName.codec ForEach.slot
  body <- Fields.required "body" (Common.seq effectCodec) ForEach.body
  pure
    ForEach.MkForEach
      { ForEach.ref = ref,
        ForEach.slot = slot,
        ForEach.body = body
      }
