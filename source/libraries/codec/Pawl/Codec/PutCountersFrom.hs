{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PutCountersFrom where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's PutCountersFrom arm.
codec :: Codec.Codec PutCountersFrom.PutCountersFrom
codec = Fields.object $ do
  from <- Fields.required "from" SlotName.codec PutCountersFrom.from
  ref <- Fields.required "ref" ObjectRef.codec PutCountersFrom.ref
  pure
    PutCountersFrom.MkPutCountersFrom
      { PutCountersFrom.from = from,
        PutCountersFrom.ref = ref
      }
