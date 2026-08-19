{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PutCounters where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PutCounters as PutCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's PutCounters arm.
codec :: Codec.Codec PutCounters.PutCounters
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) PutCounters.kind
  quantity <- Fields.required "quantity" Quantity.codec PutCounters.quantity
  ref <- Fields.required "ref" ObjectRef.codec PutCounters.ref
  pure
    PutCounters.MkPutCounters
      { PutCounters.kind = kind,
        PutCounters.quantity = quantity,
        PutCounters.ref = ref
      }
