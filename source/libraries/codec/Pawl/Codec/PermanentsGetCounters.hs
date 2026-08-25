{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentsGetCounters where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentsGetCounters as PermanentsGetCounters

-- | A bare object keyed by the record's field names, Pawl.Codec.SelfCountersReached's
-- shape.
codec :: Codec.Codec PermanentsGetCounters.PermanentsGetCounters
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) PermanentsGetCounters.kind
  permanents <- Fields.required "permanents" (Filter.codec Keyword.codec) PermanentsGetCounters.permanents
  pure
    PermanentsGetCounters.MkPermanentsGetCounters
      { PermanentsGetCounters.kind = kind,
        PermanentsGetCounters.permanents = permanents
      }
