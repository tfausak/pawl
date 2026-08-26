{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterPlacement where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterPlacement as CounterPlacement

-- | A bare object keyed by the record's field names, Pawl.Codec.SelfCountersReached's
-- shape.
codec :: Codec.Codec CounterPlacement.CounterPlacement
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) CounterPlacement.kind
  permanents <- Fields.required "permanents" (Filter.codec Keyword.codec) CounterPlacement.permanents
  pure
    CounterPlacement.MkCounterPlacement
      { CounterPlacement.kind = kind,
        CounterPlacement.permanents = permanents
      }
