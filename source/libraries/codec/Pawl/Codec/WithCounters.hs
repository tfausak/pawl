{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.WithCounters where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.WithCounters as WithCounters

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec WithCounters.WithCounters
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) WithCounters.kind
  amount <- Fields.required "amount" Quantity.codec WithCounters.amount
  pure WithCounters.MkWithCounters {WithCounters.kind = kind, WithCounters.amount = amount}
