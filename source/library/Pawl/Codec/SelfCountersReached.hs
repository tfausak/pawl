{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SelfCountersReached where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be.
codec :: Codec.Codec SelfCountersReached.SelfCountersReached
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) SelfCountersReached.kind
  amount <- Fields.required "amount" Common.natural SelfCountersReached.amount
  pure
    SelfCountersReached.MkSelfCountersReached
      { SelfCountersReached.kind = kind,
        SelfCountersReached.amount = amount
      }
