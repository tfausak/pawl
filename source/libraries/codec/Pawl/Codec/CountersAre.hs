{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CountersAre where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CountersAre as CountersAre

-- | The kind is spelled as a placement's counters spell it.
codec :: Codec.Codec CountersAre.CountersAre
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec CountersAre.object
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) CountersAre.kind
  count <- Fields.required "count" Common.natural CountersAre.count
  pure CountersAre.MkCountersAre {CountersAre.object = object, CountersAre.kind = kind, CountersAre.count = count}
