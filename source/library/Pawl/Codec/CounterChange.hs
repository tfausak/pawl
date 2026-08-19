{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterChange where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterChange as CounterChange

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec CounterChange.CounterChange
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec CounterChange.object
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) CounterChange.kind
  before <- Fields.required "before" Common.natural CounterChange.before
  after <- Fields.required "after" Common.natural CounterChange.after
  pure
    CounterChange.MkCounterChange
      { CounterChange.object = object,
        CounterChange.kind = kind,
        CounterChange.before = before,
        CounterChange.after = after
      }
