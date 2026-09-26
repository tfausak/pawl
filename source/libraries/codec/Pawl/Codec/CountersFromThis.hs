{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CountersFromThis where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CountersFromThis as CountersFromThis

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CountersFromThis.CountersFromThis keyword)
codec keywordCodec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec keywordCodec) CountersFromThis.kind
  count <- Fields.required "count" Common.natural CountersFromThis.count
  pure CountersFromThis.MkCountersFromThis {CountersFromThis.kind = kind, CountersFromThis.count = count}
