{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AsCopy where

import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AsCopy as AsCopy

-- | A bare object keyed by the record's field names, Pawl.Codec.WithCounters'
-- shape. @exceptions@ is defaulted rather than required: CR 707.9's "except ..."
-- clause is absent from most printings, so a plain Clone writes the eligible
-- filter alone.
codec :: Codec.Codec AsCopy.AsCopy
codec = Fields.object $ do
  eligible <- Fields.required "eligible" (Filter.codec Keyword.codec) AsCopy.eligible
  exceptions <- Fields.defaulted "exceptions" [] (Common.list CopyException.codec) AsCopy.exceptions
  pure AsCopy.MkAsCopy {AsCopy.eligible = eligible, AsCopy.exceptions = exceptions}
