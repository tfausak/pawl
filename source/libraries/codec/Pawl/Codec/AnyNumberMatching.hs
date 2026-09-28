{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AnyNumberMatching where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching

-- | The ceiling is ELIDED when absent, so "any number" writes no key.
codec :: Codec.Codec AnyNumberMatching.AnyNumberMatching
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) AnyNumberMatching.filter
  atMost <- Fields.defaulted "atMost" Nothing (Common.maybe Quantity.codec) AnyNumberMatching.atMost
  pure
    AnyNumberMatching.MkAnyNumberMatching
      { AnyNumberMatching.filter = filter_,
        AnyNumberMatching.atMost = atMost
      }
