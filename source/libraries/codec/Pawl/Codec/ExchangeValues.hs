{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeValues where

import qualified Pawl.Codec.ExchangedValue as ExchangedValue
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExchangeValues as ExchangeValues

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's ExchangeValues arm.
codec :: Codec.Codec ExchangeValues.ExchangeValues
codec = Fields.object $ do
  one <- Fields.required "one" ExchangedValue.codec ExchangeValues.one
  other <- Fields.required "other" ExchangedValue.codec ExchangeValues.other
  pure
    ExchangeValues.MkExchangeValues
      { ExchangeValues.one = one,
        ExchangeValues.other = other
      }
