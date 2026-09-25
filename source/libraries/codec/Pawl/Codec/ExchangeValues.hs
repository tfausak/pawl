{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeValues where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ExchangedValue as ExchangedValue
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Duration as Duration.Type
import qualified Pawl.Types.ExchangeValues as ExchangeValues

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's ExchangeValues arm.
codec :: Codec.Codec ExchangeValues.ExchangeValues
codec = Fields.object $ do
  one <- Fields.required "one" ExchangedValue.codec ExchangeValues.one
  other <- Fields.required "other" ExchangedValue.codec ExchangeValues.other
  -- CR 611.2a: a card stating no duration lasts until the end of the game.
  duration <- Fields.defaulted "duration" Duration.Type.Indefinite Duration.codec ExchangeValues.duration
  pure
    ExchangeValues.MkExchangeValues
      { ExchangeValues.one = one,
        ExchangeValues.other = other,
        ExchangeValues.duration = duration
      }
