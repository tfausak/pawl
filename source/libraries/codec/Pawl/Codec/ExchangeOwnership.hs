{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeOwnership where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExchangeOwnership as ExchangeOwnership

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec ExchangeOwnership.ExchangeOwnership
codec = Fields.object $ do
  one <- Fields.required "one" ObjectRef.codec ExchangeOwnership.one
  other <- Fields.required "other" ObjectRef.codec ExchangeOwnership.other
  pure
    ExchangeOwnership.MkExchangeOwnership
      { ExchangeOwnership.one = one,
        ExchangeOwnership.other = other
      }
