{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeBlocks where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExchangeBlocks as ExchangeBlocks

codec :: Codec.Codec ExchangeBlocks.ExchangeBlocks
codec = Fields.object $ do
  first <- Fields.required "first" SlotName.codec ExchangeBlocks.first
  second <- Fields.required "second" SlotName.codec ExchangeBlocks.second
  pure
    ExchangeBlocks.MkExchangeBlocks
      { ExchangeBlocks.first = first,
        ExchangeBlocks.second = second
      }
