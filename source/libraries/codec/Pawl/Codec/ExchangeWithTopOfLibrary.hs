{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeWithTopOfLibrary where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExchangeWithTopOfLibrary as ExchangeWithTopOfLibrary

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec ExchangeWithTopOfLibrary.ExchangeWithTopOfLibrary
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec ExchangeWithTopOfLibrary.ref
  player <- Fields.required "player" PlayerRef.codec ExchangeWithTopOfLibrary.player
  pure
    ExchangeWithTopOfLibrary.MkExchangeWithTopOfLibrary
      { ExchangeWithTopOfLibrary.ref = ref,
        ExchangeWithTopOfLibrary.player = player
      }
