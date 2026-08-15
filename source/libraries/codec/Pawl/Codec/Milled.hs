{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Milled where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Milled as Milled

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Milled.Milled
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Milled.player
  cards <- Fields.required "cards" (Common.seq ObjectId.codec) Milled.cards
  pure
    Milled.MkMilled
      { Milled.player = player,
        Milled.cards = cards
      }
